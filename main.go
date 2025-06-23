//go:build windows
// +build windows

package main

import (
	"context"
	"flag"
	"fmt"
	"image"
	"image/color"
	"log"
	"net"
	"syscall"
	"time"
	"unsafe"

	vnc "github.com/unistack-org/go-rfb"
)

// Configuration struct for bandwidth optimization
type Config struct {
	Quality          int           // 1-10 (1=lowest, 10=highest)
	UpdateRate       time.Duration // Frame update interval
	ColorDepth       int           // 8, 16, 24, 32 bits
	SkipUnchanged    bool          // Skip unchanged regions
	MaxResolution    int           // Max width/height
	CompressionLevel int           // 1-9 for encodings that support it
	Port             int           // VNC server port
	EnableLogging    bool          // Detailed logging
	AdvancedOpt      bool          // Enable advanced optimizations
}

// Windows API constants
const (
	SRCCOPY = 0x00CC0020
)

var (
	user32                 = syscall.NewLazyDLL("user32.dll")
	gdi32                  = syscall.NewLazyDLL("gdi32.dll")
	getDesktopWindow       = user32.NewProc("GetDesktopWindow")
	getDC                  = user32.NewProc("GetDC")
	releaseDC              = user32.NewProc("ReleaseDC")
	getSystemMetrics       = user32.NewProc("GetSystemMetrics")
	createCompatibleDC     = gdi32.NewProc("CreateCompatibleDC")
	createCompatibleBitmap = gdi32.NewProc("CreateCompatibleBitmap")
	selectObject           = gdi32.NewProc("SelectObject")
	bitBlt                 = gdi32.NewProc("BitBlt")
	getDIBits              = gdi32.NewProc("GetDIBits")
	deleteDC               = gdi32.NewProc("DeleteDC")
	deleteObject           = gdi32.NewProc("DeleteObject")
)

type BITMAPINFOHEADER struct {
	Size          uint32
	Width         int32
	Height        int32
	Planes        uint16
	BitCount      uint16
	Compression   uint32
	SizeImage     uint32
	XPelsPerMeter int32
	YPelsPerMeter int32
	ClrUsed       uint32
	ClrImportant  uint32
}

// Global variables for change detection
var (
	lastImage         *image.RGBA
	config            Config
	lastCursorPos     image.Point
	motionThreshold   = uint32(2000) // Higher threshold for motion areas
	staticThreshold   = uint32(500)  // Lower threshold for static areas
	framesSinceMotion = 0
)

// Optimization statistics
var (
	totalFrames       int64
	totalRegions      int64
	totalPixelsSent   int64
	totalPixelsScreen int64
	lastStatsTime     time.Time
)

func getConfigFromFlags() Config {
	quality := flag.Int("quality", 7, "Image quality 1-10 (1=lowest bandwidth, 10=highest quality)")
	fps := flag.Int("fps", 15, "Updates per second (lower = less bandwidth)")
	depth := flag.Int("depth", 32, "Color depth: 8,16,24,32 bits (lower = less bandwidth)")
	skipUnchanged := flag.Bool("skip-unchanged", true, "Skip unchanged regions (saves bandwidth)")
	maxRes := flag.Int("max-res", 1920, "Maximum resolution (lower = less bandwidth)")
	compression := flag.Int("compression", 6, "Compression level 1-9 (higher = more compression)")
	port := flag.Int("port", 6900, "VNC server port")
	verbose := flag.Bool("verbose", true, "Enable detailed logging")
	compatMode := flag.Bool("compat", false, "Enable compatibility mode for basic VNC clients")
	advancedOpt := flag.Bool("advanced-opt", true, "Enable advanced optimizations (motion detection, adaptive thresholds)")

	flag.Parse()

	updateInterval := time.Duration(1000 / *fps) * time.Millisecond

	// If compatibility mode is enabled, use more conservative settings
	if *compatMode {
		log.Printf("🔧 Compatibility mode enabled - using conservative settings")
		if *depth > 16 {
			*depth = 16 // Use 16-bit color for better compatibility
		}
		if *quality > 6 {
			*quality = 6 // Use lower quality for better compatibility
		}
		*advancedOpt = false // Disable advanced optimizations for compatibility
	}

	// Adjust optimization thresholds based on quality
	if *advancedOpt {
		if *quality >= 8 {
			staticThreshold = 300 // Very sensitive for high quality
			motionThreshold = 1500
		} else if *quality >= 5 {
			staticThreshold = 500 // Default sensitivity
			motionThreshold = 2000
		} else {
			staticThreshold = 1000 // Less sensitive for low quality
			motionThreshold = 3000
		}
	}

	return Config{
		Quality:          *quality,
		UpdateRate:       updateInterval,
		ColorDepth:       *depth,
		SkipUnchanged:    *skipUnchanged,
		MaxResolution:    *maxRes,
		CompressionLevel: *compression,
		Port:             *port,
		EnableLogging:    *verbose,
		AdvancedOpt:      *advancedOpt,
	}
}

func getPixelFormat(colorDepth int) *vnc.PixelFormat {
	// For better compatibility, always use the standard 32-bit pixel format
	// regardless of the color depth setting. The color depth will be handled
	// in the screen capture and conversion process.
	pf := vnc.PixelFormat32bit
	log.Printf("🎨 Using pixel format: %+v", pf)
	return pf
}

func getEncodings(quality int) []vnc.Encoding {
	// Use only RawEncoding as other encodings may not be available in this VNC library
	// Quality affects update frequency and color depth instead
	return []vnc.Encoding{&vnc.RawEncoding{}}
}

func captureScreen(cfg Config) (*image.RGBA, error) {
	// Get screen dimensions
	widthPtr, _, _ := getSystemMetrics.Call(0)  // SM_CXSCREEN
	heightPtr, _, _ := getSystemMetrics.Call(1) // SM_CYSCREEN
	width := int(widthPtr)
	height := int(heightPtr)

	// Apply resolution limit
	if width > cfg.MaxResolution {
		width = cfg.MaxResolution
	}
	if height > cfg.MaxResolution {
		height = cfg.MaxResolution
	}

	// Get desktop window and DC
	hDesktopWnd, _, _ := getDesktopWindow.Call()
	hDesktopDC, _, _ := getDC.Call(hDesktopWnd)
	hCaptureDC, _, _ := createCompatibleDC.Call(hDesktopDC)
	hCaptureBitmap, _, _ := createCompatibleBitmap.Call(hDesktopDC, uintptr(width), uintptr(height))

	// Select the bitmap into the compatible DC
	selectObject.Call(hCaptureDC, hCaptureBitmap)

	// Copy the desktop to our bitmap
	bitBlt.Call(hCaptureDC, 0, 0, uintptr(width), uintptr(height),
		hDesktopDC, 0, 0, SRCCOPY)

	// Prepare bitmap info with configurable bit depth
	bitCount := uint16(cfg.ColorDepth)
	if bitCount == 24 {
		bitCount = 32 // Windows prefers 32-bit alignment
	}

	bi := BITMAPINFOHEADER{
		Size:     uint32(unsafe.Sizeof(BITMAPINFOHEADER{})),
		Width:    int32(width),
		Height:   -int32(height), // negative for top-down bitmap
		Planes:   1,
		BitCount: bitCount,
	}

	// Allocate buffer for pixel data
	bytesPerPixel := int(bitCount) / 8
	bufferSize := width * height * bytesPerPixel
	buffer := make([]byte, bufferSize)

	// Get the bits
	getDIBits.Call(hCaptureDC, hCaptureBitmap, 0, uintptr(height),
		uintptr(unsafe.Pointer(&buffer[0])),
		uintptr(unsafe.Pointer(&bi)), 0)

	// Create RGBA image
	img := image.NewRGBA(image.Rect(0, 0, width, height))

	// Convert to RGBA based on color depth
	convertToRGBA(buffer, img, width, height, cfg.ColorDepth, cfg.Quality)

	// Cleanup
	deleteObject.Call(hCaptureBitmap)
	deleteDC.Call(hCaptureDC)
	releaseDC.Call(hDesktopWnd, hDesktopDC)

	return img, nil
}

func convertToRGBA(buffer []byte, img *image.RGBA, width, height, colorDepth, quality int) {
	switch colorDepth {
	case 8:
		// 8-bit color (3-3-2 RGB)
		for i := 0; i < len(buffer); i++ {
			pixel := buffer[i]
			r := (pixel & 0xE0) >> 5 // 3 bits
			g := (pixel & 0x1C) >> 2 // 3 bits
			b := pixel & 0x03        // 2 bits

			y := i / width
			x := i % width

			img.Set(x, y, color.RGBA{
				r << 5, // Scale to 8-bit
				g << 5, // Scale to 8-bit
				b << 6, // Scale to 8-bit
				255,
			})
		}
	case 16:
		// 16-bit color (5-6-5 RGB)
		for i := 0; i < len(buffer); i += 2 {
			pixel := uint16(buffer[i]) | (uint16(buffer[i+1]) << 8)
			r := (pixel & 0xF800) >> 11 // 5 bits
			g := (pixel & 0x07E0) >> 5  // 6 bits
			b := pixel & 0x001F         // 5 bits

			pixelIdx := i / 2
			y := pixelIdx / width
			x := pixelIdx % width

			img.Set(x, y, color.RGBA{
				uint8(r << 3), // Scale to 8-bit
				uint8(g << 2), // Scale to 8-bit
				uint8(b << 3), // Scale to 8-bit
				255,
			})
		}
	default: // 24 or 32-bit
		// Standard BGRA to RGBA conversion
		bytesPerPixel := 4
		if colorDepth == 24 {
			bytesPerPixel = 3
		}

		for i := 0; i < len(buffer); i += bytesPerPixel {
			b := buffer[i]
			g := buffer[i+1]
			r := buffer[i+2]
			a := uint8(255)
			if bytesPerPixel == 4 {
				a = buffer[i+3]
			}

			// Apply quality-based compression more conservatively
			if quality <= 3 {
				// Only apply dithering for very low quality settings
				r = (r / 64) * 64 // Less aggressive reduction
				g = (g / 64) * 64
				b = (b / 64) * 64
			} else if quality <= 5 {
				// Moderate quality reduction
				r = (r / 16) * 16
				g = (g / 16) * 16
				b = (b / 16) * 16
			}
			// For quality > 5, don't reduce color precision

			pixel := i / bytesPerPixel
			y := pixel / width
			x := pixel % width

			img.Set(x, y, color.RGBA{r, g, b, a})
		}
	}
}

func detectChangedRegions(oldImg, newImg *image.RGBA, blockSize int) []image.Rectangle {
	if oldImg == nil {
		// First frame, return entire screen
		bounds := newImg.Bounds()
		return []image.Rectangle{bounds}
	}

	bounds := newImg.Bounds()
	width := bounds.Dx()
	height := bounds.Dy()

	var changedRegions []image.Rectangle

	// Compare images in blocks to detect changes
	for y := 0; y < height; y += blockSize {
		for x := 0; x < width; x += blockSize {
			endX := x + blockSize
			endY := y + blockSize
			if endX > width {
				endX = width
			}
			if endY > height {
				endY = height
			}

			// Check if this block has changed
			changed := false
			for by := y; by < endY && !changed; by++ {
				for bx := x; bx < endX && !changed; bx++ {
					oldR, oldG, oldB, _ := oldImg.At(bx, by).RGBA()
					newR, newG, newB, _ := newImg.At(bx, by).RGBA()

					// Threshold for change detection (adjust for sensitivity)
					threshold := uint32(1000)
					if abs32(oldR-newR) > threshold ||
						abs32(oldG-newG) > threshold ||
						abs32(oldB-newB) > threshold {
						changed = true
					}
				}
			}

			if changed {
				changedRegions = append(changedRegions, image.Rect(x, y, endX, endY))
			}
		}
	}

	// Merge adjacent regions to reduce the number of rectangles
	return mergeAdjacentRegions(changedRegions)
}

func abs32(x uint32) uint32 {
	if x < 0 {
		return ^x + 1
	}
	return x
}

func mergeAdjacentRegions(regions []image.Rectangle) []image.Rectangle {
	if len(regions) <= 1 {
		return regions
	}

	merged := make([]image.Rectangle, 0, len(regions))
	merged = append(merged, regions[0])

	for i := 1; i < len(regions); i++ {
		current := regions[i]
		merged = append(merged, current)

		// Simple horizontal merge (can be improved)
		for j := 0; j < len(merged)-1; j++ {
			if merged[j].Max.Y == current.Min.Y &&
				merged[j].Min.X == current.Min.X &&
				merged[j].Max.X == current.Max.X {
				// Merge vertically adjacent rectangles
				merged[j] = image.Rect(merged[j].Min.X, merged[j].Min.Y,
					merged[j].Max.X, current.Max.Y)
				merged = append(merged[:len(merged)-1], merged[len(merged):]...)
				break
			}
		}
	}

	return merged
}

// Enhanced change detection with motion detection and adaptive thresholds
func detectChangedRegionsAdvanced(oldImg, newImg *image.RGBA, blockSize int, quality int) []image.Rectangle {
	if oldImg == nil {
		// First frame, return entire screen
		bounds := newImg.Bounds()
		return []image.Rectangle{bounds}
	}

	bounds := newImg.Bounds()
	width := bounds.Dx()
	height := bounds.Dy()

	var changedRegions []image.Rectangle
	var motionDetected bool

	// Adaptive threshold based on recent motion
	currentThreshold := staticThreshold
	if framesSinceMotion < 10 {
		currentThreshold = motionThreshold
	}

	// First pass: detect high-change areas (likely motion)
	motionRegions := make(map[image.Point]bool)

	// Compare images in blocks to detect changes
	for y := 0; y < height; y += blockSize {
		for x := 0; x < width; x += blockSize {
			endX := x + blockSize
			endY := y + blockSize
			if endX > width {
				endX = width
			}
			if endY > height {
				endY = height
			}

			// Check if this block has changed
			changed := false
			totalDiff := uint64(0)
			pixelCount := 0

			for by := y; by < endY && !changed; by++ {
				for bx := x; bx < endX && !changed; bx++ {
					oldR, oldG, oldB, _ := oldImg.At(bx, by).RGBA()
					newR, newG, newB, _ := newImg.At(bx, by).RGBA()

					// Calculate total difference for this pixel
					diff := abs32(oldR-newR) + abs32(oldG-newG) + abs32(oldB-newB)
					totalDiff += uint64(diff)
					pixelCount++

					if diff > currentThreshold {
						changed = true
					}
				}
			}

			if changed {
				region := image.Rect(x, y, endX, endY)
				changedRegions = append(changedRegions, region)

				// Check if this indicates motion (large changes)
				avgDiff := totalDiff / uint64(pixelCount)
				if avgDiff > uint64(motionThreshold) {
					motionDetected = true
					motionRegions[image.Point{x, y}] = true
				}
			}
		}
	}

	// Update motion tracking
	if motionDetected {
		framesSinceMotion = 0
	} else {
		framesSinceMotion++
	}

	// Enhanced region merging with motion consideration
	return mergeAdjacentRegionsAdvanced(changedRegions, motionRegions, quality)
}

// Advanced region merging that considers motion areas and quality settings
func mergeAdjacentRegionsAdvanced(regions []image.Rectangle, motionRegions map[image.Point]bool, quality int) []image.Rectangle {
	if len(regions) <= 1 {
		return regions
	}

	// Sort regions by position for better merging
	// For simplicity, we'll use the existing basic merger but with motion awareness
	merged := mergeAdjacentRegions(regions)

	// For high quality, try to split large regions in motion areas
	if quality >= 8 && len(motionRegions) > 0 {
		var optimized []image.Rectangle
		for _, region := range merged {
			// Check if region overlaps with motion areas
			overlapsMotion := false
			regionKey := image.Point{region.Min.X, region.Min.Y}
			if motionRegions[regionKey] {
				overlapsMotion = true
			}

			// Split large regions in motion areas for better compression
			if overlapsMotion && region.Dx() > 128 && region.Dy() > 128 {
				// Split into smaller regions
				midX := region.Min.X + region.Dx()/2
				midY := region.Min.Y + region.Dy()/2

				optimized = append(optimized,
					image.Rect(region.Min.X, region.Min.Y, midX, midY),
					image.Rect(midX, region.Min.Y, region.Max.X, midY),
					image.Rect(region.Min.X, midY, midX, region.Max.Y),
					image.Rect(midX, midY, region.Max.X, region.Max.Y),
				)
			} else {
				optimized = append(optimized, region)
			}
		}
		return optimized
	}

	return merged
}

// Optimized color difference calculation
func calculatePixelDifference(oldR, oldG, oldB, newR, newG, newB uint32) uint32 {
	// Use weighted RGB difference (human eye is more sensitive to green)
	rDiff := abs32(oldR-newR) * 3
	gDiff := abs32(oldG-newG) * 6 // Green has more weight
	bDiff := abs32(oldB-newB) * 1

	return (rDiff + gDiff + bDiff) / 10
}

func main() {
	config = getConfigFromFlags()

	// Print configuration
	log.Printf("=== VNC Server Starting ===")
	log.Printf("Configuration:")
	log.Printf("  Quality: %d/10", config.Quality)
	log.Printf("  FPS: %.1f", 1000.0/float64(config.UpdateRate/time.Millisecond))
	log.Printf("  Color Depth: %d bits", config.ColorDepth)
	log.Printf("  Skip Unchanged: %v", config.SkipUnchanged)
	log.Printf("  Advanced Optimizations: %v", config.AdvancedOpt)
	log.Printf("  Max Resolution: %d", config.MaxResolution)
	log.Printf("  Compression: %d/9", config.CompressionLevel)
	log.Printf("  Port: %d", config.Port)
	log.Printf("  Verbose Logging: %v", config.EnableLogging)

	// Get screen dimensions for VNC configuration
	widthPtr, _, _ := getSystemMetrics.Call(0)  // SM_CXSCREEN
	heightPtr, _, _ := getSystemMetrics.Call(1) // SM_CYSCREEN
	width := int(widthPtr)
	height := int(heightPtr)

	// Apply resolution limit
	if width > config.MaxResolution {
		width = config.MaxResolution
	}
	if height > config.MaxResolution {
		height = config.MaxResolution
	}

	ln, err := net.Listen("tcp", fmt.Sprintf(":%d", config.Port))
	if err != nil {
		log.Fatalf("Error listen. %v", err)
	}

	chServer := make(chan vnc.ClientMessage)
	chClient := make(chan vnc.ServerMessage)

	// Verify we can capture screenshots
	_, err = captureScreen(config)
	if err != nil {
		log.Fatalf("Error capturing screenshot: %v", err)
	}

	tick := time.NewTicker(config.UpdateRate)
	defer tick.Stop()
	connected := false
	clientCount := 0

	cfg := &vnc.ServerConfig{
		Width:            uint16(width),
		Height:           uint16(height),
		Handlers:         vnc.DefaultServerHandlers,
		SecurityHandlers: []vnc.SecurityHandler{&vnc.ClientAuthNone{}},
		Encodings:        getEncodings(config.Quality),
		PixelFormat:      getPixelFormat(config.ColorDepth),
		ClientMessageCh:  chServer,
		ServerMessageCh:  chClient,
		Messages:         vnc.DefaultClientMessages,
	}

	log.Printf("=== VNC Server Ready ===")
	log.Printf("Server listening on port %d", config.Port)
	log.Printf("Desktop size: %dx%d", width, height)
	log.Printf("Waiting for client connections...")
	log.Printf("Connect using: <YOUR_IP>:%d", config.Port)

	// Create a custom listener wrapper to log connections
	wrappedListener := &loggingListener{
		Listener: ln,
		config:   config,
	}

	go vnc.Serve(context.Background(), wrappedListener, cfg)

	// Process messages coming in on the ClientMessage channel.
	for {
		select {
		case <-tick.C:
			if !connected {
				continue
			}

			// Capture the desktop
			rgbaImg, err := captureScreen(config)
			if err != nil {
				log.Printf("Error capturing screenshot: %v", err)
				continue
			}

			var updateRegions []image.Rectangle

			if config.SkipUnchanged {
				// Detect changed regions for bandwidth optimization
				blockSize := 64 // Adjust based on quality setting
				if config.Quality <= 3 {
					blockSize = 128 // Larger blocks for low quality
				} else if config.Quality >= 8 {
					blockSize = 32 // Smaller blocks for high quality
				}

				// Choose optimization method based on configuration
				if config.AdvancedOpt {
					updateRegions = detectChangedRegionsAdvanced(lastImage, rgbaImg, blockSize, config.Quality)
				} else {
					updateRegions = detectChangedRegions(lastImage, rgbaImg, blockSize)
				}

				if config.EnableLogging && len(updateRegions) > 0 {
					log.Printf("Detected %d changed regions", len(updateRegions))
				}
			} else {
				// Update entire screen
				updateRegions = []image.Rectangle{rgbaImg.Bounds()}
			}

			// Send updates for changed regions only
			if len(updateRegions) > 0 {
				rectangles := make([]*vnc.Rectangle, 0, len(updateRegions))

				for _, region := range updateRegions {
					colors := make([]vnc.Color, 0, region.Dx()*region.Dy())

					for y := region.Min.Y; y < region.Max.Y; y++ {
						for x := region.Min.X; x < region.Max.X; x++ {
							r, g, b, a := rgbaImg.At(x, y).RGBA()
							clr := rgbaToColor(cfg.PixelFormat, r, g, b, a)
							colors = append(colors, *clr)
						}
					}

					rectangles = append(rectangles, &vnc.Rectangle{
						X:       uint16(region.Min.X),
						Y:       uint16(region.Min.Y),
						Width:   uint16(region.Dx()),
						Height:  uint16(region.Dy()),
						EncType: vnc.EncRaw,
						Enc: &vnc.RawEncoding{
							Colors: colors,
						},
					})
				}

				cfg.ServerMessageCh <- &vnc.FramebufferUpdate{
					NumRect: uint16(len(rectangles)),
					Rects:   rectangles,
				}
			}

			// Store current image for next comparison
			if config.SkipUnchanged {
				lastImage = rgbaImg
			}

			// Track and log optimization statistics
			logOptimizationStats(updateRegions, width, height)

		case msg := <-chServer:
			switch msg.Type() {
			case vnc.FramebufferUpdateRequestMsgType:
				if !connected {
					connected = true
					clientCount++
					log.Printf("✅ CLIENT CONNECTED: Client #%d is now active and requesting framebuffer updates", clientCount)
					log.Printf("📊 Active clients: %d", clientCount)
				}
				if config.EnableLogging {
					log.Printf("📱 Framebuffer update requested by client")
				}
			case vnc.KeyEventMsgType:
				if config.EnableLogging {
					keyMsg := msg.(*vnc.KeyEvent)
					action := "pressed"
					if keyMsg.Down == 0 {
						action = "released"
					}
					log.Printf("⌨️  Key %s: %d", action, keyMsg.Key)
				}
			case vnc.PointerEventMsgType:
				if config.EnableLogging {
					ptrMsg := msg.(*vnc.PointerEvent)
					log.Printf("🖱️  Mouse: x=%d, y=%d, buttons=%d", ptrMsg.X, ptrMsg.Y, ptrMsg.Mask)
				}
			case vnc.ClientCutTextMsgType:
				if config.EnableLogging {
					cutMsg := msg.(*vnc.ClientCutText)
					log.Printf("📋 Clipboard text received: %q", string(cutMsg.Text))
				}
			default:
				log.Printf("📨 Received client message type: %v", msg.Type())
				if config.EnableLogging {
					log.Printf("📨 Full message: %v", msg)
				}
			}
		}
	}
}

func rgbaToColor(pf *vnc.PixelFormat, r uint32, g uint32, b uint32, a uint32) *vnc.Color {
	// Convert from 16-bit color values (0-65535) to the pixel format's expected range
	clr := vnc.NewColor(pf, nil)

	// Convert to 8-bit values first (0-255)
	r8 := uint8(r >> 8)
	g8 := uint8(g >> 8)
	b8 := uint8(b >> 8)

	// Set the color values directly as 16-bit values scaled properly
	clr.R = uint16(r8)
	clr.G = uint16(g8)
	clr.B = uint16(b8)

	return clr
}

// Custom listener wrapper to log connection attempts
type loggingListener struct {
	net.Listener
	config Config
}

func (l *loggingListener) Accept() (net.Conn, error) {
	conn, err := l.Listener.Accept()
	if err != nil {
		return nil, err
	}

	// Log connection attempt
	remoteAddr := conn.RemoteAddr().String()
	log.Printf("🔌 CONNECTION ATTEMPT from %s", remoteAddr)

	// Wrap the connection to log when it closes
	wrappedConn := &loggingConn{
		Conn:       conn,
		remoteAddr: remoteAddr,
		config:     l.config,
	}

	return wrappedConn, nil
}

// Custom connection wrapper to log disconnections
type loggingConn struct {
	net.Conn
	remoteAddr string
	config     Config
	closed     bool
}

func (c *loggingConn) Close() error {
	if !c.closed {
		c.closed = true
		log.Printf("❌ CLIENT DISCONNECTED: %s", c.remoteAddr)
	}
	return c.Conn.Close()
}

func (c *loggingConn) Read(b []byte) (n int, err error) {
	n, err = c.Conn.Read(b)
	if err != nil && !c.closed {
		c.closed = true
		log.Printf("❌ CLIENT DISCONNECTED: %s (read error: %v)", c.remoteAddr, err)
		logConnectionError(err, c.remoteAddr)
	}
	return n, err
}

func (c *loggingConn) Write(b []byte) (n int, err error) {
	n, err = c.Conn.Write(b)
	if err != nil && !c.closed {
		c.closed = true
		log.Printf("❌ CLIENT DISCONNECTED: %s (write error: %v)", c.remoteAddr, err)
		logConnectionError(err, c.remoteAddr)
	}
	return n, err
}

// Config returns connection config
func (c *loggingConn) Config() interface{} {
	return c.config
}

// Additional error handling for better compatibility
func logConnectionError(err error, remoteAddr string) {
	if err != nil {
		errStr := err.Error()
		log.Printf("❌ CONNECTION ERROR from %s: %v", remoteAddr, err)

		// Provide specific guidance for known compatibility issues
		if len(errStr) > 0 {
			if fmt.Sprintf("%v", err) == "protocol error: invalid message type 20" ||
				len(errStr) > 20 && errStr[0:20] == "protocol error: invalid message type" {
				log.Printf("💡 COMPATIBILITY ISSUE DETECTED:")
				log.Printf("   This error typically occurs when using RealVNC client with extended features")
				log.Printf("   RECOMMENDED SOLUTIONS:")
				log.Printf("   1. Use TightVNC Viewer instead (confirmed working)")
				log.Printf("   2. Use UltraVNC Viewer")
				log.Printf("   3. If using RealVNC, try disabling advanced features/extensions")
				log.Printf("   4. Use command-line VNC clients like 'vncviewer' (standard)")
				log.Printf("")
				log.Printf("   Technical: RealVNC uses proprietary extensions (like message type 20)")
				log.Printf("   that are not supported by the basic RFB protocol implementation")
			}
		}
	}
}

// Track and log optimization statistics
func logOptimizationStats(updateRegions []image.Rectangle, screenWidth, screenHeight int) {
	totalFrames++
	totalRegions += int64(len(updateRegions))

	// Calculate pixels sent vs total screen pixels
	pixelsSent := 0
	for _, region := range updateRegions {
		pixelsSent += region.Dx() * region.Dy()
	}

	totalPixelsSent += int64(pixelsSent)
	totalPixelsScreen += int64(screenWidth * screenHeight)

	// Log stats every 30 seconds
	now := time.Now()
	if lastStatsTime.IsZero() {
		lastStatsTime = now
	}

	if now.Sub(lastStatsTime) >= 30*time.Second {
		if totalPixelsScreen > 0 {
			efficiency := float64(totalPixelsSent) / float64(totalPixelsScreen) * 100
			avgRegionsPerFrame := float64(totalRegions) / float64(totalFrames)

			log.Printf("📊 OPTIMIZATION STATS (last 30s):")
			log.Printf("   Frames sent: %d", totalFrames)
			log.Printf("   Avg regions per frame: %.1f", avgRegionsPerFrame)
			log.Printf("   Pixels sent: %.1fM / %.1fM total (%.1f%% efficiency)",
				float64(totalPixelsSent)/1000000,
				float64(totalPixelsScreen)/1000000,
				efficiency)
			log.Printf("   Bandwidth saved: %.1f%%", 100-efficiency)

			if framesSinceMotion > 0 {
				log.Printf("   Static period: %d frames (adaptive thresholds active)", framesSinceMotion)
			}
		}

		// Reset stats
		totalFrames = 0
		totalRegions = 0
		totalPixelsSent = 0
		totalPixelsScreen = 0
		lastStatsTime = now
	}
}
