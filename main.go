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
	EnableInput      bool          // Enable keyboard and mouse input handling
}

// Windows API constants
const (
	SRCCOPY = 0x00CC0020

	// Input event constants for SendInput
	INPUT_KEYBOARD = 1
	INPUT_MOUSE    = 0

	// Keyboard event flags
	KEYEVENTF_KEYUP = 0x0002

	// Mouse event flags
	MOUSEEVENTF_LEFTDOWN   = 0x0002
	MOUSEEVENTF_LEFTUP     = 0x0004
	MOUSEEVENTF_RIGHTDOWN  = 0x0008
	MOUSEEVENTF_RIGHTUP    = 0x0010
	MOUSEEVENTF_MIDDLEDOWN = 0x0020
	MOUSEEVENTF_MIDDLEUP   = 0x0040
	MOUSEEVENTF_MOVE       = 0x0001
	MOUSEEVENTF_ABSOLUTE   = 0x8000
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

	// Input handling APIs - using modern SendInput API
	sendInput     = user32.NewProc("SendInput")
	setCursorPos  = user32.NewProc("SetCursorPos")
	getCursorPos  = user32.NewProc("GetCursorPos")
	mapVirtualKey = user32.NewProc("MapVirtualKeyW")
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

	// Track previous mouse state for proper button handling
	lastMouseX       int
	lastMouseY       int
	lastMouseButtons uint8
)

// Optimization statistics
var (
	totalFrames       int64
	totalRegions      int64
	totalPixelsSent   int64
	totalPixelsScreen int64
	lastStatsTime     time.Time
)

// Correct INPUT structures for 64-bit Windows
type KEYBDINPUT struct {
	Wvk         uint16
	Wscan       uint16
	Dwflags     uint32
	Time        uint32
	DwextraInfo uintptr
}

type MOUSEINPUT struct {
	Dx          int32
	Dy          int32
	MouseData   uint32
	Dwflags     uint32
	Time        uint32
	DwextraInfo uintptr
}

type HARDWAREINPUT struct {
	UMsg    uint32
	WParamL uint16
	WParamH uint16
}

// Properly aligned INPUT structure for 64-bit Windows
type INPUT struct {
	Type uint32
	_    uint32 // Explicit padding for 64-bit alignment
	// Union of input structures - using the largest one
	Ki KEYBDINPUT
	_  [8]byte // Padding to ensure proper size
}

func getConfigFromFlags() Config {
	quality := flag.Int("quality", 6, "Image quality 1-10 (1=lowest bandwidth, 10=highest quality)")
	fps := flag.Int("fps", 60, "Updates per second (lower = less bandwidth)")
	depth := flag.Int("depth", 32, "Color depth: 8,16,24,32 bits (lower = less bandwidth)")
	skipUnchanged := flag.Bool("skip-unchanged", true, "Skip unchanged regions (saves bandwidth)")
	maxRes := flag.Int("max-res", 1920, "Maximum resolution (lower = less bandwidth)")
	compression := flag.Int("compression", 6, "Compression level 1-9 (higher = more compression)")
	port := flag.Int("port", 6900, "VNC server port")
	verbose := flag.Bool("verbose", true, "Enable detailed logging")
	compatMode := flag.Bool("compat", false, "Enable compatibility mode for basic VNC clients")
	advancedOpt := flag.Bool("advanced-opt", true, "Enable advanced optimizations (motion detection, adaptive thresholds)")
	input := flag.Bool("input", true, "Enable keyboard and mouse input handling")

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
		EnableInput:      *input,
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

	// Get desktop window and DC - try to get the most current view
	hDesktopWnd, _, _ := getDesktopWindow.Call()
	hDesktopDC, _, _ := getDC.Call(hDesktopWnd)

	// Force a refresh of the desktop to ensure we get current content
	// This helps prevent stale cached content from Windows DWM
	hCaptureDC, _, _ := createCompatibleDC.Call(hDesktopDC)
	hCaptureBitmap, _, _ := createCompatibleBitmap.Call(hDesktopDC, uintptr(width), uintptr(height))

	// Select the bitmap into the compatible DC
	selectObject.Call(hCaptureDC, hCaptureBitmap)

	// Copy the desktop to our bitmap - this should get the current state
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

	// Get the bits - this should now contain current desktop content
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
	log.Printf("  Input Handling: %v", config.EnableInput)
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

	// Add a periodic full refresh ticker to prevent stale content
	fullRefreshTick := time.NewTicker(5 * time.Second) // Force full refresh every 5 seconds
	defer fullRefreshTick.Stop()

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
			forceFullUpdate := false

			if config.SkipUnchanged && !forceFullUpdate {
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
				if forceFullUpdate && config.EnableLogging {
					log.Printf("🔄 Forced full screen refresh")
				}
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

		case <-fullRefreshTick.C:
			if connected && config.EnableLogging {
				log.Printf("🔄 Periodic full refresh triggered - clearing cache to prevent stale content")
			}
			// Clear the last image to force a full update on next tick
			lastImage = nil

		case msg := <-chServer:
			switch msg.Type() {
			case vnc.FramebufferUpdateRequestMsgType:
				if !connected {
					connected = true
					clientCount++
					log.Printf("✅ CLIENT CONNECTED: Client #%d is now active and requesting framebuffer updates", clientCount)
					log.Printf("📊 Active clients: %d", clientCount)
				}

				// Parse the framebuffer update request for detailed logging
				if config.EnableLogging {
					fbReq := msg.(*vnc.FramebufferUpdateRequest)
					updateType := "FULL"
					if fbReq.Inc != 0 {
						updateType = "INCREMENTAL"
					}
					log.Printf("📱 Framebuffer update requested: %s update for region (%d,%d) %dx%d",
						updateType, fbReq.X, fbReq.Y, fbReq.Width, fbReq.Height)
				}

				// Note: We use timer-based updates rather than immediate response
				// This provides consistent frame rates and better performance
			case vnc.KeyEventMsgType:
				keyMsg := msg.(*vnc.KeyEvent)
				keyDown := keyMsg.Down != 0
				vncKey := uint32(keyMsg.Key) // Convert rfb.Key to uint32

				if config.EnableLogging {
					action := "pressed"
					if !keyDown {
						action = "released"
					}
					log.Printf("⌨️  Key %s: VNC=0x%x (%d)", action, vncKey, vncKey)
				}

				// Convert VNC key code to Windows virtual key code and apply if enabled
				if config.EnableInput {
					if winVK, found := vncKeyToWinVK(vncKey); found {
						err := sendKeyboardInput(winVK, keyDown)
						if err != nil && config.EnableLogging {
							log.Printf("⚠️  Keyboard input error: %v", err)
						} else if config.EnableLogging {
							log.Printf("✅ Key applied: VNC=0x%x -> Win=0x%x", vncKey, winVK)
						}
					} else if config.EnableLogging {
						log.Printf("⚠️  Unknown VNC key code: 0x%x (%d)", vncKey, vncKey)
					}
				} else if config.EnableLogging {
					log.Printf("📝 Input handling disabled - key ignored")
				}

			case vnc.PointerEventMsgType:
				ptrMsg := msg.(*vnc.PointerEvent)
				x := int(ptrMsg.X)
				y := int(ptrMsg.Y)
				buttons := ptrMsg.Mask

				if config.EnableLogging {
					log.Printf("🖱️  Mouse: x=%d, y=%d, buttons=0x%02x", x, y, buttons)
				}

				// Apply mouse input to Windows if enabled
				if config.EnableInput {
					err := sendMouseInput(x, y, buttons, 0, 0)
					if err != nil && config.EnableLogging {
						log.Printf("⚠️  Mouse input error: %v", err)
					} else if config.EnableLogging {
						buttonStr := ""
						if buttons&0x01 != 0 {
							buttonStr += "L"
						}
						if buttons&0x02 != 0 {
							buttonStr += "M"
						}
						if buttons&0x04 != 0 {
							buttonStr += "R"
						}
						if buttonStr == "" {
							buttonStr = "none"
						}
						log.Printf("✅ Mouse applied: (%d,%d) buttons=%s", x, y, buttonStr)
					}
				} else if config.EnableLogging {
					log.Printf("📝 Input handling disabled - mouse ignored")
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

// VNC Key code to Windows Virtual Key code mapping
func vncKeyToWinVK(vncKey uint32) (uint16, bool) {
	// Common VNC key codes to Windows Virtual Key codes
	keyMap := map[uint32]uint16{
		// Letters (VNC uses ASCII for a-z, A-Z)
		0x61: 0x41, // 'a' -> VK_A
		0x62: 0x42, // 'b' -> VK_B
		0x63: 0x43, // 'c' -> VK_C
		0x64: 0x44, // 'd' -> VK_D
		0x65: 0x45, // 'e' -> VK_E
		0x66: 0x46, // 'f' -> VK_F
		0x67: 0x47, // 'g' -> VK_G
		0x68: 0x48, // 'h' -> VK_H
		0x69: 0x49, // 'i' -> VK_I
		0x6a: 0x4A, // 'j' -> VK_J
		0x6b: 0x4B, // 'k' -> VK_K
		0x6c: 0x4C, // 'l' -> VK_L
		0x6d: 0x4D, // 'm' -> VK_M
		0x6e: 0x4E, // 'n' -> VK_N
		0x6f: 0x4F, // 'o' -> VK_O
		0x70: 0x50, // 'p' -> VK_P
		0x71: 0x51, // 'q' -> VK_Q
		0x72: 0x52, // 'r' -> VK_R
		0x73: 0x53, // 's' -> VK_S
		0x74: 0x54, // 't' -> VK_T
		0x75: 0x55, // 'u' -> VK_U
		0x76: 0x56, // 'v' -> VK_V
		0x77: 0x57, // 'w' -> VK_W
		0x78: 0x58, // 'x' -> VK_X
		0x79: 0x59, // 'y' -> VK_Y
		0x7a: 0x5A, // 'z' -> VK_Z

		// Numbers
		0x30: 0x30, // '0' -> VK_0
		0x31: 0x31, // '1' -> VK_1
		0x32: 0x32, // '2' -> VK_2
		0x33: 0x33, // '3' -> VK_3
		0x34: 0x34, // '4' -> VK_4
		0x35: 0x35, // '5' -> VK_5
		0x36: 0x36, // '6' -> VK_6
		0x37: 0x37, // '7' -> VK_7
		0x38: 0x38, // '8' -> VK_8
		0x39: 0x39, // '9' -> VK_9

		// Special keys
		0xff08: 0x08, // BackSpace -> VK_BACK
		0xff09: 0x09, // Tab -> VK_TAB
		0xff0d: 0x0D, // Return -> VK_RETURN
		0xff1b: 0x1B, // Escape -> VK_ESCAPE
		0xff20: 0x14, // Caps_Lock -> VK_CAPITAL
		0xffe1: 0x10, // Shift_L -> VK_SHIFT
		0xffe2: 0x10, // Shift_R -> VK_SHIFT
		0xffe3: 0x11, // Control_L -> VK_CONTROL
		0xffe4: 0x11, // Control_R -> VK_CONTROL
		0xffe9: 0x12, // Alt_L -> VK_MENU
		0xffea: 0x12, // Alt_R -> VK_MENU
		0xffeb: 0x5B, // Super_L -> VK_LWIN
		0xffec: 0x5C, // Super_R -> VK_RWIN

		// Arrow keys
		0xff51: 0x25, // Left -> VK_LEFT
		0xff52: 0x26, // Up -> VK_UP
		0xff53: 0x27, // Right -> VK_RIGHT
		0xff54: 0x28, // Down -> VK_DOWN

		// Function keys
		0xffbe: 0x70, // F1 -> VK_F1
		0xffbf: 0x71, // F2 -> VK_F2
		0xffc0: 0x72, // F3 -> VK_F3
		0xffc1: 0x73, // F4 -> VK_F4
		0xffc2: 0x74, // F5 -> VK_F5
		0xffc3: 0x75, // F6 -> VK_F6
		0xffc4: 0x76, // F7 -> VK_F7
		0xffc5: 0x77, // F8 -> VK_F8
		0xffc6: 0x78, // F9 -> VK_F9
		0xffc7: 0x79, // F10 -> VK_F10
		0xffc8: 0x7A, // F11 -> VK_F11
		0xffc9: 0x7B, // F12 -> VK_F12

		// Other keys
		0xff50: 0x24, // Home -> VK_HOME
		0xff57: 0x23, // End -> VK_END
		0xff55: 0x21, // Page_Up -> VK_PRIOR
		0xff56: 0x22, // Page_Down -> VK_NEXT
		0xffff: 0x2E, // Delete -> VK_DELETE
		0xff63: 0x2D, // Insert -> VK_INSERT
		0x20:   0x20, // Space -> VK_SPACE
	}

	// Handle uppercase letters (VNC sends uppercase ASCII)
	if vncKey >= 0x41 && vncKey <= 0x5A {
		return uint16(vncKey), true // A-Z map directly
	}

	// Handle lowercase letters
	if vncKey >= 0x61 && vncKey <= 0x7A {
		return uint16(vncKey - 0x20), true // a-z -> A-Z
	}

	if winVK, exists := keyMap[vncKey]; exists {
		return winVK, true
	}

	return 0, false
}

// Send keyboard input to Windows using SendInput (modern, reliable API)
func sendKeyboardInput(vkey uint16, keyDown bool) error {
	var flags uint32 = 0
	if !keyDown {
		flags = KEYEVENTF_KEYUP
	}

	if config.EnableLogging {
		action := "DOWN"
		if !keyDown {
			action = "UP"
		}
		log.Printf("🔧 Sending keyboard input: VK=0x%02X (%d) %s", vkey, vkey, action)
	}

	// Create INPUT structure for keyboard input
	input := INPUT{
		Type: INPUT_KEYBOARD,
		Ki: KEYBDINPUT{
			Wvk:         vkey,
			Wscan:       0,
			Dwflags:     flags,
			Time:        0,
			DwextraInfo: 0,
		},
	}

	// Call SendInput
	ret, _, err := sendInput.Call(
		uintptr(1),                      // Number of inputs
		uintptr(unsafe.Pointer(&input)), // Pointer to INPUT array
		uintptr(unsafe.Sizeof(input)),   // Size of INPUT structure
	)

	if config.EnableLogging {
		log.Printf("🔧 SendInput keyboard result: ret=%d, err=%v", ret, err)
	}

	if ret == 0 {
		return fmt.Errorf("SendInput keyboard failed: %v", err)
	}
	return nil
}

// Send mouse input to Windows using SendInput (modern, reliable API)
func sendMouseInput(x, y int, buttons uint8, deltaX, deltaY int) error {
	if config.EnableLogging {
		log.Printf("🔧 Sending mouse input: (%d,%d) buttons=0x%02X", x, y, buttons)
	}

	// Get screen dimensions for absolute coordinate conversion
	screenWidth, _, _ := getSystemMetrics.Call(0)  // SM_CXSCREEN
	screenHeight, _, _ := getSystemMetrics.Call(1) // SM_CYSCREEN

	if screenWidth == 0 || screenHeight == 0 {
		return fmt.Errorf("invalid screen dimensions: %dx%d", screenWidth, screenHeight)
	}

	// Convert to absolute coordinates (0-65535 range)
	absX := int32((int64(x) * 65535) / int64(screenWidth))
	absY := int32((int64(y) * 65535) / int64(screenHeight))

	// Create mouse input for movement
	moveInput := INPUT{
		Type: INPUT_MOUSE,
	}

	// We need to manually set the mouse input data since Go doesn't support unions directly
	// Cast the Ki field to MOUSEINPUT since it's the same memory layout
	mouseData := (*MOUSEINPUT)(unsafe.Pointer(&moveInput.Ki))
	mouseData.Dx = absX
	mouseData.Dy = absY
	mouseData.MouseData = 0
	mouseData.Dwflags = MOUSEEVENTF_MOVE | MOUSEEVENTF_ABSOLUTE
	mouseData.Time = 0
	mouseData.DwextraInfo = 0

	// Send movement
	ret, _, err := sendInput.Call(
		uintptr(1),                          // Number of inputs
		uintptr(unsafe.Pointer(&moveInput)), // Pointer to INPUT array
		uintptr(unsafe.Sizeof(moveInput)),   // Size of INPUT structure
	)

	if config.EnableLogging {
		log.Printf("🔧 SendInput mouse move result: ret=%d, err=%v (coords: %d,%d -> %d,%d)", ret, err, x, y, absX, absY)
	}

	if ret == 0 {
		return fmt.Errorf("SendInput mouse move failed: %v (coords: %d,%d -> %d,%d)", err, x, y, absX, absY)
	}

	// Handle button state changes
	buttonChanges := lastMouseButtons ^ buttons // XOR to find changes

	if buttonChanges&0x01 != 0 { // Left button changed
		var flags uint32
		if buttons&0x01 != 0 {
			flags = MOUSEEVENTF_LEFTDOWN
		} else {
			flags = MOUSEEVENTF_LEFTUP
		}

		if config.EnableLogging {
			action := "DOWN"
			if flags == MOUSEEVENTF_LEFTUP {
				action = "UP"
			}
			log.Printf("🔧 Sending left button %s", action)
		}

		buttonInput := INPUT{Type: INPUT_MOUSE}
		buttonMouseData := (*MOUSEINPUT)(unsafe.Pointer(&buttonInput.Ki))
		buttonMouseData.Dx = 0
		buttonMouseData.Dy = 0
		buttonMouseData.MouseData = 0
		buttonMouseData.Dwflags = flags
		buttonMouseData.Time = 0
		buttonMouseData.DwextraInfo = 0

		ret, _, err := sendInput.Call(
			uintptr(1),
			uintptr(unsafe.Pointer(&buttonInput)),
			uintptr(unsafe.Sizeof(buttonInput)),
		)

		if config.EnableLogging {
			log.Printf("🔧 SendInput left button result: ret=%d, err=%v", ret, err)
		}

		if ret == 0 {
			return fmt.Errorf("SendInput left button failed: %v", err)
		}
	}

	if buttonChanges&0x02 != 0 { // Middle button changed
		var flags uint32
		if buttons&0x02 != 0 {
			flags = MOUSEEVENTF_MIDDLEDOWN
		} else {
			flags = MOUSEEVENTF_MIDDLEUP
		}

		if config.EnableLogging {
			action := "DOWN"
			if flags == MOUSEEVENTF_MIDDLEUP {
				action = "UP"
			}
			log.Printf("🔧 Sending middle button %s", action)
		}

		buttonInput := INPUT{Type: INPUT_MOUSE}
		buttonMouseData := (*MOUSEINPUT)(unsafe.Pointer(&buttonInput.Ki))
		buttonMouseData.Dx = 0
		buttonMouseData.Dy = 0
		buttonMouseData.MouseData = 0
		buttonMouseData.Dwflags = flags
		buttonMouseData.Time = 0
		buttonMouseData.DwextraInfo = 0

		ret, _, err := sendInput.Call(
			uintptr(1),
			uintptr(unsafe.Pointer(&buttonInput)),
			uintptr(unsafe.Sizeof(buttonInput)),
		)

		if config.EnableLogging {
			log.Printf("🔧 SendInput middle button result: ret=%d, err=%v", ret, err)
		}

		if ret == 0 {
			return fmt.Errorf("SendInput middle button failed: %v", err)
		}
	}

	if buttonChanges&0x04 != 0 { // Right button changed
		var flags uint32
		if buttons&0x04 != 0 {
			flags = MOUSEEVENTF_RIGHTDOWN
		} else {
			flags = MOUSEEVENTF_RIGHTUP
		}

		if config.EnableLogging {
			action := "DOWN"
			if flags == MOUSEEVENTF_RIGHTUP {
				action = "UP"
			}
			log.Printf("🔧 Sending right button %s", action)
		}

		buttonInput := INPUT{Type: INPUT_MOUSE}
		buttonMouseData := (*MOUSEINPUT)(unsafe.Pointer(&buttonInput.Ki))
		buttonMouseData.Dx = 0
		buttonMouseData.Dy = 0
		buttonMouseData.MouseData = 0
		buttonMouseData.Dwflags = flags
		buttonMouseData.Time = 0
		buttonMouseData.DwextraInfo = 0

		ret, _, err := sendInput.Call(
			uintptr(1),
			uintptr(unsafe.Pointer(&buttonInput)),
			uintptr(unsafe.Sizeof(buttonInput)),
		)

		if config.EnableLogging {
			log.Printf("🔧 SendInput right button result: ret=%d, err=%v", ret, err)
		}

		if ret == 0 {
			return fmt.Errorf("SendInput right button failed: %v", err)
		}
	}

	// Update tracking variables
	lastMouseX = x
	lastMouseY = y
	lastMouseButtons = buttons

	return nil
}
