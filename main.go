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
	lastImage *image.RGBA
	config    Config
)

func getConfigFromFlags() Config {
	quality := flag.Int("quality", 7, "Image quality 1-10 (1=lowest bandwidth, 10=highest quality)")
	fps := flag.Int("fps", 10, "Updates per second (lower = less bandwidth)")
	depth := flag.Int("depth", 16, "Color depth: 8,16,24,32 bits (lower = less bandwidth)")
	skipUnchanged := flag.Bool("skip-unchanged", true, "Skip unchanged regions (saves bandwidth)")
	maxRes := flag.Int("max-res", 1920, "Maximum resolution (lower = less bandwidth)")
	compression := flag.Int("compression", 6, "Compression level 1-9 (higher = more compression)")
	port := flag.Int("port", 6900, "VNC server port")
	verbose := flag.Bool("verbose", false, "Enable detailed logging")

	flag.Parse()

	updateInterval := time.Duration(1000 / *fps) * time.Millisecond

	return Config{
		Quality:          *quality,
		UpdateRate:       updateInterval,
		ColorDepth:       *depth,
		SkipUnchanged:    *skipUnchanged,
		MaxResolution:    *maxRes,
		CompressionLevel: *compression,
		Port:             *port,
		EnableLogging:    *verbose,
	}
}

func getPixelFormat(colorDepth int) *vnc.PixelFormat {
	// Use the library's default pixel format for compatibility
	// The library will handle color depth conversion
	return vnc.PixelFormat32bit
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

			// Apply quality-based compression (dithering for lower quality)
			if quality <= 5 {
				r = (r / 32) * 32 // Reduce color precision for lower quality
				g = (g / 32) * 32
				b = (b / 32) * 32
			}

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

func main() {
	config = getConfigFromFlags()

	// Print configuration
	log.Printf("VNC Server Configuration:")
	log.Printf("  Quality: %d/10", config.Quality)
	log.Printf("  FPS: %.1f", 1000.0/float64(config.UpdateRate/time.Millisecond))
	log.Printf("  Color Depth: %d bits", config.ColorDepth)
	log.Printf("  Skip Unchanged: %v", config.SkipUnchanged)
	log.Printf("  Max Resolution: %d", config.MaxResolution)
	log.Printf("  Compression: %d/9", config.CompressionLevel)
	log.Printf("  Port: %d", config.Port)

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

	log.Printf("Starting VNC server on port %d, desktop size: %dx%d", config.Port, width, height)
	go vnc.Serve(context.Background(), ln, cfg)

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

				updateRegions = detectChangedRegions(lastImage, rgbaImg, blockSize)

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

		case msg := <-chServer:
			switch msg.Type() {
			case vnc.FramebufferUpdateRequestMsgType:
				connected = true
				if config.EnableLogging {
					log.Printf("Client connected and requesting framebuffer updates")
				}
			default:
				if config.EnableLogging {
					log.Printf("Received client message type:%v msg:%v\n", msg.Type(), msg)
				}
			}
		}
	}
}

func rgbaToColor(pf *vnc.PixelFormat, r uint32, g uint32, b uint32, a uint32) *vnc.Color {
	// fix converting rbga to rgb http://marcodiiga.github.io/rgba-to-rgb-conversion
	clr := vnc.NewColor(pf, nil)
	clr.R = uint16(r / 257)
	clr.G = uint16(g / 257)
	clr.B = uint16(b / 257)
	return clr
}
