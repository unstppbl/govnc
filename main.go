//go:build windows
// +build windows

package main

import (
	"context"
	"image"
	"image/color"
	"log"
	"net"
	"syscall"
	"time"
	"unsafe"

	vnc "github.com/unistack-org/go-rfb"
)

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

func captureScreen() (*image.RGBA, error) {
	// Get screen dimensions
	widthPtr, _, _ := getSystemMetrics.Call(0)  // SM_CXSCREEN
	heightPtr, _, _ := getSystemMetrics.Call(1) // SM_CYSCREEN
	width := int(widthPtr)
	height := int(heightPtr)

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

	// Prepare bitmap info
	bi := BITMAPINFOHEADER{
		Size:     uint32(unsafe.Sizeof(BITMAPINFOHEADER{})),
		Width:    int32(width),
		Height:   -int32(height), // negative for top-down bitmap
		Planes:   1,
		BitCount: 32,
	}

	// Allocate buffer for pixel data
	bufferSize := width * height * 4
	buffer := make([]byte, bufferSize)

	// Get the bits
	getDIBits.Call(hCaptureDC, hCaptureBitmap, 0, uintptr(height),
		uintptr(unsafe.Pointer(&buffer[0])),
		uintptr(unsafe.Pointer(&bi)), 0)

	// Create RGBA image
	img := image.NewRGBA(image.Rect(0, 0, width, height))

	// Convert BGRA to RGBA
	for i := 0; i < len(buffer); i += 4 {
		b := buffer[i]
		g := buffer[i+1]
		r := buffer[i+2]
		a := buffer[i+3]

		pixel := i / 4
		y := pixel / width
		x := pixel % width

		img.Set(x, y, color.RGBA{r, g, b, a})
	}

	// Cleanup
	deleteObject.Call(hCaptureBitmap)
	deleteDC.Call(hCaptureDC)
	releaseDC.Call(hDesktopWnd, hDesktopDC)

	return img, nil
}

func main() {
	// Get screen dimensions for VNC configuration
	widthPtr, _, _ := getSystemMetrics.Call(0)  // SM_CXSCREEN
	heightPtr, _, _ := getSystemMetrics.Call(1) // SM_CYSCREEN
	width := int(widthPtr)
	height := int(heightPtr)

	ln, err := net.Listen("tcp", ":6900")
	if err != nil {
		log.Fatalf("Error listen. %v", err)
	}

	chServer := make(chan vnc.ClientMessage)
	chClient := make(chan vnc.ServerMessage)

	// Verify we can capture screenshots
	_, err = captureScreen()
	if err != nil {
		log.Fatalf("Error capturing screenshot: %v", err)
	}

	tick := time.NewTicker(time.Millisecond * 100) // Update every 100ms for desktop sharing
	defer tick.Stop()
	connected := false

	cfg := &vnc.ServerConfig{
		Width:            uint16(width),
		Height:           uint16(height),
		Handlers:         vnc.DefaultServerHandlers,
		SecurityHandlers: []vnc.SecurityHandler{&vnc.ClientAuthNone{}},
		Encodings:        []vnc.Encoding{&vnc.RawEncoding{}},
		PixelFormat:      vnc.PixelFormat32bit,
		ClientMessageCh:  chServer,
		ServerMessageCh:  chClient,
		Messages:         vnc.DefaultClientMessages,
	}

	log.Printf("Starting VNC server on port 6900, desktop size: %dx%d", width, height)
	go vnc.Serve(context.Background(), ln, cfg)

	// Process messages coming in on the ClientMessage channel.
	for {
		select {
		case <-tick.C:
			if !connected {
				continue
			}

			// Capture the desktop
			rgbaImg, err := captureScreen()
			if err != nil {
				log.Printf("Error capturing screenshot: %v", err)
				continue
			}

			colors := make([]vnc.Color, 0, width*height)
			for y := 0; y < height; y++ {
				for x := 0; x < width; x++ {
					r, g, b, a := rgbaImg.At(x, y).RGBA()
					clr := rgbaToColor(cfg.PixelFormat, r, g, b, a)
					colors = append(colors, *clr)
				}
			}

			cfg.ServerMessageCh <- &vnc.FramebufferUpdate{
				NumRect: 1,
				Rects: []*vnc.Rectangle{
					&vnc.Rectangle{
						X:       0,
						Y:       0,
						Width:   uint16(width),
						Height:  uint16(height),
						EncType: vnc.EncRaw,
						Enc: &vnc.RawEncoding{
							Colors: colors,
						},
					}}}

		case msg := <-chServer:
			switch msg.Type() {
			case vnc.FramebufferUpdateRequestMsgType:
				connected = true
				log.Printf("Client connected and requesting framebuffer updates")
			default:
				log.Printf("Received client message type:%v msg:%v\n", msg.Type(), msg)
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
