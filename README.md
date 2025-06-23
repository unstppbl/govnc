# Windows VNC Desktop Sharing Server

This Go application creates a VNC server that captures and shares the Windows desktop in real-time.

## Features

- Real-time Windows desktop capture using Windows GDI APIs
- VNC server on port 6900
- No authentication required (for local testing)
- Raw encoding for maximum compatibility

## Requirements

- Windows operating system
- Go 1.21 or later
- Must be compiled on Windows (uses Windows-specific APIs)

## Building

```bash
go mod tidy
go build -o govnc.exe main.go
```

## Usage

1. Run the executable:
   ```bash
   govnc.exe
   ```

2. Connect using any VNC client to `localhost:6900`

## How it Works

The application uses Windows GDI32 and User32 APIs to:

1. Capture the desktop using `BitBlt` and `GetDIBits`
2. Convert the captured bitmap from BGRA to RGBA format
3. Send framebuffer updates to connected VNC clients
4. Updates every 100ms for smooth desktop sharing

## VNC Client Connection

Connect to:
- **Host**: `localhost` or your machine's IP address
- **Port**: `6900`
- **Password**: None (no authentication)

## Technical Details

- Uses Windows syscalls through Go's `syscall` package
- Captures primary display only
- 32-bit color depth (RGBA)
- Raw VNC encoding for best compatibility
- Automatic screen resolution detection

## Notes

- This is designed for Windows only
- For production use, consider adding authentication
- Performance depends on screen resolution and update frequency
- The application must be compiled on Windows due to Windows-specific APIs 