# Windows VNC Desktop Sharing Server

This Go application creates a VNC server that captures and shares the Windows desktop in real-time with comprehensive bandwidth optimization features.

## Features

- Real-time Windows desktop capture using Windows GDI APIs
- **Bandwidth Optimization**: Configurable quality, frame rate, and color depth
- **Smart Updates**: Only sends changed screen regions to save bandwidth
- **Multiple Configurations**: Command-line options for different network conditions
- VNC server with configurable port
- No authentication required (for local testing)

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

### Basic Usage
```bash
govnc.exe
```

### Advanced Configuration
```bash
# Low bandwidth configuration (dial-up/slow mobile)
govnc.exe -quality=2 -fps=5 -depth=8 -max-res=800

# Medium bandwidth configuration (standard broadband)
govnc.exe -quality=6 -fps=15 -depth=16 -max-res=1280

# High bandwidth configuration (fast connection)
govnc.exe -quality=9 -fps=30 -depth=32 -max-res=1920

# Custom configuration
govnc.exe -port=5900 -quality=7 -fps=20 -verbose
```

## Configuration Options

| Option | Default | Description | Bandwidth Impact |
|--------|---------|-------------|------------------|
| `-quality` | 7 | Image quality 1-10 (1=lowest, 10=highest) | Higher = more bandwidth |
| `-fps` | 10 | Updates per second | Higher = more bandwidth |
| `-depth` | 16 | Color depth: 8,16,24,32 bits | Higher = more bandwidth |
| `-skip-unchanged` | true | Skip unchanged regions | Saves significant bandwidth |
| `-max-res` | 1920 | Maximum resolution (width/height) | Lower = less bandwidth |
| `-compression` | 6 | Compression level 1-9 | Higher = less bandwidth |
| `-port` | 6900 | VNC server port | N/A |
| `-verbose` | false | Enable detailed logging | N/A |

## Bandwidth Optimization Features

### 1. **Smart Region Updates**
- Only transmits changed areas of the screen
- Can reduce bandwidth by 80-90% for typical desktop usage
- Configurable block sizes based on quality setting

### 2. **Configurable Quality Levels**
- **Quality 1-3**: Maximum compression, larger update blocks
- **Quality 4-6**: Balanced compression and quality
- **Quality 7-8**: High quality with minimal compression
- **Quality 9-10**: Maximum quality, no compression

### 3. **Adaptive Color Depth**
- **8-bit**: 256 colors, ~75% bandwidth reduction
- **16-bit**: 65K colors, ~50% bandwidth reduction  
- **24-bit**: 16M colors, ~25% bandwidth reduction
- **32-bit**: Full color, maximum bandwidth

### 4. **Dynamic Frame Rates**
- Lower FPS for bandwidth-constrained connections
- Higher FPS for smooth interaction on fast connections

## Preset Configurations

### Dial-up/Slow Mobile (< 56k)
```bash
govnc.exe -quality=1 -fps=3 -depth=8 -max-res=640
```

### Standard Mobile/DSL (256k-1M)
```bash
govnc.exe -quality=4 -fps=8 -depth=16 -max-res=1024
```

### Broadband (1M-10M)
```bash
govnc.exe -quality=7 -fps=15 -depth=24 -max-res=1280
```

### High-speed (10M+)
```bash
govnc.exe -quality=9 -fps=30 -depth=32 -max-res=1920
```

## VNC Client Connection

Connect to:
- **Host**: `localhost` or your machine's IP address
- **Port**: `6900` (or custom port with `-port` option)
- **Password**: None (no authentication)

## Performance Tips

1. **For slow connections**: Use quality 1-3, 8-bit color, low FPS
2. **For fast connections**: Use quality 8-10, 32-bit color, high FPS
3. **For battery life**: Lower FPS and resolution
4. **For responsiveness**: Higher FPS, accept higher bandwidth usage

## Technical Details

- Uses Windows GDI32 and User32 APIs for screen capture
- Captures primary display only
- Automatic screen resolution detection with configurable limits
- Block-based change detection for bandwidth optimization
- Quality-based color precision adjustment

## Example Output
```
VNC Server Configuration:
  Quality: 7/10
  FPS: 15.0
  Color Depth: 16 bits
  Skip Unchanged: true
  Max Resolution: 1920
  Compression: 6/9
  Port: 6900
Starting VNC server on port 6900, desktop size: 1920x1080
Client connected and requesting framebuffer updates
Detected 3 changed regions
```

## Notes

- This is designed for Windows only
- For production use, consider adding authentication
- Performance scales with screen resolution and update frequency
- The application must be compiled on Windows due to Windows-specific APIs
- Change detection saves significant bandwidth on static desktops 