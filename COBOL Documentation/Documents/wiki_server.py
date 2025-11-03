#!/usr/bin/env python3
"""
COBOL Documentation Wiki Server
Provides a local HTTP server for the COBOL documentation wiki to avoid CORS restrictions.
"""

import http.server
import socketserver
import webbrowser
import os
import sys
import threading
import time
from pathlib import Path

# Configuration
DEFAULT_PORT = 8080
DEFAULT_HOST = "localhost"

class WikiHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    """Custom HTTP request handler for the COBOL wiki."""
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=os.path.dirname(os.path.abspath(__file__)), **kwargs)
    
    def end_headers(self):
        # Add CORS headers
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        
        # Prevent caching for development
        self.send_header('Cache-Control', 'no-cache, no-store, must-revalidate')
        self.send_header('Pragma', 'no-cache')
        self.send_header('Expires', '0')
        
        super().end_headers()
    
    def do_OPTIONS(self):
        """Handle OPTIONS requests for CORS preflight."""
        self.send_response(200)
        self.end_headers()
    
    def log_message(self, format, *args):
        """Custom log message format."""
        timestamp = time.strftime('%Y-%m-%d %H:%M:%S')
        print(f"[{timestamp}] {format % args}")

def find_free_port(start_port=DEFAULT_PORT, max_attempts=10):
    """Find a free port starting from the given port."""
    import socket
    
    for port in range(start_port, start_port + max_attempts):
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.bind(('', port))
                return port
        except OSError:
            continue
    
    raise OSError(f"Could not find a free port in range {start_port}-{start_port + max_attempts}")

def start_server(port=None, host=DEFAULT_HOST, auto_open=True):
    """Start the wiki server."""
    
    # Ensure we're in the right directory
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(script_dir)
    
    # Find a free port if none specified
    if port is None:
        try:
            port = find_free_port()
        except OSError as e:
            print(f"❌ Error finding free port: {e}")
            return False
    
    # Check if wiki_reader.html exists
    if not os.path.exists('wiki_reader.html'):
        print("❌ Error: wiki_reader.html not found in current directory")
        print(f"   Current directory: {os.getcwd()}")
        print("   Make sure you're running this script from the Documents directory")
        return False
    
    try:
        # Create and start server
        with socketserver.TCPServer((host, port), WikiHTTPRequestHandler) as httpd:
            server_url = f"http://{host}:{port}/wiki_reader.html"
            
            print("=" * 60)
            print("🚀 COBOL Documentation Wiki Server")
            print("=" * 60)
            print(f"📂 Serving directory: {script_dir}")
            print(f"🌐 Server address:    {server_url}")
            print(f"🔌 Port:              {port}")
            print("=" * 60)
            print("📋 Available endpoints:")
            print(f"   • Wiki Reader:     {server_url}")
            print(f"   • Documentation:   http://{host}:{port}/*.md")
            print(f"   • Diagrams:        http://{host}:{port}/Diagrams/*.mmd")
            print("=" * 60)
            print("💡 Tips:")
            print("   • Press Ctrl+C to stop the server")
            print("   • Server automatically handles CORS and caching")
            print("   • All Markdown and Mermaid files are accessible")
            print("=" * 60)
            
            # Open browser if requested
            if auto_open:
                def open_browser():
                    time.sleep(1)  # Give server time to start
                    print(f"🌐 Opening browser to: {server_url}")
                    webbrowser.open(server_url)
                
                browser_thread = threading.Thread(target=open_browser)
                browser_thread.daemon = True
                browser_thread.start()
            
            print(f"✅ Server running at: {server_url}")
            print("   (Server started - waiting for requests...)")
            print()
            
            # Serve forever
            httpd.serve_forever()
            
    except KeyboardInterrupt:
        print("\n" + "=" * 60)
        print("🛑 Server stopped by user (Ctrl+C)")
        print("=" * 60)
        return True
        
    except OSError as e:
        if "Address already in use" in str(e):
            print(f"❌ Error: Port {port} is already in use")
            print(f"   Try a different port: python {sys.argv[0]} --port {port + 1}")
        else:
            print(f"❌ Server error: {e}")
        return False
        
    except Exception as e:
        print(f"❌ Unexpected error: {e}")
        return False

def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Start the COBOL Documentation Wiki Server",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python wiki_server.py                    # Start on default port (8080)
  python wiki_server.py --port 8090        # Start on port 8090
  python wiki_server.py --no-browser       # Start without opening browser
  python wiki_server.py --host 0.0.0.0     # Allow external connections
        """
    )
    
    parser.add_argument(
        '--port', '-p', 
        type=int, 
        default=None,
        help=f'Port to run server on (default: auto-detect starting from {DEFAULT_PORT})'
    )
    
    parser.add_argument(
        '--host', 
        default=DEFAULT_HOST,
        help=f'Host to bind to (default: {DEFAULT_HOST})'
    )
    
    parser.add_argument(
        '--no-browser', 
        action='store_true',
        help='Do not automatically open browser'
    )
    
    args = parser.parse_args()
    
    # Start server
    success = start_server(
        port=args.port,
        host=args.host,
        auto_open=not args.no_browser
    )
    
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
