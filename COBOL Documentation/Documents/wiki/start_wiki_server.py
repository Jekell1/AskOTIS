#!/usr/bin/env python3
"""
Simple HTTP Server for COBOL Documentation Wiki
This server allows the wiki to load files without browser security restrictions.
"""

import http.server
import socketserver
import webbrowser
import os
import sys
from pathlib import Path

# Configuration
PORT = 8080
WIKI_DIR = Path(__file__).parent

class WikiHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    """Custom handler that adds CORS headers and better error handling"""
    
    def end_headers(self):
        # Add CORS headers to allow fetch requests
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        super().end_headers()
    
    def do_OPTIONS(self):
        # Handle preflight requests
        self.send_response(200)
        self.end_headers()
    
    def log_message(self, format, *args):
        # Custom logging format
        print(f"[{self.date_time_string()}] {format % args}")

def find_free_port(start_port=8080):
    """Find a free port starting from the given port number"""
    import socket
    port = start_port
    while port < start_port + 100:
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.bind(('localhost', port))
                return port
        except OSError:
            port += 1
    return None

def main():
    # Change to the wiki directory
    os.chdir(WIKI_DIR)
    
    print("🚀 COBOL Documentation Wiki Server")
    print("=" * 40)
    
    # Check if wiki files exist
    required_files = [
        'wiki_reader.html',
        'APIPAY_Documentation.html',
        'LONPFB_Documentation.html'
    ]
    
    missing_files = []
    for file in required_files:
        if not Path(file).exists():
            missing_files.append(file)
    
    if missing_files:
        print("⚠️  Warning: Some files are missing:")
        for file in missing_files:
            print(f"   - {file}")
        print()
    
    # Find a free port
    port = find_free_port(PORT)
    if not port:
        print("❌ Could not find a free port. Try closing other applications.")
        sys.exit(1)
    
    try:
        # Create and start the server
        with socketserver.TCPServer(("localhost", port), WikiHTTPRequestHandler) as httpd:
            print(f"📡 Server starting on http://localhost:{port}")
            print(f"📂 Serving files from: {WIKI_DIR}")
            print(f"🌐 Wiki URL: http://localhost:{port}/wiki_reader.html")
            print()
            print("🔧 Server Features:")
            print("   ✅ CORS headers enabled for fetch requests")
            print("   ✅ All wiki files accessible")
            print("   ✅ Diagrams and documentation supported")
            print()
            print("Press Ctrl+C to stop the server")
            print("-" * 40)
            
            # Try to open the wiki in the default browser
            try:
                webbrowser.open(f'http://localhost:{port}/wiki_reader.html')
                print("🚀 Opening wiki in your default browser...")
            except Exception as e:
                print(f"⚠️  Could not auto-open browser: {e}")
                print(f"   Please manually open: http://localhost:{port}/wiki_reader.html")
            
            print()
            
            # Start serving
            httpd.serve_forever()
            
    except KeyboardInterrupt:
        print("\n\n👋 Server stopped by user")
    except Exception as e:
        print(f"❌ Server error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
