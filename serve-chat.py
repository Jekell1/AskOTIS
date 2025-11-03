"""
Simple HTTP server to serve the OTIS RAG chat interface.
Run this to avoid CORS issues when testing locally.
"""
import http.server
import socketserver
import webbrowser
import os

PORT = 8000
DIRECTORY = os.path.dirname(os.path.abspath(__file__))

class MyHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=DIRECTORY, **kwargs)
    
    def end_headers(self):
        # Add CORS headers
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', 'Content-Type')
        super().end_headers()

if __name__ == "__main__":
    with socketserver.TCPServer(("", PORT), MyHTTPRequestHandler) as httpd:
        url = f"http://localhost:{PORT}/otis-rag-chat.html"
        print(f"🚀 OTIS RAG Chat Server")
        print(f"=" * 50)
        print(f"Server running at: {url}")
        print(f"Press Ctrl+C to stop")
        print(f"=" * 50)
        
        # Open browser
        webbrowser.open(url)
        
        # Start server
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n\n👋 Server stopped")
