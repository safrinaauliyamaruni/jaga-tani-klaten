"""Server lokal sederhana. Jalankan:  python serve.py   lalu buka http://localhost:8000"""
import http.server, socketserver, webbrowser, os
os.chdir(os.path.dirname(os.path.abspath(__file__)))
PORT = 8000
class H(http.server.SimpleHTTPRequestHandler):
    extensions_map = {**http.server.SimpleHTTPRequestHandler.extensions_map,
                      '.shp':'application/octet-stream','.dbf':'application/octet-stream','.prj':'text/plain','.csv':'text/csv'}
    def end_headers(self):
        self.send_header('Cache-Control','no-store'); super().end_headers()
socketserver.TCPServer.allow_reuse_address = True
with socketserver.ThreadingTCPServer(('', PORT), H) as s:
    print(f'Server jalan di http://localhost:{PORT}  (Ctrl+C untuk berhenti)')
    webbrowser.open(f'http://localhost:{PORT}/index.html'); s.serve_forever()
