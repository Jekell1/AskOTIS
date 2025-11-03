from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS
import requests
import os

app = Flask(__name__)
CORS(app)

# Azure Search configuration
API_KEY = os.environ.get("AZURE_SEARCH_ADMIN_KEY")
API_BASE_URL = 'https://az-use1-ai-search.search.windows.net'
API_VERSION = '2024-07-01'

@app.route('/')
def serve_demo():
    return send_from_directory('.', 'cobol-search-demo.html')

@app.route('/api/search', methods=['POST'])
def proxy_search():
    try:
        # Get the request data from the frontend
        search_data = request.get_json()
        
        # Make the request to Azure Search
        url = f"{API_BASE_URL}/indexes/cobol-index/docs/search?api-version={API_VERSION}"
        headers = {
            'Content-Type': 'application/json',
            'api-key': API_KEY
        }
        
        response = requests.post(url, json=search_data, headers=headers)
        
        if response.status_code == 200:
            return jsonify(response.json())
        else:
            return jsonify({
                'error': f'Azure Search API returned status {response.status_code}',
                'details': response.text
            }), response.status_code
            
    except Exception as e:
        return jsonify({
            'error': 'Server error',
            'details': str(e)
        }), 500

@app.route('/api/health')
def health_check():
    return jsonify({
        'status': 'healthy',
        'api_base': API_BASE_URL,
        'timestamp': '2025-09-04'
    })

if __name__ == '__main__':
    print("🚀 Starting COBOL Search Demo Server...")
    print("📍 Demo available at: http://localhost:5000")
    print("🔍 API endpoint: http://localhost:5000/api/search")
    app.run(debug=True, port=5000)
