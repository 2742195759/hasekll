import requests  
import json
  
url = 'http://127.0.0.1:10000'  
headers = {'Content-Type': 'application/json'}  
data = {'command': 'fetch', 'path': '/home/data/work_v100.sh', 'content': ''}  
  
response = requests.post(url, data=json.dumps(data), headers=headers)  
  
if response.status_code == 200:  
    response_data = response.json()  
    print(response_data)  
else:  
    print(f"Error: {response.status_code}")
