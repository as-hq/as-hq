import requests
import subprocess
import sys

name = sys.argv[0]

# Send a post to a Python server to let it know about a deletion
requests.post('http://localhost:10000', json = {'action': 'destroy', 'name': name})

# Execute the deletion code
subprocess.call(['docker stop ' + name], shell = True)
subprocess.call(['docker rm ' + name], shell = True)



