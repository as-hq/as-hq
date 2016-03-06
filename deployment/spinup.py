import requests
import subprocess
import json

# Send a request to a Python server to get the port mappings
rep = requests.post('http://localhost:10000', json = {'action': 'create'})
instance = json.loads(rep.headers['instance'])

# Build up the docker run command
backendPort = '-p ' + str(instance['backend_port']) + ':80 '
filePort = '-p ' + str(instance['fileinput_port']) + ':9000 '
staticPort = '-p ' + str(instance['static_port']) + ':8000 '
securityConfig = '--cap-add=SYS_PTRACE --security-opt=apparmor:unconfined '
nameConfig = '--name=' + instance['name'] + ' ' 
cmd = 'docker run -d ' + \
      backendPort + \
      filePort + \
      staticPort + \
      nameConfig + \
      securityConfig + 'c283787c2064'

# Execute 'docker run'
subprocess.call([cmd], shell = True)



