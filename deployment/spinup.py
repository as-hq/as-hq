import requests
import subprocess

# Send a request to a Python server to get the port mappings
info = requests.post('http://localhost:81', data = {'action': 'create'})

# Build up the docker run command
backendPort = '-p ' + info.backend_port + ':80 '
filePort = '-p ' + info.fileinput_port + ':9000 '
staticPort = '-p ' + info.static_port + ':8000 '
securityConfig = '--cap-add=SYS_PTRACE --security-opt=apparmor:unconfined '
nameConfig = '--name=' + info.identifier + ' ' 
cmd = 'docker run -d ' + backendPort + filePort + staticPort + nameConfig + securityConfig + '13ed4293b8ba'

# Execute 'docker run'
subprocess.call([cmd], shell = True)



