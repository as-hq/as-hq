from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os
import cgi
import json
import uuid
import random
import subprocess
from docker import Client

router_address          = ('0.0.0.0', 10000)

default_backend_port    = 20000
default_fileinput_port  = 30000
default_static_port     = 40000

cli = Client(base_url='unix://var/run/docker.sock')

# this class should be treated as immutable.
# all instance methods return new objects and do not mutate the underlying.
class ASInstance(object):
  def __init__(self, backend_port, fileinput_port, static_port):
    self.backend_port = backend_port
    self.fileinput_port = fileinput_port
    self.static_port = static_port
    self.name = str(uuid.uuid4())

  # immutable
  def incrementPorts(self):
    return ASInstance(backend_port = self.backend_port + 1, \
                      fileinput_port = self.fileinput_port + 1, \
                      static_port = self.static_port + 1)

  def getDockerID(self):
    return cli.inspect_container(self.name)['Id']

  def getStatus(self):
    gen = cli.stats(self.name)
    status = json.loads(next(gen))
    status.update(self.toJSON())
    return status

  def spinup(self):
    # Build up the docker run command
    backendPort = '-p ' + str(self.backend_port) + ':5000 '
    filePort = '-p ' + str(self.fileinput_port) + ':9000 '
    staticPort = '-p ' + str(self.static_port) + ':8000 '
    nameConfig = '--name=' + self.name + ' ' 

    cmd = 'docker run -d ' + \
          backendPort + \
          filePort + \
          staticPort + \
          nameConfig + 'alphasheets/demo'

    # Execute 'docker run'
    subprocess.call([cmd], shell = True)

  def spindown(self):
    cli.stop(self.name)
    cli.remove_container(self.name)

  # immutable
  def toJSON(self):
    return {'backend_port': self.backend_port, \
            'fileinput_port': self.fileinput_port, \
            'static_port': self.static_port, \
            'name': self.name}

class ASRouter(BaseHTTPRequestHandler):
  # track the last created instance (even if it is eventually deleted)
  # so that we never use the same ports twice
  last_created_instance = None
  # a mapping from instance ID to instance
  instances = {}

  # returns ASInstance
  def createInstance(self):
    if ASRouter.last_created_instance is not None:
      newInstance = ASRouter.last_created_instance.incrementPorts()
    else:
      newInstance = ASInstance(backend_port = default_backend_port, \
                               fileinput_port = default_fileinput_port, \
                               static_port = default_static_port)
      ASRouter.last_created_instance = newInstance

    ASRouter.instances[newInstance.name] = newInstance
    ASRouter.last_created_instance = newInstance
    newInstance.spinup()

  def destroyInstance(self, name): 
    if (name in ASRouter.instances):
      ASRouter.instances[name].spindown()
      del ASRouter.instances[name]

  # allow cross-origin from all origins
  def do_OPTIONS(self):
    self.send_response(200, "ok")
    self.send_header('Access-Control-Allow-Origin', '*')
    self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
    self.send_header("Access-Control-Allow-Headers", "X-Requested-With")
    self.send_header("Access-Control-Allow-Headers", "Content-Type")
    self.end_headers()

  # the route clients will use to request a backend instance to connect to 
  def do_GET(self):
    resp = random.choice(self.instances.values()).toJSON()
    self.send_response(200)
    self.send_header('instance', json.dumps(resp))

  # the container spinup and spindown scripts will use to create and destroy instances
  def do_POST(self):
    content_len = int(self.headers.getheader('content-length', 0))
    post_body = json.loads(self.rfile.read(content_len))

    if post_body['action'] == 'create':
      self.createInstance()
      self.sendReply()

    elif post_body['action'] == 'destroy':
      self.destroyInstance(post_body['name'])
      self.sendReply()

    elif post_body['action'] == 'get_status':
      status = ASRouter.instances[post_body['name']].getStatus()
      self.sendReply('status', json.dumps(status))

    elif post_body['action'] == 'get_all_status':
      statuses = [c.getStatus() for c in ASRouter.instances.values()]
      self.sendReply('statuses', json.dumps(statuses))

    else:
      self.send_response(400)
      print "ERROR: received action other than create or destroy"

  def sendReply(self, header=None, value=None):
    self.send_response(200)
    if header != None:
      self.send_header(header, value)
    self.send_header('Access-Control-Allow-Origin', '*')
    self.send_header('Access-Control-Expose-Headers', header)
    self.end_headers()

if __name__ == '__main__':
  httpd = HTTPServer(router_address, ASRouter)
  print('Router running on address: ' + str(router_address))
  httpd.serve_forever()
