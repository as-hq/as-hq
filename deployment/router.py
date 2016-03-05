from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os
import cgi
import json
import uuid
import random
import json

# this class should be treated as immutable.
# all instance methods return new objects and do not mutate the underlying.
class ASInstance(object):
  def __init__(backend_port, fileinput_port, static_port):
    self.backend_port = backend_port
    self.fileinput_port = fileinput_port
    self.static_port = static_port
    self.name = str(uuid.uuid4())

  # immutable
  def incrementPorts(self):
    return ASInstance(backend_port = self.backend_port + 1, \
                      fileinput_port = self.fileinput_port + 1, \
                      static_port = self.static_port + 1)

  # immutable
  def toJSON(self):
    return {backend_port: self.backend_port, \
            fileinput_port: self.fileinput_port \
            static_port: self.static_port \
            name: self.name}

class Router(BaseHTTPRequestHandler):
  def initInstances(self):
    # track the last created instance (even if it is eventually deleted)
    # so that we never use the same ports twice
    self.last_created_instance = None
    # a mapping from instance ID to instance
    self.instances = {}

  # returns ASInstance
  def createInstance(self):
    if self.last_created_instance is not None:
      newInstance = self.last_created_instance.incrementPorts()
    else:
      newInstance = ASInstance(backend_port=20000, fileinput_port=30000, static_port=40000)

    self.instances[newInstance.name] = newInstance
    self.last_created_instance = newInstance
    return newInstance

  def destroyInstance(self, name): 
    del self.instances[name]

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
    resp = random.choice(self.instances.keys()).toJSON()
    self.send_response(200)
    self.send_header('instance', json.dumps(resp))

  # the route spinup and spindown scripts will use to create and destroy instances
  def do_POST(self):
    print "got post request"
    content_len = int(self.headers.getheader('content-length', 0))
    post_body = self.rfile.read(content_len)

    if post_body['action'] == 'create':
      resp = self.createInstance().toJSON()
      self.send_response(200)
      self.send_header('instance', json.dumps(resp))
      self.end_headers()

    elif post_body['action'] == 'destroy':
      self.destroyInstance(post_body['name'])
      self.send_response(200)

    else:
      self.send_response(400)
      print "ERROR: received action other than create or destroy"