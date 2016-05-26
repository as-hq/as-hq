from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import requests
import random
import json

INSTANCE_ROUTER_PORT  = 11000
router_address        = ('0.0.0.0', 12000)

class ASMasterRouter(BaseHTTPRequestHandler):
  hosts = []
  # allow cross-origin from all origins
  def do_OPTIONS(self):
    self.send_response(200, "ok")
    self.send_header('Access-Control-Allow-Origin', '*')
    self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
    self.send_header("Access-Control-Allow-Headers", "X-Requested-With")
    self.send_header("Access-Control-Allow-Headers", "Content-Type")
    self.end_headers()
  
  #  get an instance to connect to
  def do_GET(self):
    host = random.choice(ASMasterRouter.hosts)
    try:
      r = requests.get('http://' + host + ':' + str(INSTANCE_ROUTER_PORT)) 
      if (r.status_code == 200):
        print 'SUCCESS! got an instance from a router.'
        instance = json.loads(r.content)
        instance['host'] = host
        self.sendContent(200, json.dumps(instance))
      else:
        print 'GET instance router failed'
        self.send_response(500, 'Received error from instance router.')
    except e:
      print 'GET instance router failed'
      self.send_response(500, 'Could not reach instance router')

  def do_POST(self):
    content_len = int(self.headers.getheader('content-length', 0))
    post_body = json.loads(self.rfile.read(content_len))

    if post_body['action'] == 'get_all_hosts':
      self.sendContent(200, json.dumps(ASMasterRouter.hosts)) 

  def sendContent(self, status, content):
    self.send_response(status)
    self.send_header('Access-Control-Allow-Origin', '*')
    self.end_headers()
    self.wfile.write(content) 

if __name__ == '__main__':
  with open('./hosts.txt', 'r') as f:
    ASMasterRouter.hosts = f.read().splitlines()
  httpd = HTTPServer(router_address, ASMasterRouter)
  print('Router running on address: ' + str(router_address))
  httpd.serve_forever()
