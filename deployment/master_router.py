from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import requests
import random
import json

HOSTS = []
router_address = ('0.0.0.0', 10000)

class ASMasterRouter(BaseHTTPRequestHandler):
  #  get an instance to connect to
  def do_GET(self):
    host = random.choice(HOSTS)
    r = requests.get('http://' + host + ':10000') 
    if (r.status_code == 200):
      print 'SUCCESS! got an instance from a router.'
      self.sendContent(200, r.content)
    else:
      print 'FUCK! your get fucked up.'
      self.send_response(500, 'Received error from instance router.')

  def do_POST(self):
    content_len = int(self.headers.getheader('content-length', 0))
    post_body = json.loads(self.rfile.read(content_len))

    if post_body['action'] == 'get_hosts':
      self.sendContent(200, json.dumps(HOSTS)) 

  def sendContent(self, status, content):
    self.send_response(status)
    self.send_header('Access-Control-Allow-Origin', '*')
    self.end_headers()
    self.wfile.write(content) 

if __name__ == '__main__':
  httpd = HTTPServer(router_address, ASMasterRouter)
  print('Router running on address: ' + str(router_address))
  httpd.serve_forever()
