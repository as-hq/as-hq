from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os
import cgi
import json

# If connected to the server, address = ip address of server
# localhost configs: change host to localhost
defaultHost = '18.102.225.27'
defaultFrontendPort = 8080
defaultFileHandlingPort = 9000

# Very simple server that saves to the current folder (static)
# Handles file uploads, should also deal with multiple files/parts
class FileInputHandler(BaseHTTPRequestHandler):
  frontendAddress = None # required to be set

  def do_POST(self):
    print "got post request"
    form = cgi.FieldStorage(fp=self.rfile,headers=self.headers,environ={"REQUEST_METHOD":"POST"})
    for item in form.list:
      with open(item.name,'w') as fh:
        fh.write(item.value)
        fh.close()
    # Need to deal with CORS
    self.send_response(200)
    self.send_header('Access-Control-Allow-Origin', self.frontendAddress)
    self.end_headers()
   
if __name__ == '__main__':

  settings = None
  try:
    f = open('../Environment.json', 'r')
    settings = json.loads(f.read())
    f.close()
  except:
    settings = {}

  # below can probably be DRY'd up
  if 'host' in settings: 
    host = settings['host']
  else: 
    print("host not specified in environment file, falling back on default")
    host = defaultHost
  print("Connecting to host " + host + "\n")

  if 'fileInputHandlerPort' in settings:
    fileInputHandlerPort = settings['fileInputHandlerPort']
  else:
    print("fileInputHandlerPort not specified in environment file, falling back on default")
    fileInputHandlerPort = defaultFileHandlingPort
  print("Attaching to port '" + str(fileInputHandlerPort) + "'\n")

  if 'frontendPort' in settings:
    frontendPort = settings['frontendPort']
  else:
    print("frontendPort not specified in environment file, falling back on default")
    frontendPort = defaultFrontendPort
  print("Using frontend port '" + str(frontendPort) + "'\n")
  
  FileInputHandler.frontendAddress = 'http://' + host + ':' + str(defaultFrontendPort)
  server_address = (host, fileInputHandlerPort)
  httpd = HTTPServer(server_address, FileInputHandler)
  print('http server is running on address: ' + str(server_address))
  httpd.serve_forever()