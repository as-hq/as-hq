from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os
import cgi

# If connected to the server, address = ip address of server
# localhost configs: change serverAddress to localhost
frontendHost = 'localhost'
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

  server_address = None
  if 'fileInputHandlerPort' in settings:
    port = settings['fileInputHandlerPort']
    print("Attaching to port '" + port + "' from Environment.json\n")
    server_address = (serverAddress, port)
  else:
    print("No environment specified, falling back on defaults\n")
    server_address = (serverAddress, defaultFileHandlingPort)

  if 'frontendPort' in settings:
    port = settings['frontendPort']
    print("Using frontend port '" + port + "' from Environment.json\n")
    FileInputHandler.frontendAddress = 'http://' + frontendHost + ':' + port
  else:
    print("No environment specified, falling back on defaults\n")
    FileInputHandler.frontendAddress = 'http://' + frontendHost + ':' + defaultFrontendPort

  httpd = HTTPServer(server_address, FileInputHandler)
  print('http server is running...')
  httpd.serve_forever()