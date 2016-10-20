from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os
import cgi
import json

# Very simple server that saves to the current folder (static)
# Handles file uploads, should also deal with multiple files/parts
class FileInputHandler(BaseHTTPRequestHandler):

  def do_POST(self):
    print "got post request"
    form = cgi.FieldStorage(fp=self.rfile,headers=self.headers,environ={"REQUEST_METHOD":"POST"})
    for item in form.list:
      with open(item.name,'w') as fh:
        fh.write(item.value)
        fh.close()
    # Need to deal with CORS
    self.send_response(200)
    self.send_header('Access-Control-Allow-Origin', '*')
    self.end_headers()
   
if __name__ == '__main__':

  f = open('./Environment.json', 'r')
  settings = json.loads(f.read())
  f.close()

  host = settings['host']
  print("Connecting to host " + host + "\n")

  fileInputHandlerPort = settings['fileInputHandlerPort']
  print("Attaching to port '" + str(fileInputHandlerPort) + "'\n")
  
  server_address = (host, fileInputHandlerPort)
  httpd = HTTPServer(server_address, FileInputHandler)
  print('http server is running on address: ' + str(server_address))
  httpd.serve_forever()
