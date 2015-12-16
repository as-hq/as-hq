from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
import os
import cgi

# If connected to the server, address = ip address of server
# localhost configs: change serverAddress to localhost
serverAddress = '18.102.225.27'
serverPort = '8080'
fileHandlingPort = 9000

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
    self.send_header('Access-Control-Allow-Origin', 'http://' + serverAddress + ':' + serverPort)
    self.end_headers()
   
def run():
  server_address = (serverAddress, fileHandlingPort)
  httpd = HTTPServer(server_address, FileInputHandler)
  print('http server is running...')
  httpd.serve_forever()
  
if __name__ == '__main__':
  run()