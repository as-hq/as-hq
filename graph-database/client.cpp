#include "zmq.hpp"
#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <time.h>    


int main ()
{
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REQ);

    std::cout << "Connecting to server" << std::endl;
    socket.connect ("tcp://localhost:5555");

    
    clock_t begin = clock(); 

    for (int j = 0 ; j < 100000; ++j){
        zmq::message_t request (12);
        memcpy ((void *) request.data (), "SetRelations", 12);
        socket.send (request,ZMQ_SNDMORE);
    }
    zmq::message_t request (5);
    memcpy ((void *) request.data (), "Hello", 5);
    socket.send(request);
    std::cout << "Client finished sending" << std::endl; 

    clock_t end = clock(); 
    std::cout << "Time taken to send from c to s: " << (double)(end - begin)/CLOCKS_PER_SEC <<  std::endl; 

    //  Get the reply (won't work for multipart)
    zmq::message_t reply;
    socket.recv (&reply);
    std::cout << "Client received reply " << std::endl;
    
}