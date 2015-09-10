#include <zmq.hpp>
#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>

int main ()
{
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REQ);

    std::cout << "Connecting to hello world server" << std::endl;
    socket.connect ("tcp://localhost:5555");

    //  Do 10 requests, waiting each time for a response
    for (int request_nbr = 0; request_nbr != 100; request_nbr++) {

        for (int j = 0 ; j < 4; ++j){
            zmq::message_t request (5);
            memcpy ((void *) request.data (), "Hello", 5);
            socket.send (request,ZMQ_SNDMORE);
        }
        zmq::message_t request (5);
        memcpy ((void *) request.data (), "Hello", 5);
        socket.send(request);
        std::cout << "Sent hello " << request_nbr << std::endl; 

        //  Get the reply (won't work for multipart)
        zmq::message_t reply;
        socket.recv (&reply);
        std::string rpl = std::string(static_cast<char*>(reply.data()), reply.size());
        std::cout << "Received reply " << rpl << "\t" << request_nbr << std::endl;
    }
    return 0;
}