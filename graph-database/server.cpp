#include "zmq.hpp"
#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include "graph.cpp"

/* 
    Cases on type of request and calls the correct handler 
    Always returns a list of strings, the first of which is success/fail
*/
std::vector<std::string> processRequest(DAG& dag, std::vector<std::string>& request){
    std::string type = request[0];
    request.erase(request.begin());
    if (type == "GetDescendants")
        return dag.getDescendants(request);
    else if (type == "GetImmediateAncestors")
        return dag.getImmediateAncestors(request);
    else if (type == "SetRelations"){
        int i = 0 ; 
        while (i < request.size()) {
            std::string toLoc = request[i];
            std::vector<std::string> fromLocs; 
            i++; 
            while (request[i] != "|" && i < request.size()){
                fromLocs.push_back(request[i]);
                i++; 
            }
            dag.updateDAG(toLoc,fromLocs);
        }

        std::vector<std::string> empty;
        return empty;
    }
}


int main () {

    /* Socket configuration */
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind ("tcp://*:5555");

    /* Variables to help detect start + end of multi-part messages */
    int rcvMore = 0;
    size_t sizeInt = sizeof(int);
    bool bRcvMore = true;

    /* DAG to be stored in memory */
    DAG dag; 

    while (true) {

        /* Wait for next multi-part message from client */
        std::vector<std::string> request;
        zmq::message_t requestPart;
        socket.recv(&requestPart, rcvMore); // blocks until receives a message
        std::string requestPartStr = std::string(static_cast<char*>(requestPart.data()), requestPart.size());
        // std::cout << "server got " << requestPartStr << std::endl;
        request.push_back(requestPartStr);
        socket.getsockopt(ZMQ_RCVMORE, &rcvMore, &sizeInt);
        bRcvMore = (rcvMore == 1);

        /* Continue appending to request if message has rcvmore tag */
        while(bRcvMore){
            zmq::message_t msg;
            socket.recv(&msg, rcvMore);
            std::string msgStr = std::string(static_cast<char*>(msg.data()), msg.size());
            // std::cout << "server got " << msgStr << std::endl;
            request.push_back(msgStr);
            socket.getsockopt(ZMQ_RCVMORE, &rcvMore, &sizeInt);
            bRcvMore = (rcvMore == 1);
        }
        // std::cout << "Received message" << std::endl;

        std::vector<std::string> response = processRequest(dag,request);
        response.push_back("OK"); //TODO: error handling

        /* Send response back to client */
        for (int i = 0 ; i < response.size()-1; ++i){
            zmq::message_t res (response[i].length());
            memcpy ((void *) res.data (), response[i].c_str(), response[i].length());
            socket.send (res,ZMQ_SNDMORE);
        } 
        zmq::message_t res (response.back().length());
        memcpy ((void *) res.data (), response.back().c_str(), response.back().length());
        socket.send (res); 

        // std::cout << "Sent response" << std::endl;
        // dag.showGraph();



    }
}

