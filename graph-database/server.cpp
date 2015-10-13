#include "zmq.hpp"
#include "graph.cpp"

#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <time.h>       
#include <boost/algorithm/string.hpp>

/* 
    Cases on type of request and calls the correct handler 
    Always returns a list of strings, the first of which is success/fail
*/
std::vector<std::string> processRequest(DAG& dag, std::string& request){

    // split the message 
    std::vector<std::string> requestParts;
    boost::split(requestParts, request, boost::is_any_of("@"));

    // handle different request types
    std::string type = requestParts[0];
    requestParts.erase(requestParts.begin());

    std::cout << "Processing action: " << type << std::endl;

    if (type == "GetDescendants")
        return dag.getDescendants(requestParts);
    else if (type == "GetImmediateAncestors")
        return dag.getImmediateAncestors(requestParts);
    else if (type == "SetRelations"){
        int i = 0 ; 
        while (i < requestParts.size()) {
            // std::cout << "processing request part: " << requestParts[i] << std::endl;
            // split the relation
            std::vector<std::string> relation;
            boost::split(relation, requestParts[i], boost::is_any_of("&"));

            // set the relation
            dag.updateDAG(relation);
            i++;
        }

        std::vector<std::string> empty;
        return empty;
    } else if (type == "Clear") {
        dag.clearDAG();
        std::cout << "DAG cleared.";
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

    /* DAG to be stored in memory */
    DAG dag; 

    while (true) {

        /* Wait for next multi-part message from client */
        zmq::message_t requestMsg;
        clock_t begin = clock(); 
        socket.recv(&requestMsg, rcvMore); // blocks until receives a message

        std::string request = std::string(static_cast<char*>(requestMsg.data()), requestMsg.size());
        // removes first and last quotes from string (artifact of ByteString show)
        request = request.substr(1, request.size() - 2); 

        // std::cout << "Received message: " << request << std::endl;

        std::vector<std::string> response = processRequest(dag,request);
        response.push_back("OK"); //TODO: error handling

        clock_t end = clock(); 
        std::cout << "Time taken: " << (double)(end - begin)/CLOCKS_PER_SEC << std::endl; 

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

