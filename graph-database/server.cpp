#include "zmq.hpp"
#include "graph.cpp"

#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <time.h>       
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/regex.hpp>
#include <boost/regex.hpp>

/* 
    Cases on type of request and calls the correct handler 
    Always returns a list of strings, the first of which is success/fail
*/

using namespace boost; 
using namespace std; 

const char* msgPartDelimiter = "`";
const char* relationDelimiter = "&";

vector<string> processRequest(DAG& dag, string& request){

    // split the message 
    vector<string> requestParts; 
    boost::algorithm::split_regex(requestParts, request, regex(msgPartDelimiter));

    // handle different request types
    string type = requestParts[0];
    requestParts.erase(requestParts.begin());

    cout << "Processing action: " << type << endl;

    if (type == "GetDescendants") {
        return dag.getAllDescendants(requestParts);
    } else if (type == "GetProperDescendants") {
        return dag.getProperDescendants(requestParts);
    } else if (type == "GetImmediateAncestors") {
        return dag.getImmediateAncestors(requestParts);
    } else if (type == "SetRelations"){
        dag.clearPrevCache(); 
        dag.showGraph("BEFORE SETRELATIONS"); 

        int i = 0; 
        vector<DAG::Vertex> toLocs;  
        while (i < requestParts.size()) {
            // cout << "processing request part: " << requestParts[i] << endl;
            // split the relation
            vector<string> relation;
            boost::split(relation, requestParts[i], boost::is_any_of(relationDelimiter));

            DAG::Vertex toLoc = relation[0]; 
            DAG::VertexSet fromLocs;
            for (int i = 1; i < relation.size(); ++i)
                fromLocs.insert(relation[i]);

            // set the relation
            dag.updateDAG(toLoc, fromLocs);
            toLocs.push_back(toLoc);
            i++;
        }

        for (const auto& tl : toLocs) { 
            if (dag.containsCycle(tl)) {
                return {tl, "CIRC_DEP"};
            }
        }

        dag.showGraph("AFTER SETRELATIONS"); 
        return {"OK"};
    } else if (type == "Clear") {
        dag.clearDAG();
        cout << "DAG cleared.";
        return {"OK"};
    } else if (type == "RollbackGraph") { 
        dag.rollback(); 
        dag.showGraph("AFTER ROLLBACKGRAPH"); 
        return {"OK"};
    } else if (type == "Recompute") {
        dag.clearDAG();
        int computeResult = dag.recomputeDAG();
        cout << "DAG recomputed.";
        if (computeResult == 0)
            return {"OK"};
        else 
            return {"ERROR"};
    }

    return {"UNKNOWN_REQUEST_TYPE"};
}


int main () {

    /* Socket configuration */
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind ("tcp://*:5555");

    /* Variables to help detect start + end of multi-part messages */
    int rcvMore = 0;
    size_t sizeInt = sizeof(int);

    cout << "\nServer started\n";
    /* DAG to be stored in memory */
    DAG dag; 
    int computeResult = dag.recomputeDAG();
    if (computeResult != 0) {
        cout << "Graph DB recomputation failure. Exiting..." << endl << endl;
        return -1;
    }

    while (true) {
        /* Wait for next multi-part message from client */
        zmq::message_t requestMsg;
        clock_t begin = clock(); 
        socket.recv(&requestMsg, rcvMore); // blocks until receives a message

        string request = string(static_cast<char*>(requestMsg.data()), requestMsg.size());
        // removes first and last quotes from string (artifact of ByteString show)
        request = request.substr(1, request.size() - 2); 

        // cout << "Received message: " << request << endl;

        vector<string> response = processRequest(dag,request);

        clock_t end = clock(); 
        cout << "Time taken: " << (double)(end - begin)/CLOCKS_PER_SEC << endl; 

        /* Send response back to client */
        for (int i = 0 ; i < response.size()-1; ++i){
            zmq::message_t res (response[i].length());
            memcpy ((void *) res.data (), response[i].c_str(), response[i].length());
            socket.send (res,ZMQ_SNDMORE);
        } 
        zmq::message_t res (response.back().length());
        memcpy ((void *) res.data (), response.back().c_str(), response.back().length());
        socket.send (res); 

        // cout << "Sent response" << endl;
        // dag.showGraph();
    }
}

