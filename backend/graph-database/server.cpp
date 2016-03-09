#include "zmq.h"
#include "graph.h"
#include "location.h"

#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <time.h>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/regex.hpp>
#include <boost/regex.hpp>

#include <map>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

using namespace boost; 
using namespace std; 
using boost::property_tree::ptree;
using boost::property_tree::read_json;

/***********************************************************************************************************************/
/* Communication with graph methods, involves show and read methods mainly */

const char* msgPartDelimiter = "`";
const char* relationDelimiter = "&";

/*
    For functions like getDescendants, given the requestParts and an empty location vector,
    simply cast to Location type for the function
*/
vector<Location>& generateLocs(const vector<string>& requestParts, vector<Location>& locs) {
    locs.resize(requestParts.size()); 
    transform(requestParts.begin(),requestParts.end(),locs.begin(),fromString);
    return locs;
}

/*
    Stringify a DAG Status to send as last part of message back 
*/
string stringifyStatus(const DAG::DAGStatus& status){
    switch(status){
        case DAG::DAGStatus::OK:
            return "OK";
            break;
        case DAG::DAGStatus::CIRC_DEP:
            return "CIRC_DEP";
            break;
        case DAG::DAGStatus::ERROR:
            return "ERROR";
            break;
        case DAG::DAGStatus::UNKNOWN_REQUEST_TYPE:
            return "UNKNOWN_REQUEST_TYPE";
            break;
    }
}

/*
    Take a DAG response and an empty string of responses, 
    Return a list of strings to send back to  Haskell
    First n-1 are just location strings, last string is status
*/
vector<string> stringifyResponse(const DAG::DAGResponse& resp, vector<string>& response) {
    for (const auto& loc: resp.locs) {
        response.push_back(toString(loc));
    }
    response.push_back(stringifyStatus(resp.status));
    return response;
}

/*
    Given all of the request parts, split them by relation delimiter and set all of those relations
    (We can set multiple relations at once in the DB, useful for eval, delete, etc. that take [ASCell])
    Can possibly result in circular error and its head (any vertex of the cycle)
*/
DAG::DAGResponse applySetRelations(DAG& dag, const vector<string>& requestParts) {
    int i = 0; 
    vector<DAG::Vertex> toLocs; 
    while (i < requestParts.size()) {
        // get the next relation by splitting
        vector<string> relation;
        boost::split(relation, requestParts[i], boost::is_any_of(relationDelimiter));
        // get the to and from locs
        DAG::Vertex toLoc = fromString(relation[0]); 
        DAG::VertexSet fromLocs;
        for (int i = 1; i < relation.size(); ++i) {
            fromLocs.insert(fromString(relation[i]));
        }
        // set the relation
        dag.updateDAG(toLoc, fromLocs);
        toLocs.push_back(toLoc);
        i++;
    }
    
    vector<Location> responseLocs = {};

    int ind = dag.indexOfFirstVertexInCycle(toLocs);
    if (ind != -1) { 
        responseLocs.push_back(toLocs[ind]);
        return {responseLocs, DAG::DAGStatus::CIRC_DEP};
    }

    return {responseLocs, DAG::DAGStatus::OK};
}

/***********************************************************************************************************************/

/* 
    Returns the vector of strings that's the response to Haskell, given the current dag and the overall request 
    Main non-ZMQ logic within the server is here 
*/
vector<string> processRequest(DAG& dag, string& request) {
    // Split message by message delimiter 
    vector<string> requestParts; 
    boost::algorithm::split_regex(requestParts, request, regex(msgPartDelimiter));
    string type = requestParts[0];
    requestParts.erase(requestParts.begin());

    cout << "Processing action: " << type << endl;
    vector<Location> tempLocs;
    vector<string> response;

    if (type == "GetDescendants") {
        DAG::DAGResponse r = dag.getEntireRootedSubgraph(generateLocs(requestParts, tempLocs), true);
        return stringifyResponse(r,response);
    } else if (type == "GetProperDescendants") {
        DAG::DAGResponse r = dag.getProperDescendants(generateLocs(requestParts, tempLocs));
        return stringifyResponse(r,response);
    } else if (type == "GetAllAncestors") {
        DAG::DAGResponse r = dag.getEntireRootedSubgraph(generateLocs(requestParts, tempLocs), false);
        return stringifyResponse(r, response);
    } else if (type == "GetImmediateAncestors") {
        DAG::DAGResponse r = dag.getImmediateAncestors(generateLocs(requestParts, tempLocs));
        return stringifyResponse(r,response);
    } else if (type == "SetRelations") {
        DAG::DAGResponse r = applySetRelations(dag, requestParts);
        return stringifyResponse(r, response);
    } else if (type == "Clear") {
        dag.clearDAG();
        return {stringifyStatus(DAG::DAGStatus::OK)};
    } else if (type == "ClearSheet") {
        dag.clearSheetDAG(requestParts[0]);
        return {stringifyStatus(DAG::DAGStatus::OK)};
    } else {
        return {stringifyStatus(DAG::DAGStatus::UNKNOWN_REQUEST_TYPE)};
    }
}


int main () {

    /* Reading settings from Environment.js */
    string addr;
    try {
        ptree json;
        read_json("../Environment.json", json);
        addr = json.get("graphDbAddress_cpp", "tcp://*:5555"); // specifying default in case we don't find it
        cout << "Found address in environment: " << addr << endl;
    } catch (...) {
        cout << "exception reading Environment.json, falling back on defaults" << endl;
        addr = "tcp://*:5555";
    }

    /* Socket configuration */
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind (addr);

    /* Variables to help detect start + end of multi-part messages */
    int rcvMore = 0;
    size_t sizeInt = sizeof(int);

    cout << "\nServer started\n";
    /* DAG to be stored in memory */
    DAG dag; 
    // int computeResult = dag.recomputeDAG();
    // if (computeResult != 0) {
    //     cout << "Graph DB recomputation failure. Exiting..." << endl << endl;
    //     return -1;
    // }

    while (true) {
        /* Wait for next multi-part message from client */
        zmq::message_t requestMsg;
        clock_t begin = clock(); 
        socket.recv(&requestMsg, rcvMore); // blocks until receives a message

        string request = string(static_cast<char*>(requestMsg.data()), requestMsg.size());
        // removes first and last quotes from string (artifact of ByteString show)
        request = request.substr(1, request.size() - 2); 
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
        /* ^ Sent back request */
    }
}
