#include "graph.hpp"
#include <algorithm>
#include <hiredis/hiredis.h>
#include <string>
#include <vector>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/regex.hpp>
#include <math.h>
#include <ctype.h>

#define REDIS_HOST "localhost"
#define REDIS_PORT 6379
#define SCAN_BOUND 100000
#define INDEX_REGEX "\\$?[A-Za-z]+\\$?[0-9]+"
#define RANGE_REGEX "\\$?[A-Za-z]+\\$?[0-9]+:\\$?[A-Za-z]+\\$?[0-9]+"
#define AS_INDEX_REGEX "I\\/[^\\/]+\\/\\([0-9]+,[0-9]+\\)"
#define REF_PART_REGEX "\\/"
#define REF_PART "/"

using namespace std; 
using namespace boost; 


/****************************************************************************************************************************************/


void DAG::rollback() {
	for (const auto& fan : prevCache)
		updateDAG(fan.first, fan.second, false);
}

bool DAG::clearPrevCache() {
	prevCache.clear();
}

void DAG::clearDAG() {
	toFromAdjList.clear();
	fromToAdjList.clear();
	clearPrevCache();
}

/****************************************************************************************************************************************/

/*
  every time updateDAG is called, prevCache is updated with what the original graph
  contained. If the cache is empty and updateDAG() is called a number of times in a row 
  and then rollback() is called, all the updateDAG()'s get reverted. clearPrevCache() is 
  called at the start of each processRequest of type SetRelations, during which updateDAG() 
  is called numerous times in succession. 

  Also, there's actually a way of implementing the rollback entirely in Haskell 
  that's much cleaner than this, but this is faster (one less communication) and it's already 
  implemented, so whatever. (To be precise, setCellsAncestorsInDbForce on the new cells with 
  expressions replaced with "" and setCellsAncestorsInDb with the old cells in the DB.)
*/

bool DAG::cycleCheckDfs(const Vertex& loc, unordered_map<Vertex,bool>& visited, unordered_map<Vertex,bool>& rec_stack) { 
	if (!visited[loc]) {
		visited[loc] = true; 
		rec_stack[loc] = true; 

		if (fromToAdjList.count(loc)) { // so that the fromToAdjList key isn't created if there's nothing there
			for (const auto& toLoc : fromToAdjList[loc]) {
				if (!visited[toLoc] && DAG::cycleCheckDfs(toLoc, visited, rec_stack)) {
					return true; 
				}
				else if (rec_stack[toLoc]) {
					return true; 
				}
			}
		}
	}
	rec_stack[loc] = false; 
	return false; 
}

bool DAG::containsCycle(const DAG::Vertex& start) { 
	unordered_map<DAG::Vertex,bool> visited, rec_stack; 
	return cycleCheckDfs(start, visited, rec_stack); 
}


/****************************************************************************************************************************************/

void DAG::updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs, bool addToCache) {
	DAG::VertexSet vl = toFromAdjList[toLoc]; //old fromLocs

	// If a vertex gets updated multiple times, only record the first update. 
	if (addToCache) {
		if (prevCache.count(toLoc) == 0)
			prevCache[toLoc] = toFromAdjList[toLoc];
	}
	
	/* Loop over the current fromLocs of toLoc and delete from forward adjacency list */
	for (const auto& oldFl : vl){
		fromToAdjList[oldFl].erase(toLoc);

		// If a vertex no longer has fromLocs, delete it
		if (fromToAdjList[oldFl].empty())
			fromToAdjList.erase(oldFl);
	}
	toFromAdjList.erase(toLoc); 

	/* Loop over the new fromLocs and add to the forward adjacency list */
	for (const auto& fl : fromLocs) {
		toFromAdjList[toLoc].insert(fl);
		// if Pointer reference, put an Index ref in the forward list, and put the Pointer ref in the backward list.
		// then evalChain' triggers the recomputation of an expression with an @ reference in it correctly
		// using getDescendants, then uses getAncestors on those descendants to get the actual Pointer reference.
		if (fl[0] == 'P') {
			DAG::Vertex flIndex = fl;
			flIndex[0] = 'I'; 
			fromToAdjList[flIndex].insert(toLoc);
		} else {
			fromToAdjList[fl].insert(toLoc);
		}
	/* Replace toLoc entry in backwards adjacency list */
	}

}

void DAG::updateDAGDfs (const DAG::Vertex& loc, unordered_map<DAG::Vertex,bool>& visited, vector<DAG::Vertex>& order){
	if (fromToAdjList.count(loc)) { // so that the fromToAdjList key isn't created if there's nothing there
		for (const auto& toLoc : fromToAdjList[loc]){
			if (!visited[toLoc]) {
				DAG::updateDAGDfs(toLoc,visited,order);
			}
		}
	}
	order.push_back(loc);
	visited[loc] = true; 
}

int DAG::recomputeDAG () {
	cout << "Rebuilding DAG..." << endl;
	redisContext *c = redisConnect(REDIS_HOST, REDIS_PORT);
	redisReply *reply;
	if (c->err) {
    	cerr << "Connection error: " << c->errstr << endl;
    	return -1;
	}

	string redisCmd = "scan 0 match I/* count " + to_string(SCAN_BOUND);
	reply = (redisReply *)redisCommand(c, redisCmd.c_str());

	int numReplies=0; 

	if (reply->type == REDIS_REPLY_ARRAY && reply->element[1]->type == REDIS_REPLY_ARRAY) {

		for (int i = 0; i < reply->element[1]->elements; i++) {
			if (reply->element[1]->element[i]->type == REDIS_REPLY_STRING) {
				numReplies = reply->element[1]->elements;
				string key = reply->element[1]->element[i]->str;
				string redisGetCmd = "get " + key;
				redisAppendCommand(c, redisGetCmd.c_str());
			}
		}

  	freeReplyObject(reply);

		for (int i=0; i<numReplies; i++) {
			redisGetReply(c, (void **)&reply);
			if (reply->type == REDIS_REPLY_STRING) {
				DAG::Vertex toNode = getIndexFromCell(reply->str);
				DAG::VertexSet fromNodes = parseDependencies(reply->str, toNode);
				updateDAG(toNode, fromNodes, false); // no caching on reconstruction
			}
			freeReplyObject(reply);
		}
		cout << "Done.\n" << endl;
		return 0;
  } else {
  	cout << "Error: received Redis reply of type other than array\n" << endl;
  	freeReplyObject(reply);
    	return -1;
  }
}

/****************************************************************************************************************************************/

// Given a list of cells, return all of their descendants in the DAG, sorted topologically. 
//(X is a proper descendant of Y if there's a path of length >= 1 from X to Y.)
vector<DAG::Vertex> DAG::getAllDescendants(const vector<DAG::Vertex>& locs){
	unordered_map<DAG::Vertex,bool> visited;
	vector<DAG::Vertex> order; 

	for (const auto& loc: locs) {
		visited[loc] = false;
	}

	for (const auto& loc: locs) {
		if (!visited[loc]){
			DAG::updateDAGDfs(loc, visited, order);
		}
	}

	reverse(order.begin(),order.end());
	order.push_back("OK");
	return order;
}

vector<DAG::Vertex> DAG::getProperDescendants(const vector<DAG::Vertex>& locs){
	unordered_map<DAG::Vertex,bool> visited;
	vector<DAG::Vertex> order; 

	for (const auto& loc: locs) {
		visited[loc] = false;
	}

	for (const auto& loc: locs) {
		if (!visited[loc]){
			if (fromToAdjList.count(loc)) { // so that the fromToAdjList key isn't created if there's nothing there
				for (const auto& toLoc : fromToAdjList[loc]){
					if (!visited[toLoc]) {
						DAG::updateDAGDfs(toLoc,visited,order);
					}
				}
			}
			visited[loc] = true; 
		}
	}

	reverse(order.begin(),order.end());
	order.push_back("OK");
	return order;
}


/****************************************************************************************************************************************/

vector<DAG::Vertex> DAG::getImmediateAncestors(const vector<DAG::Vertex>& locs){
	// cout << "in dag get immediate ancestors " << endl; 
	unordered_set<DAG::Vertex> ancestors;
	for (const auto& loc : locs){
		if (toFromAdjList.count(loc)) { // so that the toFromAdjList key isn't created if there's nothing there
			for (const auto& anc: toFromAdjList[loc])
				ancestors.insert(anc);
		}
	}
	// cout << "filled up ancestors" << endl; 
	vector<DAG::Vertex> vAncestors(ancestors.begin(),ancestors.end());
	vAncestors.push_back("OK");
	return vAncestors;
}

bool DAG::operator==(const DAG& rhs) {
	return (toFromAdjList == rhs.toFromAdjList) && (fromToAdjList == rhs.fromToAdjList);
}

/****************************************************************************************************************************************/
// Parsing utilities

DAG::VertexSet parseDependencies(const string& str, const DAG::Vertex& fromLoc) {
	vector<string> rangeMatches = regexAll(str, RANGE_REGEX);
	string rangesRemoved = regex_replace(str, regex(RANGE_REGEX), "");
	vector<string> indexMatches = regexAll(rangesRemoved, INDEX_REGEX);

	indexMatches.insert(indexMatches.end(), rangeMatches.begin(), rangeMatches.end());
	// cout << "index matches: " << endl;
  	// printVec(indexMatches);
	vector<vector<int>> coordinates;
	transform(indexMatches.begin(), indexMatches.end(), back_inserter(coordinates), exRefToCoordinate);
	// cout << "coordinates: " << endl;
  	// printVec2(coordinates);
	string sheetId = getSheetIdFromIndex(fromLoc);
	return constructASIndices(sheetId, coordinates);
}

DAG::VertexSet constructASIndices(const string& sheetId, vector<vector<int>>& coords) {
	DAG::VertexSet idxs;
	for (const auto& coord : coords) {
		if (coord.size() == 2) {
			idxs.insert(makeASIndex(sheetId, coord[0], coord[1]));
		} else {
			vector<vector<int>> rngCoords;
			rngCoords = decomposeRange(coord);
			for (const auto& rngCoord : rngCoords)
				idxs.insert(makeASIndex(sheetId, rngCoord[0], rngCoord[1]));
		}
	}
	return idxs;
}

string makeASIndex(const string& sheetId, const int& col, const int& row) {
	return "I/" + sheetId + "/(" + to_string(col) + "," + to_string(row) + ")";
}

vector<int> exRefToCoordinate(string& ref) {
	vector<string> corners;
	boost::split(corners, ref, boost::is_any_of(":"));
	if (corners.size() == 2) {
		vector<int> tl = exIndexToCoordinate(corners[0]);
		vector<int> br = exIndexToCoordinate(corners[1]);
		tl.insert(tl.end(), br.begin(), br.end());
		return tl;
	} else {
		return exIndexToCoordinate(ref);
	}
}

vector<int> exIndexToCoordinate(string& ref) {
	int i = (ref[0] == '$') ? 1 : 0; //skip a doller
	int start = i;
	while (isalpha(ref[i])) i++;
	string col = ref.substr(start, i-start);

	i = (ref[i-start+1] == '$') ? i+1 : i; // skip anothar doller
	string row = ref.substr(i);
	return {exColToInt(col), stoi(row)};
}

vector<vector<int>> decomposeRange(const vector<int>& rng) {
	int area = (rng[2] - rng[0] + 1) * (rng[3] - rng[1] + 1);
	area = (area == 0) ? 1 : area;
	vector<vector<int>> idxs; idxs.reserve(area * 2);
	for (int c=rng[0]; c <= rng[2]; c++) {
		for (int r=rng[1]; r <= rng[3]; r++)
			idxs.push_back({c, r});
	}
	return idxs;
}

int exColToInt(string& col) {
	int colInt = 0, len = col.length();
	for (int i=0; i<len; i++)
		colInt += ((int)col[i] - 64) * (int)(pow(26, len-i-1));
	return colInt;
}

DAG::Vertex getIndexFromCell(const string& str) {
	return regexAll(str, AS_INDEX_REGEX)[0];
}

string getSheetIdFromIndex(const DAG::Vertex& idx) {
	vector<string> refParts;
	split(refParts, idx, is_any_of("/"));
	return refParts[1]; // get sheet id
}

vector<string> regexAll(const string& str, const string& pattern) {
	vector<string> matches;
	sregex_token_iterator iter(str.begin(), str.end(), regex(pattern), 0);
	sregex_token_iterator end;
	for( ; iter != end; ++iter ) matches.push_back(*iter);
    return matches;
}

/****************************************************************************************************************************************/
// printing

void printVec2(vector<vector<int>> in) {
	for (const auto& elem : in) {
		cout << endl;		
		for (const auto& elem2 : elem) cout << elem2 << ", ";
	}
	cout << endl;
}

void printVec(vector<string> in) {
	cout << endl;		
	for (const auto& elem : in) cout << elem << ", ";		
	cout << endl;
}

void showAdjList(const DAG::AdjacencyList& al, string msg) {
	cout << "=================================================================" << endl << msg << endl; 
	for (const auto& toFroms : al){
		auto toLoc = toFroms.first; 
		auto fromLocs = toFroms.second;

		cout << toLoc << ": ";
		for (const auto& fromLoc : fromLocs)
			cout << fromLoc << "\t";
		cout << endl; 
	}
}

void DAG::showGraph(string msg) {
	// cout << "==================================================================================================================================";
	// cout << "\n" << msg << "\n\n";
	// showAdjList(fromToAdjList, "From To Adjacency List");
	// showAdjList(toFromAdjList, "To From Adjacency List");
	// showAdjList(prevCache, "Previous cache");
}