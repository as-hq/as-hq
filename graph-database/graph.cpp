#include "graph.hpp"
#include <algorithm>

/****************************************************************************************************************************************/

using namespace std; 

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

DAG& DAG::updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs, bool addToCache) {
	DAG::VertexSet vl = toFromAdjList[toLoc]; //old fromLocs

	// If a vertex gets updated multiple times, only record the first update. 
	if (addToCache) {
		if (prevCache.count(toLoc) == 0 && toFromAdjList.count(toLoc) > 0 && toFromAdjList[toLoc].size() > 0)
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
		fromToAdjList[fl].insert(toLoc);
	/* Replace toLoc entry in backwards adjacency list */
		toFromAdjList[toLoc].insert(fl);
	}

	return *this; 
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

/****************************************************************************************************************************************/

// Given a list of cells, return all of their descendants in the DAG, sorted topologically. 
//(X is a proper descendant of Y if there's a path of length >= 1 from X to Y.)
vector<DAG::Vertex> DAG::getDescendants(const vector<DAG::Vertex>& locs){
	unordered_map<DAG::Vertex,bool> visited;
	vector<DAG::Vertex> order; 

	for (const auto& loc: locs)
		visited[loc] = false;

	for (const auto& loc: locs) {
		if (!visited[loc])
			DAG::updateDAGDfs(loc, visited, order);
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
	cout << "==================================================================================================================================";
	cout << "\n" << msg << "\n\n";
	showAdjList(fromToAdjList, "From To Adjacency List");
	showAdjList(toFromAdjList, "To From Adjacency List");
	showAdjList(prevCache, "Previous cache");
}