#include "graph.hpp"
#include <algorithm>

/****************************************************************************************************************************************/

using namespace std; 

void DAG::rollback() {
	for (const auto& fan : fanCache)
		updateDAG(fan.first, fan.second, false);

	clearFanCache();
}

//::ALEX:: rename
bool DAG::dfsVisit2(const Vertex& loc, unordered_map<Vertex,bool>& visited) { 
	visited[loc] = true; 
	for (const auto& toLoc : fromToAdjList[loc]) {
		if (!visited[toLoc]) {
			if (DAG::dfsVisit2(toLoc, visited))
				return true; 
		} else { 
			return true; 
		}
	}
	return false; 
}

bool DAG::containsCycle(const DAG::Vertex& start) { 
	unordered_map<DAG::Vertex,bool> visited; 
	return dfsVisit2(start, visited); 
}



DAG& DAG::updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs, bool addToCache) {
	// cout << "In dag update with toloc: " << toLoc << endl; 
	if (addToCache) {
		if (fanCache.find(toLoc) == fanCache.end())
			fanCache[toLoc] = toFromAdjList[toLoc];
	}

	DAG::VertexSet vl = toFromAdjList[toLoc]; //old fromLocs
	/* Loop over the current fromLocs of toLoc and delete from forward adjacency list */
	for (const auto& oldFl : vl){
		fromToAdjList[oldFl].erase(toLoc);
		/* If a vertex no longer has fromLocs, delete it */
		if (fromToAdjList[oldFl].empty())
			fromToAdjList.erase(oldFl);
	}

	toFromAdjList.erase(toLoc); //::ALEX:: ???
	// cout << "\n\ndeleted toLoc and oldFl's from fromToAdjList\n";
	// showGraph();

	/* Loop over the new fromLocs and add to the forward adjacency list */
	for (const auto& fl : fromLocs) {
		fromToAdjList[fl].insert(toLoc);
	/* Replace toLoc entry in backwards adjacency list */
		toFromAdjList[toLoc].insert(fl);
	}

	// cout << "Updated graph in update dag: " << endl;
	// showGraph(); 
	return *this; 
}

/****************************************************************************************************************************************/

void DAG::dfsVisit (const DAG::Vertex& loc, unordered_map<DAG::Vertex,bool>& visited, vector<DAG::Vertex>& order){
	for (const auto& toLoc : fromToAdjList[loc]){
		if (!visited[toLoc]) {
			DAG::dfsVisit(toLoc,visited,order);
		}
	}
	order.push_back(loc);
	visited[loc] = true; 
}

bool DAG::clearFanCache() {
	fanCache.clear();
}

// Given a list of cells, return all of their descendants in the DAG, sorted topologically. 
//(X is a proper descendant of Y if there's a path of length >= 1 from X to Y.)
vector<DAG::Vertex> DAG::getDescendants(const vector<DAG::Vertex>& locs){
	unordered_map<DAG::Vertex,bool> visited;
	vector<DAG::Vertex> order; 

	for (const auto& loc: locs)
		visited[loc] = false;

	for (const auto& loc: locs) {
		if (!visited[loc])
			DAG::dfsVisit(loc, visited, order);
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
		for (const auto& anc: toFromAdjList[loc]){
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

void DAG::showGraph(){
	showAdjList(fromToAdjList, "From To Adjacency List");
	showAdjList(toFromAdjList, "To From Adjacency List");
	showAdjList(fanCache, "Fan Cache");
}