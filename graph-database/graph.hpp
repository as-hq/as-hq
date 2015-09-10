#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <unordered_map>
#include <unordered_set>

class DAG {
public:
	typedef std::unordered_set<std::string> vertexSet;
	typedef std::unordered_map<std::string,vertexSet> AdjacencyList;

	DAG& updateDAG(const std::string &toLoc, const std::vector<std::string> &fromLocs);
	void dfsVisit (const std::string& loc, std::unordered_map<std::string,bool>& visited, std::vector<std::string>& order);
	std::vector<std::string> getDescendants(const std::vector<std::string>& locs);
	std::vector<std::string> getImmediateAncestors(const std::vector<std::string>& locs);
	void showGraph();

private:
	AdjacencyList toFromAdjList;
	AdjacencyList fromToAdjList;
};