#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <unordered_map>
#include <unordered_set>

class DAG {
public:
	typedef std::string Vertex;
	typedef std::unordered_set<Vertex> VertexSet;
	typedef std::unordered_map<Vertex,VertexSet> AdjacencyList;
	typedef std::pair<Vertex,VertexSet> Fan;

	DAG& updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs, bool addToCache=true);
  DAG& clearDAG();
	void dfsVisit (const Vertex& loc, std::unordered_map<Vertex,bool>& visited, std::vector<Vertex>& order);
	bool dfsVisit2(const Vertex& loc, std::unordered_map<Vertex,bool>& visited);
	std::vector<Vertex> getDescendants(const std::vector<Vertex>& locs);
	std::vector<Vertex> getImmediateAncestors(const std::vector<Vertex>& locs);
	void showGraph();
	void rollback();
	bool containsCycle(const DAG::Vertex&);

	bool clearFanCache();

	//define fan
	bool operator==(const DAG& rhs);

private:
	AdjacencyList toFromAdjList;
	AdjacencyList fromToAdjList;

	AdjacencyList fanCache;
};