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

	DAG& updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs, bool addToCache=true);
  void clearDAG();

  //TODO: in this implementation, the last entry of the vector does NOT actually
  //represent a vertex; it's either an OK or some error message, indicating the outcome
  //of the computation. It just happens that Vertex is a string, so this hacky
  //thing is allowed. 
	std::vector<Vertex> getDescendants(const std::vector<Vertex>& locs);
	std::vector<Vertex> getImmediateAncestors(const std::vector<Vertex>& locs);
	void showGraph();
	void rollback(); //can only roll back one "commit"; prevCache stores the previous values
	bool containsCycle(const DAG::Vertex&);
	bool clearPrevCache();

	bool operator==(const DAG& rhs);

private:
	AdjacencyList toFromAdjList;
	AdjacencyList fromToAdjList;
	AdjacencyList prevCache;

	void updateDAGDfs(const Vertex& loc, std::unordered_map<Vertex,bool>& visited, std::vector<Vertex>& order);
	bool cycleCheckDfs(const Vertex& loc, std::unordered_map<Vertex,bool>& visited, std::unordered_map<Vertex,bool>& rec_stack);
};
