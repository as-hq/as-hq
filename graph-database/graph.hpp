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

	void updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs, bool addToCache=true);
    void clearDAG();

  //TODO: in this implementation, the last entry of the vector does NOT actually
  //represent a vertex; it's either an OK or some error message, indicating the outcome
  //of the computation. It just happens that Vertex is a string, so this hacky
  //thing is allowed. 
	std::vector<Vertex> getProperDescendants(const std::vector<Vertex>& locs);
  std::vector<Vertex> getAllDescendants(const std::vector<Vertex>& locs);
	std::vector<Vertex> getImmediateAncestors(const std::vector<Vertex>& locs);
	void showGraph(std::string msg);
	void rollback(); //can only roll back one "commit"; prevCache stores the previous values
	bool containsCycle(const DAG::Vertex&);
	bool clearPrevCache();
    int recomputeDAG();


    bool operator==(const DAG& rhs);

private:
    AdjacencyList toFromAdjList;
    AdjacencyList fromToAdjList;
    AdjacencyList prevCache;

    void updateDAGDfs(const Vertex& loc, std::unordered_map<Vertex,bool>& visited, std::vector<Vertex>& order);
    bool cycleCheckDfs(const Vertex& loc, std::unordered_map<Vertex,bool>& visited, std::unordered_map<Vertex,bool>& rec_stack);
};

DAG::VertexSet parseDependencies(const std::string& str, const DAG::Vertex& fromLoc);
DAG::VertexSet constructASIndices(const std::string& sheetId, std::vector<std::vector<int>>& coords);
std::vector<int> exRefToCoordinate(std::string& ref);
std::vector<int> exIndexToCoordinate(std::string& idx);
std::vector<std::vector<int>> decomposeRange(const std::vector<int>& rng);
int exColToInt(std::string& col);
DAG::Vertex getIndexFromCell(const std::string& str);
std::string getSheetIdFromIndex(const DAG::Vertex& idx);
std::vector<std::string> regexAll(const std::string& str, const std::string& pattern);
void printVec2(std::vector<std::vector<int>> in);
void printVec(std::vector<std::string> in);
std::string makeASIndex(const std::string& sheetId, const int& col, const int& row);