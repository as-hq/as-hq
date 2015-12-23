#ifndef GRAPH_H
#define GRAPH_H

#include <string>
#include <iostream>
#include <unistd.h>
#include <unordered_map>
#include <unordered_set>
#include "location.cpp"

using namespace std;

class DAG {
public:
  typedef Location Vertex;
  typedef unordered_set<Vertex> VertexSet;
  typedef unordered_map<Vertex,VertexSet> AdjacencyList;
  // Column -> [Int -> [Vertex]].
  typedef unordered_map<Column, unordered_map<int, unordered_set<Vertex>>> ColNeighbors;

  /* The last element of the response back to Haskell, success/failure */
  enum DAGStatus {
    OK,
    CIRC_DEP,
    ERROR,
    UNKNOWN_REQUEST_TYPE
  };

  /* A response is a vector of locations (descendants, etc) and a status */
  struct DAGResponse {
    vector<Location> locs;
    DAGStatus status;
  };

  void updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs);
  void clearDAG();

  DAGResponse getProperDescendants(const vector<Vertex>& locs);
  DAGResponse getAllDescendants(const vector<Vertex>& locs);
  DAGResponse getImmediateAncestors(const vector<Vertex>& locs);
  bool containsCycle(const DAG::Vertex&);

  void showGraph(string msg);
  int recomputeDAG();
  bool operator==(const DAG& rhs);

private:
  AdjacencyList toFromAdjList;
  AdjacencyList fromToAdjList;
  AdjacencyList prevCache;
  ColNeighbors fromColTo;

  /* Fills the second argument vector with the immediate descendants in the graph of the first argument. */
  void fillImmediateDescendants(const Vertex& loc, vector<Vertex>& locs);
  void depthFirstSearch(const Vertex& loc, unordered_map<Vertex,bool>& visited, vector<Vertex>& order);
  bool cycleCheck(const Vertex& loc, unordered_map<Vertex,bool>& visited, unordered_map<Vertex,bool>& rec_stack);
};

#endif /* GRAPH_H */ 
