#ifndef GRAPH_H
#define GRAPH_H

#include <string>
#include <iostream>
#include <unistd.h>
#include <unordered_map>
#include <unordered_set>
#include "location.h"

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

  DAGResponse getEntireRootedSubgraph(const vector<Vertex>& locs, const bool& searchForward);
  DAGResponse getProperDescendants(const vector<Vertex>& locs);
  DAGResponse getImmediateAncestors(const vector<Vertex>& locs);
  bool containsCycle(const DAG::Vertex&);

  void showGraph(string msg);
  int recomputeDAG();
  bool operator==(const DAG& rhs);

  AdjacencyList toFromAdjList;
  AdjacencyList fromToAdjList;
  AdjacencyList prevCache;
  ColNeighbors fromColumnTo;

  /* Fills the locs vector with the immediate descendants loc.
   * Immediate descendants of loc are a mix of descendants referenced as part of a
   * range, pointer, or index expression (in the fromToAdjList), and the colRange
   * descendants (in the fromColTo map). */
  VertexSet getImmediateDescendantSet(const Vertex& loc);
  VertexSet getImmediateAncestorSet(const Vertex& loc);
  VertexSet findColDescendantSet(const Vertex& loc);
  void depthFirstSearch(
    const Vertex& loc, 
    unordered_map<Vertex,bool>& visited, 
    vector<Vertex>& order,
    const bool& searchForward // indicates whether to search forward or backward along edges
  ); 
  bool cycleCheck(const Vertex& loc, unordered_map<Vertex,bool>& visited, unordered_map<Vertex,bool>& rec_stack);
private:
};

#endif /* GRAPH_H */
