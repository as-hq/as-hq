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
  typedef unordered_map<Column, unordered_map<int, unordered_set<Vertex>>> ColNeighbors;
  // Column -> [Int -> [Vertex]]. Ex: If B1 has A1+1 and C1 has A3+1 in it and D1 has A1+2 in it,
  // this would correspond to [(column A, [1 -> [B1, D1], 3 -> [C1]])]. 
  // (it's *possible* the indices above might be off by 1 -- Alex 2/29)

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
  void clearSheetDAG(string sheetId);
  void clearDAG();

  DAGResponse getEntireRootedSubgraph(const vector<Vertex>& locs, const bool& searchForward);
  DAGResponse getProperDescendants(const vector<Vertex>& locs);
  DAGResponse getImmediateAncestors(const vector<Vertex>& locs);
  int indexOfFirstVertexInCycle(const vector<DAG::Vertex>&);

  void showGraph(string msg);
  bool operator==(const DAG& rhs);

  // arrow from A1 to B1 means A1 is a parent of B1
  AdjacencyList childrenToParents; // an element of this adjacency list takes in a child and returns its parents
  AdjacencyList parentsToChildren; // an element of this adjacency list takes in a parent and returns its children
  ColNeighbors fromColumnTo;

  /* Fills the locs vector with the immediate descendants loc.
   * Immediate descendants of loc are a mix of descendants referenced as part of a
   * range, pointer, or index expression (in the parentsToChildren), and the colRange
   * descendants (in the fromColTo map). */

private:
  VertexSet getImmediateAncestorSet(const Vertex& loc);
  VertexSet getImmediateDescendantSet(const Vertex& loc);
  VertexSet findColDescendantSet(const Vertex& loc);
  bool cycleCheck(const Vertex& loc, unordered_map<Vertex,bool>& visited, unordered_map<Vertex,bool>& rec_stack);
  void depthFirstSearch(
    const Vertex& loc, 
    unordered_map<Vertex,bool>& visited, 
    vector<Vertex>& order,
    const bool& searchForward // indicates whether to search forward or backward along edges
  );
};

#endif /* GRAPH_H */
