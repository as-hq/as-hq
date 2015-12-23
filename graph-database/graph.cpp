#include "graph.h"
#include "parsing-utils.cpp"
#include <algorithm>
#include <math.h>
#include <ctype.h>

using namespace std;

/****************************************************************************************************************************************/

void DAG::clearDAG() {
  toFromAdjList.clear();
  fromToAdjList.clear();
}

/************ Helper methods for finding immediate descendant. *****/

VertexSet findColDescendants(DAG::Vertex loc){
  VertexSet a;
  int r = getRowNum(loc);
  Column column = getColumn(loc);
  if (fromColumnTo.count(column) > 0) { // Don't create column key in fromColumnTo if column is not present.
    for (const auto& p : fromColumnTo[column]) {
      if (p.first() <= r) {
        VertexSet v  = p.second();
        a.insert(v.begin(), v.end());
      }
    }
  }
}

VertexSEt getImmediateDesc(DAG::Vertex loc) {
  VertexSet a;
  if (fromToAdjList.count(loc) > 0) { 
    a = fromToAdjList[loc];
  }
  return findColDescendants(loc).insert(a.begin(), a.end());
}
/****************************************************************************************************************************************/

bool DAG::cycleCheck(const Vertex& loc, unordered_map<Vertex,bool>& visited, unordered_map<Vertex,bool>& rec_stack) {
  if (!visited[loc]) {
    visited[loc] = true;
    rec_stack[loc] = true;

    if (hasImmediateDesc(loc)){
      for (const auto& toLoc : getImmediateDesc(loc)) {
        if (!visited[toLoc] && DAG::cycleCheck(toLoc, visited, rec_stack)) {
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
  return cycleCheck(start, visited, rec_stack);
}


/****************************************************************************************************************************************/

/* Set one relation in the DAG */
void DAG::updateDAG(DAG::Vertex toLoc, const DAG::VertexSet& fromLocs) {
  DAG::VertexSet vl = toFromAdjList[toLoc]; //old fromLocs
    /* Loop over the current fromLocs of toLoc and delete from forward adjacency list */
  for (const auto& oldFl : vl){
    // if oldFl is a pointer, convert to index and delete toLoc from the index's dependencies
    // To clarify with an example, when D5=@C5 is entered, D5's ancestor is Pointer C5 and Index C5's descendant is D5.
    // When you want to update D5's ancestors, you have to delete Index C5 -> D5 from fromTo, and not Pointer C5 -> D5, which doesn't exist.
    // Similar logic exists for ranges and colRanges;  there's a symmetry between updating and replacing.
    if (oldFl.getLocationType() == Location::LocationType::POINTER) {
      fromToAdjList[oldFl.pointerToIndex()].erase(toLoc);
    } else if (oldFl.getLocationType() == Location::LocationType::COLRANGE) {
      vector<Column> columns;
      int minRow;
      oldFl.colRangeToMinRowAndColumns(minRow, columns); // minRow and columns have the data in colRange
        // TODO: timchu, 12/21/15. Use a helper function to delete minRow from col if the only index in it is erased.
      for (const auto& column: columns){
        fromColTo[column][minRow].erase(toLoc);
      }
    } else if (oldFl.getLocationType() == Location::LocationType::RANGE) {
      vector<Location> indices;
      oldFl.rangeToIndices(indices); // indices now has the decomposed Indices
      for (const auto& index: indices) {
        fromToAdjList[index].erase(toLoc);
      }
    } else { // normal index erasal
      fromToAdjList[oldFl].erase(toLoc);
    }

    // If a vertex no longer has fromLocs, delete it
    if (fromToAdjList[oldFl].empty())
      fromToAdjList.erase(oldFl);
  }
  toFromAdjList.erase(toLoc);


  /* Loop over the new fromLocs and add to the forward adjacency list */
  for (const auto& fl : fromLocs) {
    /* The to-from adjacency list can map Location -> Pointer, Index, or Range
      Index is normal, for C1 = sum(A1:A1000), we have C1(to) -> A1:A1000(from)
      For D5 = @C5, we have D5(to) -> @C5(pointer,from)
      Conceptually, this adjacency list stores direct info from AS/ the expression
    */
    toFromAdjList[toLoc].insert(fl);
    /* If Pointer reference, put an Index ref in the forward list, and put the Pointer ref in the backward list.
      then evalChain' triggers the recomputation of an expression with an @ reference in it correctly
      using getDescendants, then uses getAncestors on those descendants to get the actual Pointer reference.
      If range reference, then decompose the range and set the toLoc as an immediate descendant of each of the
      decomposed indices. For example, for C1 = sum(A1:A1000), set A1 -> C1, A2 -> C1 etc in the fromTo map,
      so that descendants are computed correctly.
    */
    if (fl.getLocationType() == Location::LocationType::POINTER) {
      fromToAdjList[fl.pointerToIndex()].insert(toLoc);
    } else if (fl.getLocationType() == Location::LocationType::COLRANGE) {
      vector<Column> columns;
      int minRow;
      fl.colRangeToMinRowAndColumns(minRow, columns); // minRow and columns have the data in colRange
      for (const auto& column: columns){
        fromColTo[column][minRow].erase(toLoc);
      }
    } else if (fl.getLocationType() == Location::LocationType::RANGE) {
      vector<Location> indices;
      fl.rangeToIndices(indices); // indices now has the decomposed range
      for (const auto& index: indices){
        fromToAdjList[index].insert(toLoc);
      }
    } else {
      fromToAdjList[fl].insert(toLoc);
    }
  }

}

/* Given a location, current visited state, current (reverse) topological order, do one more layer of the DFS and recurse */
void DAG::depthFirstSearch (const DAG::Vertex& loc, unordered_map<DAG::Vertex,bool>& visited, vector<DAG::Vertex>& order){
  if (hasImmediateDesc(loc)) { // so that keys aren't created if there's nothing there.
    for (const auto& toLoc : getImmediateDesc(loc)){
      if (!visited[toLoc]) {
        DAG::depthFirstSearch(toLoc,visited,order);
      }
    }
  }
  order.push_back(loc);
  visited[loc] = true;
}


/****************************************************************************************************************************************/

// Given a list of cells, return all of their descendants in the DAG, sorted topologically.
//(X is a proper descendant of Y if there's a path of length >= 1 from X to Y.)
DAG::DAGResponse DAG::getAllDescendants(const vector<DAG::Vertex>& locs){
  unordered_map<DAG::Vertex,bool> visited;
  vector<DAG::Vertex> order;

  for (const auto& loc: locs) {
    visited[loc] = false;
  }

  for (const auto& loc: locs) {
    if (!visited[loc]){
      DAG::depthFirstSearch(loc, visited, order);
    }
  }

  reverse(order.begin(),order.end());
  return {order,DAG::DAGStatus::OK};
}

DAG::DAGResponse DAG::getProperDescendants(const vector<DAG::Vertex>& locs){
  unordered_map<DAG::Vertex,bool> visited;
  vector<DAG::Vertex> order;

  for (const auto& loc: locs) {
    visited[loc] = false;
  }

  for (const auto& loc: locs) {
    if (!visited[loc]){
      if (hasImmediateDesc(loc)) {
        for (const auto& toLoc : getImmediateDescendants(loc)){
            DAG::depthFirstSearch(toLoc,visited,order);
          }
        }
      }
      visited[loc] = true;
    }
  }

  reverse(order.begin(),order.end());
  return {order,DAG::DAGStatus::OK};
}


/****************************************************************************************************************************************/
// Getting ancestors

DAG::DAGResponse DAG::getImmediateAncestors(const vector<DAG::Vertex>& locs){
  unordered_set<DAG::Vertex> ancestors;
  for (const auto& loc : locs){
    if (toFromAdjList.count(loc)) { // so that the toFromAdjList key isn't created if there's nothing there
      for (const auto& anc: toFromAdjList[loc])
        ancestors.insert(anc);
    }
  }
  vector<DAG::Vertex> vAncestors(ancestors.begin(),ancestors.end());
  return {vAncestors,DAG::DAGStatus::OK};
}

/* DAGs are equal if their adjacency lists are equal */
bool DAG::operator==(const DAG& rhs) {
  return (toFromAdjList == rhs.toFromAdjList) && (fromToAdjList == rhs.fromToAdjList);
}


/****************************************************************************************************************************************/
// Printing

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
  cout << "\t==========================================================================" << endl << msg << endl;
  for (const auto& toFroms : al){
    auto toLoc = toFroms.first;
    auto fromLocs = toFroms.second;

    cout << "\t" << toString(toLoc)<< ": ";
    for (const auto& fromLoc : fromLocs)
      cout << toString(fromLoc) << "\t";
    cout << endl;
  }
}

void DAG::showGraph(string msg) {
  cout << "==========================================================================";
  cout << "\n" << msg << "\n";
  showAdjList(fromToAdjList, "\tFrom To Adjacency List");
  showAdjList(toFromAdjList, "\tTo From Adjacency List");
}
