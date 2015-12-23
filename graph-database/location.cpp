#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <boost/regex.hpp>
#include "location.h"

using namespace std;
string refDelimiter = "/"; 

/***********************************************************************************************************************/
// Class methods

/* Convert a pointer to the underlying index */
Location Location::pointerToIndex() const {
  if (type == LocationType::POINTER) {
    return Location(LocationType::INDEX,sheetName,tlCol,tlRow,0,0);
  } else {
    cout << "Didn't use pointerToIndex on a POINTER type" << endl;
    throw "Didn't use pointerToIndex on a POINTER type";
  }
};

/* Decompose a range into indices. The order doesn't particularly matter. */
void Location::rangeToIndices(vector<Location>& indices) const {
  if (type == LocationType::RANGE) {
    for (int i = tlCol; i <= brCol; ++i) {
      for (int j = tlRow; j <= brRow; ++j) {
        indices.push_back(Location(LocationType::INDEX,sheetName,i,j,0,0));
      }
    }
  } else {
    cout << "Didn't use rangeToIndices on a RANGE type" << endl;
    throw "Didn't use rangeToIndices on a RANGE type";
  }
};

void Location::colRangeToMinRowAndColumns(int& minRow, vector<Column>& columns) const {
  cout << "HAHAHA" << endl;
};
/***********************************************************************************************************************/
// Hash function of Location for map data structures 

namespace std {
  size_t hash<Location>::operator()(const Location& l) const {
    return hash_value(l); // using the friend boost hash
  }
}

/***********************************************************************************************************************/
// Hash function of Column for map data structures 

namespace std {
  size_t hash<Column>::operator()(const Column& c) const {
    return hash_value(c); // using the friend boost hash
  }
}
/***********************************************************************************************************************/
// To and from string methods for Location

/* 
  Make an index/pointer regex and a range regex and try to match both
  Throw error if no parse
*/
Location fromString(const string& str) {
  string prefix = "^([IPR])\\" + refDelimiter + "(.*)\\" + refDelimiter;
  string index = "\\(([0-9]+),([0-9]+)\\)$";
  string range = "\\(\\(([0-9]+),([0-9]+)\\),\\(([0-9]+),([0-9]+)\\)\\)$";
  boost::regex indexPattern(prefix + index);
  boost::regex rangePattern(prefix + range);
  boost::smatch r;
  if (boost::regex_search(str, r, indexPattern)) {
    string type(r[1].first, r[1].second);
    string sheet(r[2].first, r[2].second);
    string col(r[3].first, r[3].second);
    string row(r[4].first, r[4].second);
    // cout << sheet << "\t" << col << "\t" << row << "\t" << endl;
    if (type == "I") {
      return Location(Location::LocationType::INDEX,sheet,stoi(col),stoi(row),0,0);
    } else if (type == "P") {
      return Location(Location::LocationType::POINTER,sheet,stoi(col),stoi(row),0,0);
    }
  } else if (boost::regex_search(str, r, rangePattern)) {
    string type(r[1].first, r[1].second);
    string sheet(r[2].first, r[2].second);
    string col(r[3].first, r[3].second);
    string row(r[4].first, r[4].second);
    string col2(r[5].first, r[5].second);
    string row2(r[6].first, r[6].second);
    // cout << sheet << "\t" << col << "\t" << row << "\t" << col2 <<  "\t" << row2 << endl;
    if (type == "R") {
      return Location(Location::LocationType::RANGE,sheet,stoi(col),stoi(row),stoi(col2),stoi(row2));
    }
  } 
  cout << "Could not parse string into location " << str << endl;
  throw "Could not parse string into location";
}

string toString(const Location& l) {
  string middle = refDelimiter + l.getSheetName() + refDelimiter;
  string location;
  int tlCol = l.getTlCol(); 
  int tlRow = l.getTlRow();
  int brRow = l.getBrRow();
  int brCol = l.getBrCol();
  switch(l.getLocationType()) {
    case Location::LocationType::INDEX:
      location = "(" + to_string(tlCol) + "," + to_string(tlRow) + ")";
      return "I" + middle + location;
      break;
    case Location::LocationType::POINTER:
      location = "(" + to_string(tlCol) + "," + to_string(tlRow) + ")";
      return "P" + middle + location;
      break;
    case Location::LocationType::RANGE: 
      location = "((" +  to_string(tlCol) + "," + to_string(tlRow) + "),(" + to_string(brCol) + "," + to_string(brRow) + "))";
      return "R" + middle + location;
      break;
  }
};
