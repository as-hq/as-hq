#ifndef LOCATION_H             
#define LOCATION_H

#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <boost/regex.hpp>

using namespace std;
class Location {
public:

  /* Different types of locations that can be stored as vertices in the DAG */
  enum LocationType {
    INDEX,
    POINTER,
    RANGE
  };

  /* Location constructors */
  Location() {};
  Location(const LocationType &t, const string& sheet, const int tlCol, int tlRow, int brCol, int brRow) 
    : type(t), sheetName(sheet), tlCol(tlCol), tlRow(tlRow), brCol(brCol), brRow(brRow) {};

  /* Convert a pointer to the underlying index */
  Location pointerToIndex() const;

  /* Decompose a range into indices. The order doesn't particularly matter. */
  void rangeToIndices(vector<Location>& indices) const;

  /* Equality of locations, needed for vector deletion etc. */
  bool operator==(const Location& rhs) const { 
    return (tlCol == rhs.getTlCol())
      && (tlRow == rhs.getTlRow())
      && (brCol == rhs.getBrCol())
      && (brRow == rhs.getBrRow())
      && (sheetName == rhs.getSheetName())
      && (type == rhs.getLocationType());
  };

  /* Boost hash */
  friend size_t hash_value(const Location& l) {
    size_t seed = 0;
    boost::hash_combine(seed, l.tlCol);
    boost::hash_combine(seed, l.tlRow);
    boost::hash_combine(seed, l.brCol);
    boost::hash_combine(seed, l.brRow);
    boost::hash_combine(seed, l.sheetName);
    boost::hash_combine(seed, l.type);
    return seed;
  }

  /* Standard getters for private attributes */
  int getTlCol() const {
    return tlCol;
  };
  int getTlRow() const {
    return tlRow;
  };
  int getBrCol() const {
    return brCol;
  };
  int getBrRow() const {
    return brRow;
  };
  string getSheetName() const {
    return sheetName;
  };
  LocationType getLocationType() const {
    return type;
  };

private:
  LocationType type;
  string sheetName;
  /* All locations have all four coordinates, replace br with (0,0) for non-ranges */
  int tlCol; 
  int tlRow;
  int brCol;
  int brRow;
};

#endif /* LOCATION_H */ 