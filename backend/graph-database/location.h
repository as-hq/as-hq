#ifndef LOCATION_H
#define LOCATION_H

#include <string>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <boost/regex.hpp>

using namespace std;

class Column {
public:
  Column() {};
  Column(const string& sheet, const int col): _sheetId(sheet), _columnNumber(col) {};

  bool operator==(const Column& rhs) const {
    return (_columnNumber == rhs.getColumnNumber()
            && _sheetId == rhs.getSheetId());
  }

  friend size_t hash_value(const Column& c) {
    size_t seed = 0;
    boost::hash_combine(seed, c.getColumnNumber());
    boost::hash_combine(seed, c.getSheetId());
    return seed;
  }

  /* Standard getters for private attributes */

  int getColumnNumber() const {
    return _columnNumber;
  }
  string getSheetId() const {
    return _sheetId;
  }

private:
  string _sheetId;
  int _columnNumber;
};

class Location {
public:

  /* Different types of locations that can be stored as vertices in the DAG */
  enum LocationType {
    INDEX,
    POINTER,
    COLRANGE,
    RANGE
  };

  /* Location constructors */
  Location() {};
  Location(const LocationType &t, const string& sheet, const int tlCol, int tlRow, int brCol, int brRow) 
    : type(t), sheetId(sheet), tlCol(tlCol), tlRow(tlRow), brCol(brCol), brRow(brRow) {};

  /* Convert a pointer to the underlying index */
  Location pointerToIndex() const;

  /* Gets the minRow and Columns from a colRange */
  void colRangeToMinRowAndColumns(int& minRow, vector<Column>& cols) const;

  /* Decompose a range into indices. The order doesn't particularly matter. */
  void rangeToIndices(vector<Location>& indices) const;

  /* Get the column that an index is contained in. */
  Column indexToContainingColumn() const;

  /* Equality of locations, needed for vector deletion etc. */
  bool operator==(const Location& rhs) const { 
    return (tlCol == rhs.getTlCol())
      && (tlRow == rhs.getTlRow())
      && (brCol == rhs.getBrCol())
      && (brRow == rhs.getBrRow())
      && (sheetId == rhs.getSheetId())
      && (type == rhs.getLocationType());
  };

  /* Boost hash */
  friend size_t hash_value(const Location& l) {
    size_t seed = 0;
    boost::hash_combine(seed, l.tlCol);
    boost::hash_combine(seed, l.tlRow);
    boost::hash_combine(seed, l.brCol);
    boost::hash_combine(seed, l.brRow);
    boost::hash_combine(seed, l.sheetId);
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
  string getSheetId() const {
    return sheetId;
  };
  LocationType getLocationType() const {
    return type;
  };

private:
  LocationType type;
  string sheetId;
  /* All locations have all four coordinates, replace br with (0,0) for non-ranges */
  /* Replace brRow with 0 for colRange */
  int tlCol;
  int tlRow;
  int brCol;
  int brRow;
};

/******************** Helper functions for isolating features of index *****************/
int getRowNumOfIndex(const Location& loc);
int getColumnNumOfIndex(const Location& loc);
Column getColumnOfIndex(const Location& loc);


/********************* String conversion helpers *************************************/
Location fromString(string str);
string toString(const Location& l);


/********************* Hash functions for Locations and Columns **********************/
namespace std {
  template <>
  struct hash<Location> {
    size_t operator() (const Location &l) const;
  };

  template <>
  struct hash<Column> {
    size_t operator() (const Column &l) const;
  };
}

#endif /* LOCATION_H */ 
