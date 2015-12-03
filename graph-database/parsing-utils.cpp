#include "graph.h"
#include <hiredis/hiredis.h>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/regex.hpp>

// TODO: defines are bad style in C++
#define REDIS_HOST "localhost"
#define REDIS_PORT 6379
#define SCAN_BOUND 100000
#define INDEX_REGEX "\\$?[A-Za-z]+\\$?[0-9]+"
#define RANGE_REGEX "\\$?[A-Za-z]+\\$?[0-9]+:\\$?[A-Za-z]+\\$?[0-9]+"
#define AS_INDEX_REGEX "I\\/[^\\/]+\\/\\([0-9]+,[0-9]+\\)"
#define REF_PART_REGEX "\\/"
#define REF_PART "/"

using namespace std;
using namespace boost;

/****************************************************************************************************************************************/
// Parsing utilities

string makeASIndexString(const string& sheetId, const int col, const int row) {
 return "I/" + sheetId + "/(" + to_string(col) + "," + to_string(row) + ")";
}

int exColToInt(string& col) {
 int colInt = 0, len = col.length();
 for (int i=0; i<len; i++)
   colInt += ((int)col[i] - 64) * (int)(pow(26, len-i-1));
 return colInt;
}

vector<int> exIndexToCoordinate(string& ref) {
 int i = (ref[0] == '$') ? 1 : 0; //skip a doller
 int start = i;
 while (isalpha(ref[i])) i++;
 string col = ref.substr(start, i-start);

 i = (ref[i-start+1] == '$') ? i+1 : i; // skip anothar doller
 string row = ref.substr(i);
 return {exColToInt(col), stoi(row)};
}

vector<int> exRefToCoordinate(string& ref) {
 vector<string> corners;
 boost::split(corners, ref, boost::is_any_of(":"));
 if (corners.size() == 2) {
   vector<int> tl = exIndexToCoordinate(corners[0]);
   vector<int> br = exIndexToCoordinate(corners[1]);
   tl.insert(tl.end(), br.begin(), br.end());
   return tl;
 } else {
   return exIndexToCoordinate(ref);
 }
}

vector<vector<int>> decomposeRange(const vector<int>& rng) {
 int area = (rng[2] - rng[0] + 1) * (rng[3] - rng[1] + 1);
 area = (area == 0) ? 1 : area;
 vector<vector<int>> idxs; idxs.reserve(area * 2);
 for (int c=rng[0]; c <= rng[2]; c++) {
   for (int r=rng[1]; r <= rng[3]; r++)
     idxs.push_back({c, r});
 }
 return idxs;
}

string getSheetIdFromIndex(const string& idx) {
 vector<string> refParts;
 split(refParts, idx, is_any_of("/"));
 return refParts[1]; // get sheet id
}

vector<string> regexAll(const string& str, const string& pattern) {
 vector<string> matches;
 sregex_token_iterator iter(str.begin(), str.end(), regex(pattern), 0);
 sregex_token_iterator end;
 for( ; iter != end; ++iter ) matches.push_back(*iter);
    return matches;
}

/****************************************************************************************************************************************/
// Parsing relating to the DAG

DAG::VertexSet constructASIndices(const string& sheetId, vector<vector<int>>& coords) {
 DAG::VertexSet idxs;
 for (const auto& coord : coords) {
   if (coord.size() == 2) {
     idxs.insert(fromString(makeASIndexString(sheetId, coord[0], coord[1])));
   } else {
     vector<vector<int>> rngCoords;
     rngCoords = decomposeRange(coord);
     for (const auto& rngCoord : rngCoords)
       idxs.insert(fromString(makeASIndexString(sheetId, rngCoord[0], rngCoord[1])));
   }
 }
 return idxs;
}

DAG::VertexSet parseDependencies(const string& str, const DAG::Vertex& fromLoc) {
 vector<string> rangeMatches = regexAll(str, RANGE_REGEX);
 string rangesRemoved = regex_replace(str, regex(RANGE_REGEX), "");
 vector<string> indexMatches = regexAll(rangesRemoved, INDEX_REGEX);

 indexMatches.insert(indexMatches.end(), rangeMatches.begin(), rangeMatches.end());
 // cout << "index matches: " << endl;
 // printVec(indexMatches);
 vector<vector<int>> coordinates;
 transform(indexMatches.begin(), indexMatches.end(), back_inserter(coordinates), exRefToCoordinate);
 // cout << "coordinates: " << endl;
 // printVec2(coordinates);
 string sheetId = getSheetIdFromIndex((toString(fromLoc)));
 return constructASIndices(sheetId, coordinates);
}

DAG::Vertex getIndexFromCell(const string& str) {
 return fromString(regexAll(str, AS_INDEX_REGEX)[0]);
}
