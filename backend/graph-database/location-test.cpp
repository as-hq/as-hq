#define BOOST_TEST_MODULE location_test
#include <boost/test/included/unit_test.hpp>
#include "location.cpp"
#include <time.h>    

using namespace std;

/****************************************************************************************************************************************/

BOOST_AUTO_TEST_SUITE(locToString)

BOOST_AUTO_TEST_CASE(index){
  Location l = Location(Location::LocationType::INDEX,"hello",1,3,0,0);
  string s = toString(l);
  BOOST_CHECK(s == "I/hello/(1,3)");

}

BOOST_AUTO_TEST_CASE(pointer){
  Location l = Location(Location::LocationType::POINTER,"lolz",2,3,0,0);
  string s = toString(l);
  BOOST_CHECK(s == "P/lolz/(2,3)");
}

BOOST_AUTO_TEST_CASE(range){
  Location l = Location(Location::LocationType::RANGE,"rng",1,3,4,5);
  string s = toString(l);
  BOOST_CHECK(s == "R/rng/((1,3),(4,5))");

}

BOOST_AUTO_TEST_SUITE_END()

/****************************************************************************************************************************************/

BOOST_AUTO_TEST_SUITE(stringToLoc)

BOOST_AUTO_TEST_CASE(index){
  string s = "I/hello/(1,3)";
  Location l = fromString(s);
  BOOST_CHECK(l == Location(Location::LocationType::INDEX,"hello",1,3,0,0));
}

BOOST_AUTO_TEST_CASE(pointer){
  string s = "P/lolz/(2,3)";
  Location l = fromString(s);
  BOOST_CHECK(l == Location(Location::LocationType::POINTER,"lolz",2,3,0,0));
}

BOOST_AUTO_TEST_CASE(range){
  string s = "R/rng/((1,3),(4,5))";
  Location l = fromString(s);
  BOOST_CHECK(l == Location(Location::LocationType::RANGE,"rng",1,3,4,5));
}


BOOST_AUTO_TEST_SUITE_END()