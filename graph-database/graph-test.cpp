#define BOOST_TEST_MODULE graph_test
#include <boost/test/included/unit_test.hpp>
#include "graph.cpp"
#include <time.h>    

using namespace std;

Location a1 = Location(Location::LocationType::INDEX,"",1,1,0,0);
Location a2 = Location(Location::LocationType::INDEX,"",1,2,0,0);
Location b1 = Location(Location::LocationType::INDEX,"",2,1,0,0);
Location b2 = Location(Location::LocationType::INDEX,"",2,2,0,0);

Location a1b1 = Location(Location::LocationType::RANGE,"",1,1,2,1);
Location a1a3 = Location(Location::LocationType::RANGE,"",1,1,1,3);

Location pa1 = Location(Location::LocationType::POINTER,"",1,1,0,0);
Location pa2 = Location(Location::LocationType::POINTER,"",1,2,0,0);


BOOST_AUTO_TEST_SUITE(updating)

BOOST_AUTO_TEST_CASE(updateEmpty) {
	DAG d;
	unordered_set<Location> fromLocs = {a1,a2};
	d.updateDAG(b1,fromLocs);
	d.showGraph("updateEmpty");
}

BOOST_AUTO_TEST_CASE(updatesAreIdempotent ){
  DAG d;
	unordered_set<Location> fromLocs = {a1,a2};
	d.updateDAG(b1,fromLocs);
	d.updateDAG(b1,fromLocs);
	d.showGraph("doubleUpdate");
}


BOOST_AUTO_TEST_CASE(replaceUpdateShouldDeleteExtraVertices) {
  DAG d;
	unordered_set<Location> fromLocs = {a1,a2};
	unordered_set<Location> newFromLocs = {b2};
	d.updateDAG(b1,fromLocs);
	d.updateDAG(b1,newFromLocs);
	d.showGraph("replaceUpdate");
}

BOOST_AUTO_TEST_CASE(updateRange1) {
  DAG d;
	unordered_set<Location> fromLocs = {a1,a1a3};
	d.updateDAG(b1,fromLocs);
	d.showGraph("updateRange1");
}

BOOST_AUTO_TEST_CASE(updateRange2) {
  DAG d;
	unordered_set<Location> fromLocs = {a1b1,a1a3};
	d.updateDAG(b2,fromLocs);
	d.showGraph("updateRange2");
}

BOOST_AUTO_TEST_SUITE_END()

// /****************************************************************************************************************************************/

// BOOST_AUTO_TEST_SUITE(descendants)

// BOOST_AUTO_TEST_CASE(simpleDescendants){
//     DAG d;
// 	std::vector<std::string> toLocs = {"b","c"};
// 	std::vector<std::string> dQuery = {"a"};
// 	d.updateDAG("a",toLocs);
// 	std::vector<std::string> descendants = d.getDescendants(dQuery);
// 	for (const auto& loc : descendants)
// 		std::cout << loc << "\t"; 
// 	std::cout << std::endl; 
// }

// BOOST_AUTO_TEST_CASE(largeGraphDescendants){
//     DAG d;
// 	std::vector<std::string> toLocs;
// 	for (int i = 0 ; i < 1000000; ++i){
// 		toLocs.push_back(std::to_string(i));
// 	}
// 	std::vector<std::string> dQuery = {"a"};
// 	d.updateDAG("a",toLocs);
// 	std::vector<std::string> descendants = d.getDescendants(dQuery);
// 	for (const auto& loc : descendants)
// 		std::cout << loc << "\t"; 
// 	std::cout << std::endl; 
// }


// BOOST_AUTO_TEST_SUITE_END()

/****************************************************************************************************************************************/

BOOST_AUTO_TEST_SUITE(cycles)

BOOST_AUTO_TEST_CASE(notAlwaysCycle) {
	DAG d; 
	d.updateDAG(a1, {a2});
	BOOST_CHECK(!d.containsCycle(a1));
}

BOOST_AUTO_TEST_CASE(pointerCycle) {
	DAG d; 
	d.updateDAG(a1, {pa2});
	d.updateDAG(a2, {pa1});
	BOOST_CHECK(d.containsCycle(a1));
}

BOOST_AUTO_TEST_CASE(containsCycleInTriangle) {
	DAG d; 
	d.updateDAG(a2, {a1});
	d.updateDAG(b1, {a2});
	d.updateDAG(a1, {b1});
	BOOST_CHECK(d.containsCycle(a1));
}

BOOST_AUTO_TEST_CASE(containsCycleInSelfRef) {
	DAG d; 
	std::vector<std::string> rels; 
	d.updateDAG(a1, {a1,a2});
	BOOST_CHECK(d.containsCycle(a1));
}


BOOST_AUTO_TEST_CASE(containsCycleForIntersectingRange) {
	DAG d; 
	std::vector<std::string> rels; 
	d.updateDAG(a1, {a1a3});
	BOOST_CHECK(d.containsCycle(a1));
}

BOOST_AUTO_TEST_CASE(containsCycleForSelfPointer) {
	DAG d; 
	std::vector<std::string> rels; 
	d.updateDAG(a1, {pa1,a2});
	BOOST_CHECK(d.containsCycle(a1));
}



BOOST_AUTO_TEST_SUITE_END()