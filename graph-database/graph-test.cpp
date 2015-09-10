#define BOOST_TEST_MODULE graph_test
#include <boost/test/included/unit_test.hpp>
#include "graph.cpp"
#include <time.h>       


/****************************************************************************************************************************************/

BOOST_AUTO_TEST_SUITE(updating)

BOOST_AUTO_TEST_CASE(updateEmpty){
	DAG d;
	std::vector<std::string> toLocs = {"b","c"};
	d.updateDAG("a",toLocs);
	d.showGraph();
}

BOOST_AUTO_TEST_CASE(doubleUpdate){
    DAG d;
	std::vector<std::string> toLocs = {"b","c"};
	d.updateDAG("a",toLocs);
	d.updateDAG("a",toLocs);
	d.showGraph();
}

BOOST_AUTO_TEST_CASE(manySmallUpdates){
	std::cout << "Doing many small updates" << std::endl; 
    DAG d;
    clock_t begin = clock(); 
    std::vector<std::string> fl = {"d"};
	for (int i = 0 ; i < 10000; ++i){
		d.updateDAG(std::to_string(i),fl);
	}
	clock_t end = clock(); 
	printf("Time taken: %.2fs\n", (double)(end - begin)/CLOCKS_PER_SEC);
}

BOOST_AUTO_TEST_CASE(oneBigUpdate){
	std::cout << "Doing one big update" << std::endl; 
    DAG d;
    clock_t begin = clock(); 
    std::vector<std::string> toLocs;
	for (int i = 0 ; i < 10000; ++i){
		toLocs.push_back(std::to_string(i));
	}
	d.updateDAG("a",toLocs);
	clock_t end = clock(); 
	printf("Time taken: %.2fs\n", (double)(end - begin)/CLOCKS_PER_SEC);
}

BOOST_AUTO_TEST_CASE(replaceUpdateShouldDeleteExtraVertices){
    DAG d;
	std::vector<std::string> toLocs = {"b","c"};
	std::vector<std::string> newToLocs = {"d"};
	d.updateDAG("a",toLocs);
	d.updateDAG("a",newToLocs);
	d.showGraph();
}


BOOST_AUTO_TEST_SUITE_END()

/****************************************************************************************************************************************/

BOOST_AUTO_TEST_SUITE(descendants)

BOOST_AUTO_TEST_CASE(simpleDescendants){
    DAG d;
	std::vector<std::string> toLocs = {"b","c"};
	std::vector<std::string> dQuery = {"a"};
	d.updateDAG("a",toLocs);
	std::vector<std::string> descendants = d.getDescendants(dQuery);
	for (const auto& loc : descendants)
		std::cout << loc << "\t"; 
	std::cout << std::endl; 
}

BOOST_AUTO_TEST_CASE(largeGraphDescendants){
    DAG d;
	std::vector<std::string> toLocs;
	for (int i = 0 ; i < 1000000; ++i){
		toLocs.push_back(std::to_string(i));
	}
	std::vector<std::string> dQuery = {"a"};
	d.updateDAG("a",toLocs);
	std::vector<std::string> descendants = d.getDescendants(dQuery);
	for (const auto& loc : descendants)
		std::cout << loc << "\t"; 
	std::cout << std::endl; 
}


BOOST_AUTO_TEST_SUITE_END()

/****************************************************************************************************************************************/

BOOST_AUTO_TEST_SUITE(ancestors)


BOOST_AUTO_TEST_SUITE_END()