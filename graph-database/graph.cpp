#include "graph.hpp"
#include <algorithm>    

/****************************************************************************************************************************************/

DAG& DAG::updateDAG(const std::vector<std::string>& relation){
	// std::cout << "In dag update with toloc: " << toLoc << std::endl; 
	// std::cout << "fromlocs: " << std::endl; 
	//for (const auto& l : fromLocs)
		// std::cout << "\t" << l << std::endl; 
	/* Loop over the current fromLocs of toLoc and delete from forward adjacency list */

	std::string toLoc = relation[0];

	DAG::vertexSet vl = this->toFromAdjList[toLoc];

	for (const auto& oldFl : vl){
		this->fromToAdjList[oldFl].erase(toLoc);
		/* If a vertex no longer has fromLocs, delete it */
		if (this->fromToAdjList[oldFl].empty())
			this->fromToAdjList.erase(oldFl);
	}
	/* Loop over the new fromLocs and add to  the forward adjacency list */
	for (int i = 1; i < relation.size(); i++){
		this->fromToAdjList[relation[i]].insert(toLoc);
	}
	/* Replace toLoc entry in backwards adjacency list */
	this->toFromAdjList.erase(toLoc);
	for (int i = 1; i < relation.size(); i++){
		this->toFromAdjList[toLoc].insert(relation[i]);
	}
	// std::cout << "Updated graph in update dag: " << std::endl;
	// this->showGraph(); 
	return *this; 
}

/****************************************************************************************************************************************/

void DAG::dfsVisit (const std::string& loc, std::unordered_map<std::string,bool>& visited, std::vector<std::string>& order){
	for (const auto& toLoc : this->fromToAdjList[loc]){
		if (!visited[toLoc]){
			DAG::dfsVisit(toLoc,visited,order);
		}
	}
	visited[loc] = true; 
	order.push_back(loc);
}

std::vector<std::string> DAG::getDescendants(const std::vector<std::string>& locs){
	// std::cout << "In dag descendants" << std::endl; 
	std::unordered_map<std::string,bool> visited;
	std::vector<std::string> order; 

	for (const auto& loc: locs){
        visited[loc] = false;
    }   
	for (const auto& loc: locs){
        if(!visited[loc]){
            DAG::dfsVisit(loc, visited, order);
        }
    }
    std::reverse(order.begin(),order.end());
    return order;
}

/****************************************************************************************************************************************/

std::vector<std::string> DAG::getImmediateAncestors(const std::vector<std::string>& locs){
	// std::cout << "in dag get immediate ancestors " << std::endl; 
	std::unordered_set<std::string> ancestors;
	for (const auto& loc : locs){
		for (const auto& anc: this->toFromAdjList[loc]){
			ancestors.insert(anc);
		}
	}
	// std::cout << "filled up ancestors" << std::endl; 
	std::vector<std::string> vAncestors(ancestors.begin(),ancestors.end());
	return vAncestors;
}

/****************************************************************************************************************************************/

void DAG::showGraph(){
	std::cout << "=================================================================" << std::endl; 
	std::cout << "From To Adjacency List" << std::endl; 
	DAG::AdjacencyList al = this->fromToAdjList;
	for (DAG::AdjacencyList::iterator it = al.begin(); it != al.end(); ++it){
		std::cout << it->first << ": ";
		for (const auto& toLoc : this->fromToAdjList[it->first])
			std::cout << toLoc << "\t";
		std::cout << std::endl; 
	}
	std::cout << "To From Adjacency List" << std::endl; 
	al = this->toFromAdjList;
	for (DAG::AdjacencyList::iterator it = al.begin(); it != al.end(); ++it){
		std::cout << it->first << ": ";
		for (const auto& fromLoc : this->toFromAdjList[it->first])
			std::cout << fromLoc << "\t";
		std::cout << std::endl; 
	}
	std::cout << "=================================================================" << std::endl; 

}
