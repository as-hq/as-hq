module TestDAG () where
	import ASDAG as D
	import qualified ASTypes as A  
	test = [(1,2),(1,3),(2,4),(4,3), (2,6), (5,1)]

	a :: [A.ASLocation] --list of ASLocations 
	a = map A.indexl [(2,1), (3,1), (4,1), (3,2)]

	b :: [A.ASLocation] --list of ASLocations 
	b = map A.indexl [(1,2), (4,5), (1,2), (2,1)]

	dag :: Relation A.ASLocation --list of (ASLocation, ASLocation)
	dag = zip a b 

	--testing if decomposeLocs works correctly; should give (1,2), (1,3) ... (3,4)
	d = A.decomposeLocs [A.rangel ((1,2),(3,4))]

	--note: we use Data.Set in this implemention; needs Ord ASLocation to use BST-type internal structure
	test0 = updateDAG dag (A.indexl(1,6),[A.indexl (1,4),A.indexl (1,5)])

	--should replace (2,4) and (2,6) 
	test1=updateDAG test (2,[6,7,8,10])

	--should delete all edges of form (1,x)
	test2=deleteDAG test 1

	--nothing depends on cell 2 now
	test3=updateDAG test (2,[])

	--don't include (1,1)
	test4=updateDAG test (1,[1,5])

	--A1=B1+C1
	--B1=C1+D1
	--E1=A1+B1
	--if B1 is changed, A1 should be changed first, then E1; descendants = [2,1,5]
	--B1 depends on C1 and D1; ancestors = [2,3,4]
	test5 =  updateDAG (updateDAG (updateDAG [] (1,[2,3])) (2,[3,4])) (5,[1,2])
	test5a = ancestors 2 test5
	test5d  = descendants 2 test5 

	--should detect cycle
	test6 = [(1,2)]
	test6u = updateDAG test6 (2,[1])

	--test: A1=sum(B1:B2)+C1, C1=2*B1, B2=B_1^2
	test7a=updateDAG [] (A.indexl (1,1),A.decomposeLocs[A.rangel ((2,1),(2,2)), A.indexl(3,1)])
	test7b=updateDAG test7a (A.indexl(3,1),[A.indexl(2,1)])
	test7c=updateDAG test7b (A.indexl(2,2),[A.indexl(2,1)])

	--now we update B1; update order: B1, (B2,C1), A1 
	test7d= descendants (A.indexl(2,1)) test7c 
	test7e= ancestors (A.indexl(2,1)) test7c --things that B1 depends on (should be only B1)

	--test8: just checking if updateDAG works as expected for ASLocation things
	test8a = updateDAG [] (A.indexl (1,1),A.decomposeLocs[A.rangel ((2,1),(3,2)), A.indexl(4,3)])
	test8b = updateDAG test8a (A.indexl (1,1),[A.indexl (2,2)]) --now A1 only depends on B2, nothing else
