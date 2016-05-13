OPTIONS = -g -rdynamic -ggdb -O3
D_FLAGS =
C_FLAGS = -Wall $(OPTIONS) $(D_FLAGS) -I./libsendao
L_FLAGS = -L/home/sendao/libsendao -Wall $(OPTIONS)
LIBS = -lsendao


CLIOBJS = deploy.o pipe.o scripts.o

deploy: $(CLIOBJS)
	rm -f deploy
	g++ $(L_FLAGS) -o deploy $(CLIOBJS) $(LIBS)

clean:
	rm -f $(CLIOBJS)
	rm -f deploy

.cpp.o:
	g++ -c $(C_FLAGS) $<
	
libsendao.a:
	make -C libsendao

