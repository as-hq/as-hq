OPTIONS = -Wall -g
LIBS = -lc -ldl
C_FLAGS = -fPIC -rdynamic $(OPTIONS) -DLINUX -std=c++0x

LIBTGT = libsendao.a
SO_FLAGS = -shared -Wl,-soname,$(LIBTGT)

OBJECTS = list.o array.o attribs.o util.o io.o string.o system.o tables.o lists.o objects.o

$(LIBTGT): $(OBJECTS)
	ar -cr $(LIBTGT) $(OBJECTS)
#	g++ $(SO_FLAGS) -o $(LIBTGT) $(OBJECTS) $(LIBS)
	cp $(LIBTGT) ..

.cpp.o:
	g++ $(C_FLAGS) -c $<


clean:
	rm -f *.o $(LIBTGT) /tmp/$(LIBTGT)
