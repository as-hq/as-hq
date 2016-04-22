CC = g++
CC_FLAGS = -w -lzmq -std=c++11 -lboost_regex -lpthread

EXEC = server
SOURCES = location.cpp graph.cpp server.cpp
OBJECTS = $(SOURCES:.cpp=.o)

# Main target
$(EXEC): $(OBJECTS)
	$(CC) $(CC_FLAGS) $(OBJECTS) -o $(EXEC)

%.o: %.cpp
	$(CC) -c $(CC_FLAGS) $< -o $@

clean:
	rm -f $(EXEC) $(OBJECTS)

valgrind:
	valgrind --tool=memcheck --leak-check=yes --show-reachable=yes ./$(EXEC)
