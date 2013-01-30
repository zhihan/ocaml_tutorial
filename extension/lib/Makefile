CXX = g++
SOURCES= myvector.cpp
OBJS =$(SOURCES:.cpp=.o) 

%.o: %.cpp
	$(CXX) -c -o $@ $<

libmy.a: $(OBJS)

libmy.a:
	@echo Library: "$@ ( $^ )" 
	@rm -f $@
	@ar cq $@ $^
