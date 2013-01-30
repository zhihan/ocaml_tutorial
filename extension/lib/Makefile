CXX = g++
SOURCES= myvector.cpp
OBJS =$(SOURCES:.cpp=.o) 

# Adding -no-exception to avoid link error on Cygwin

%.o: %.cpp
	$(CXX) -fno-exceptions -c -o $@ $<

libmy.a: $(OBJS)

libmy.a:
	@echo Library: "$@ ( $^ )" 
	@rm -f $@
	@ar cq $@ $^
