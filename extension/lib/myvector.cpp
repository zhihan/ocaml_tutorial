#include "myvector.h"
#include "myvector.hpp"

realVector* newVector(void)
{
  return new realVector;
}

void deleteVector(realVector* r) 
{
  delete r;
}

double vector_get(realVector* v, int i)
{
  return (*v)[i];
}

void vector_append(realVector* v, double x)
{
  (*v).push_back(x);
}

int vector_size(realVector* v)
{
    return static_cast<int>((*v).size());
}
