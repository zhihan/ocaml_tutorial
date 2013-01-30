#ifndef myvector_h
#define myvector_h

/* This is a standard C header file*/

#ifdef __cplusplus
extern "C" {
#endif

  struct realVector;
  typedef struct realVector realVector;

  realVector* newVector(void);
  void deleteVector(realVector* );
  double vector_get(realVector* , int);
  void vector_append(realVector* , double);

#ifdef __cplusplus
}
#endif
#endif
