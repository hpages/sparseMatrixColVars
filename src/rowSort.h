#ifndef _ROWSORT_H_
#define _ROWSORT_H_

#include <Rinternals.h>

SEXP C_rowSort(SEXP x, SEXP decreasing);
SEXP C_rowNthLargest(SEXP x, SEXP nth);

#endif /* _ROWSORT_H_ */
