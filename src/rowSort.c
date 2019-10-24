#include "rowSort.h"

#include <Rdefines.h>
#include <stdlib.h>  /* for qsort() */

static size_t size_of_Rtype(SEXPTYPE Rtype)
{
	switch (Rtype) {
	    case RAWSXP:  return sizeof(Rbyte);
	    case INTSXP:  return sizeof(int);
	    case REALSXP: return sizeof(double);
	    case CPLXSXP: return sizeof(Rcomplex);
	}
	error("%s type not supported", CHAR(type2str(Rtype)));
}

static void get_row_ints(const int *x, int x_nrow, int x_ncol, int i,
			 int *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		row_buf[j] = x[offset];
	return;
}

static void get_row_doubles(const double *x, int x_nrow, int x_ncol, int i,
			    double *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		row_buf[j] = x[offset];
	return;
}

static void set_row_ints(int *x, int x_nrow, int x_ncol, int i,
			 const int *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		x[offset] = row_buf[j];
	return;
}

static void set_row_doubles(double *x, int x_nrow, int x_ncol, int i,
			    const double *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		x[offset] = row_buf[j];
	return;
}

static int cmp_ints(const void *p1, const void *p2)
{
	int x1, x2;

	x1 = *((const int *) p1);
	x2 = *((const int *) p2);
	if (x1 == x2)
		return 0;
	return x1 > x2 ? 1 : -1;
}

static int cmp_doubles(const void *p1, const void *p2)
{
	double x1, x2;

	x1 = *((const double *) p1);
	x2 = *((const double *) p2);
	if (x1 == x2)
		return 0;
	return x1 > x2 ? 1 : -1;
}

static int cmp_ints_desc(const void *p1, const void *p2)
{
	return cmp_ints(p2, p1);
}

static int cmp_doubles_desc(const void *p1, const void *p2)
{
	return cmp_doubles(p2, p1);
}

static void qsort_ints(int *x, int x_len, int desc)
{
	if (desc) {
		qsort(x, x_len, sizeof(int), cmp_ints_desc);
	} else {
		qsort(x, x_len, sizeof(int), cmp_ints);
	}
	return;
}

static void qsort_doubles(double *x, int x_len, int desc)
{
	if (desc) {
		qsort(x, x_len, sizeof(double), cmp_doubles_desc);
	} else {
		qsort(x, x_len, sizeof(double), cmp_doubles);
	}
	return;
}

/* --- .Call ENTRY POINT ---
 * Does not handle NAs properly yet.
 */
SEXP C_rowSort(SEXP x, SEXP decreasing)
{
	SEXP x_dim, ans;
	int x_nrow, x_ncol, desc, i;
	void *row_buf;

	x_dim = GET_DIM(x);
	x_nrow = INTEGER(x_dim)[0];
	x_ncol = INTEGER(x_dim)[1];
	desc = LOGICAL(decreasing)[0];
	row_buf = R_alloc(size_of_Rtype(TYPEOF(x)), x_ncol);

	ans = PROTECT(duplicate(x));
	switch (TYPEOF(x)) {
	    case INTSXP:
		for (i = 0; i < x_nrow; i++) {
			get_row_ints(INTEGER(x), x_nrow, x_ncol, i,
				     (int *) row_buf);
			qsort_ints(row_buf, x_ncol, desc);
			set_row_ints(INTEGER(ans), x_nrow, x_ncol, i,
				     (const int *) row_buf);
		}
		break;
	    case REALSXP:
		for (i = 0; i < x_nrow; i++) {
			get_row_doubles(REAL(x), x_nrow, x_ncol, i,
					(double *) row_buf);
			qsort_doubles(row_buf, x_ncol, desc);
			set_row_doubles(REAL(ans), x_nrow, x_ncol, i,
					(const double *) row_buf);
		}
		break;
	    default:
		error("%s type not supported", CHAR(type2str(TYPEOF(x))));
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * Does not handle NAs properly yet.
 */
#define GET_NTH_ELT(i) (nth_len == 1 ? INTEGER(nth)[0] : INTEGER(nth)[i])

SEXP C_rowNthLargest(SEXP x, SEXP nth)
{
	SEXP x_dim, ans;
	int x_nrow, x_ncol, nth_len, i, n;
	void *row_buf;

	x_dim = GET_DIM(x);
	x_nrow = INTEGER(x_dim)[0];
	x_ncol = INTEGER(x_dim)[1];
	nth_len = LENGTH(nth);
	if (nth_len != 1 && nth_len != x_nrow)
		error("invalid 'nth'");
	row_buf = R_alloc(size_of_Rtype(TYPEOF(x)), x_ncol);

	ans = PROTECT(allocVector(TYPEOF(x), x_nrow));
	switch (TYPEOF(x)) {
	    case INTSXP:
		for (i = 0; i < x_nrow; i++) {
			get_row_ints(INTEGER(x), x_nrow, x_ncol, i,
				     (int *) row_buf);
			qsort_ints(row_buf, x_ncol, 1);
			n = GET_NTH_ELT(i);
			if (n == NA_INTEGER || n < 1 || n > x_ncol) {
				UNPROTECT(1);
				error("'nth' must contain non-NA values "
				      "that are >= 1 and <= ncol(x)");
			}
			INTEGER(ans)[i] = ((const int *) row_buf)[n - 1];
		}
		break;
	    case REALSXP:
		for (i = 0; i < x_nrow; i++) {
			get_row_doubles(REAL(x), x_nrow, x_ncol, i,
					(double *) row_buf);
			qsort_doubles(row_buf, x_ncol, 1);
			n = GET_NTH_ELT(i);
			if (n == NA_INTEGER || n < 1 || n > x_ncol) {
				UNPROTECT(1);
				error("'nth' must contain non-NA values "
				      "that are >= 1 and <= ncol(x)");
			}
			REAL(ans)[i] = ((const double *) row_buf)[n - 1];
		}
		break;
	    default:
		error("%s type not supported", CHAR(type2str(TYPEOF(x))));
	}
	UNPROTECT(1);
	return ans;
}

