#include "rowSort.h"

#include <Rdefines.h>
#include <stdlib.h>  /* for qsort() */
#include "S4Vectors_interface.h"

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

static void get_int_row(const int *x, int x_nrow, int x_ncol, int i,
			int *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		row_buf[j] = x[offset];
	return;
}

static void get_double_row(const double *x, int x_nrow, int x_ncol, int i,
			   double *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		row_buf[j] = x[offset];
	return;
}

static void set_int_row(int *x, int x_nrow, int x_ncol, int i,
			const int *row_buf)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		x[offset] = row_buf[j];
	return;
}

static void set_double_row(double *x, int x_nrow, int x_ncol, int i,
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

static void qsort_int_row(const int *x, int x_nrow, int x_ncol, int i,
			  int desc, int *out, int *row_buf)
{
	get_int_row(x, x_nrow, x_ncol, i, row_buf);
	qsort_ints(row_buf, x_ncol, desc);
	set_int_row(out, x_nrow, x_ncol, i, row_buf);
	return;
}

static void qsort_double_row(const double *x, int x_nrow, int x_ncol, int i,
			     int desc, double *out, double *row_buf)
{
	get_double_row(x, x_nrow, x_ncol, i, row_buf);
	qsort_doubles(row_buf, x_ncol, desc);
	set_double_row(out, x_nrow, x_ncol, i, row_buf);
	return;
}

static void rxsort_int_row(const int *x, int x_nrow, int x_ncol, int i,
			   int desc, int *out, int *base,
			   unsigned short int *rxbuf1, int *rxbuf2)
{
	int j;
	long long int offset;

	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		base[j] = offset;
	sort_ints(base, x_ncol, x, desc, 1, rxbuf1, rxbuf2);
	for (j = 0, offset = i; j < x_ncol; j++, offset += x_nrow)
		out[offset] = x[base[j]];
	return;
}

static SEXP get_rownames(SEXP x)
{
	SEXP x_dimnames;

	x_dimnames = GET_DIMNAMES(x);
	if (x_dimnames == R_NilValue)
		return R_NilValue;
	return VECTOR_ELT(x_dimnames, 0);
}

/* --- .Call ENTRY POINT ---
 * Does not handle NAs properly yet.
 */
SEXP C_rowSort(SEXP x, SEXP decreasing, SEXP use_radix_sort)
{
	SEXP x_dim, ans, x_rownames, ans_dimnames;
	int x_nrow, x_ncol, desc, i;
	void *row_buf;
	int use_rx, *rxbuf2 = NULL;
	unsigned short int *rxbuf1 = NULL;

	x_dim = GET_DIM(x);
	x_nrow = INTEGER(x_dim)[0];
	x_ncol = INTEGER(x_dim)[1];
	desc = LOGICAL(decreasing)[0];
	row_buf = R_alloc(size_of_Rtype(TYPEOF(x)), x_ncol);

	use_rx = LOGICAL(use_radix_sort)[0];
	if (TYPEOF(x) == INTSXP) {
		/* Radix sort starts to be beneficial when the number of cols
		   is > 1024 (based on empirical results on my laptop).
		   The benefits seems minor though unless the number of cols
		   is really large (i.e. > 1e6) */
		if (use_rx == NA_INTEGER)
			use_rx = x_ncol > 1024;
		if (use_rx) {
			rxbuf1 = (unsigned short int *)
				 R_alloc(sizeof(unsigned short int), x_ncol);
			rxbuf2 = (int *) R_alloc(sizeof(int), x_ncol);
		}
	} else {
		if (use_rx != NA_INTEGER)
			warning("'use.radix.sort' is only supported "
				"when 'storage.mode(x)'\n  "
				"is \"integer\" so was ignored");
	}

	ans = PROTECT(allocMatrix(TYPEOF(x), x_nrow, x_ncol));
	switch (TYPEOF(x)) {
	    case INTSXP:
		if (use_rx) {
			/* Radix sort */
			for (i = 0; i < x_nrow; i++)
				rxsort_int_row(
					INTEGER(x), x_nrow, x_ncol, i, desc,
					INTEGER(ans), (int *) row_buf,
					rxbuf1, rxbuf2);
		} else {
			/* qsort() */
			for (i = 0; i < x_nrow; i++)
				qsort_int_row(
					INTEGER(x), x_nrow, x_ncol, i, desc,
					INTEGER(ans), (int *) row_buf);
		}
		break;
	    case REALSXP:
		for (i = 0; i < x_nrow; i++)
			qsort_double_row(
				REAL(x), x_nrow, x_ncol, i, desc,
				REAL(ans), (double *) row_buf);
		break;
	    default:
		error("%s type not supported", CHAR(type2str(TYPEOF(x))));
	}

	/* Propagate rownames */
	x_rownames = get_rownames(x);
	if (x_rownames != R_NilValue) {
		ans_dimnames = PROTECT(NEW_LIST(2));
		SET_VECTOR_ELT(ans_dimnames, 0, x_rownames);
		SET_DIMNAMES(ans, ans_dimnames);
		UNPROTECT(1);
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
	SEXP x_dim, x_rownames, ans;
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
			get_int_row(INTEGER(x), x_nrow, x_ncol, i,
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
			get_double_row(REAL(x), x_nrow, x_ncol, i,
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

	/* Propagate rownames */
	x_rownames = get_rownames(x);
	if (x_rownames != R_NilValue)
		SET_NAMES(ans, x_rownames);

	UNPROTECT(1);
	return ans;
}

