#include "colVars.h"

static double col_sum(const double *x, int x_len, int nrow, int narm,
		      int *sample_size)
{
	double xi, sum;
	int i;

	*sample_size = nrow;
	sum = 0.0;
	for (i = 0; i < x_len; i++) {
		xi = x[i];
		if (narm && (R_IsNA(xi) || R_IsNaN(xi))) {
			(*sample_size)--;
			continue;
		}
		sum += xi;
	}
	return sum;
}

static double col_var(const double *x, int x_len, int nrow, int narm)
{
	double xi, sum, mean, sigma, delta;
	int sample_size, i;

	sum = col_sum(x, x_len, nrow, narm, &sample_size);
	mean = sum / (double) sample_size;
	sigma = mean * mean * (nrow - x_len);
	for (i = 0; i < x_len; i++) {
		xi = x[i];
		if (narm && (R_IsNA(xi) || R_IsNaN(xi)))
			continue;
		delta = xi - mean;
		sigma += delta * delta;
	}
	return sigma / (sample_size - 1.0);
}

/* --- .Call ENTRY POINT --- */
SEXP C_dgCMatrix_colVars(SEXP x, SEXP na_rm)
{
	SEXP x_Dim, x_x, x_p, ans;
	int x_nrow, x_ncol, narm, j, offset, count;

	x_Dim = GET_SLOT(x, install("Dim"));
	x_nrow = INTEGER(x_Dim)[0];
	x_ncol = INTEGER(x_Dim)[1];
	x_x = GET_SLOT(x, install("x"));
	x_p = GET_SLOT(x, install("p"));
	narm = LOGICAL(na_rm)[0];

	ans = PROTECT(NEW_NUMERIC(x_ncol));
	for (j = 0; j < x_ncol; j++) {
		offset = INTEGER(x_p)[j];
		count = INTEGER(x_p)[j + 1] - offset;
		REAL(ans)[j] = col_var(REAL(x_x) + offset, count, x_nrow, narm);
	}
	UNPROTECT(1);
	return ans;
}

