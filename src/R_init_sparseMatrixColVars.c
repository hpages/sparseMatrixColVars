#include "colVars.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

static const R_CallMethodDef callMethods[] = {

/* colVars.c */
	CALLMETHOD_DEF(C_dgCMatrix_colVars, 2),

	{NULL, NULL, 0}
};

void R_init_sparseMatrixColVars(DllInfo *info)
{
        R_registerRoutines(info, NULL, callMethods, NULL, NULL);
        return;
}

