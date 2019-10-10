dgCMatrix_colVars <- function(x, na.rm=FALSE)
{
    stopifnot(is(x, "dgCMatrix"),
              is.logical(na.rm), length(na.rm) == 1L, !is.na(na.rm))
    .Call("C_dgCMatrix_colVars", x, na.rm, PACKAGE="sparseMatrixColVars")
}

