rowSort <- function(x, decreasing=FALSE)
{
    stopifnot(is.matrix(x),
              storage.mode(x) %in% c("integer", "double"),
              is.logical(decreasing), length(decreasing) == 1L,
                                      !is.na(decreasing))
    .Call("C_rowSort", x, decreasing, PACKAGE="sparseMatrixColVars")
}

