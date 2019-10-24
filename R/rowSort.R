### Setting 'decreasing' to TRUE has the effect of flipping the returned
### matrix with respect to the vertical axis. More precisely: if x1 is
### rowSort(x) then rowSort(x, decreasing=TRUE) should be identical to
### x1[ , rev(seq_len(ncol(x1)))]
rowSort <- function(x, decreasing=FALSE)
{
    stopifnot(is.matrix(x),
              storage.mode(x) %in% c("integer", "double"),
              is.logical(decreasing), length(decreasing) == 1L,
                                      !is.na(decreasing))
    .Call("C_rowSort", x, decreasing, PACKAGE="sparseMatrixColVars")
}

### An optimized implementation of rowSort(x)[ , ncol(x) - nth + 1L]
rowNthLargest <- function(x, nth=1L)
{
    stopifnot(is.matrix(x),
              storage.mode(x) %in% c("integer", "double"),
              is.integer(nth))
    .Call("C_rowNthLargest", x, nth, PACKAGE="sparseMatrixColVars")
}

### A fast pure base R implementation of the above.
baseR_rowNthLargest <- function(x, nth=1L)
{
    ## order first by row, then by value within the row
    oo <- order(rep.int(seq_len(nrow(x)), ncol(x)), x)
    i <- oo[seq_len(nrow(x)) * ncol(x) - nth + 1L]
    x[i]
}

