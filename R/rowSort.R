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

### A fast pure base R implementation of rowSort(x)[ , ncol(x) - nth + 1L]
baseR_rowNthLargest <- function(x, nth=1L)
{
    stopifnot(is.matrix(x),
              is.numeric(nth), length(nth) == 1L, !is.na(nth))
    if (!is.integer(nth))
        nth <- as.integer(nth)
    x_nrow <- nrow(x)
    x_ncol <- ncol(x)
    if (nth < 1L || nth > x_ncol)
        stop("'nth' must be >= 1 and <= ncol(x)")

    row_seq <- seq_len(x_nrow)
    ## Order first by row, then by value within the row.
    ## Note that 'rep.int(row_seq, x_ncol)' is equivalent to 'base::row(x)'
    ## but slightly faster.
    oo <- order(rep.int(row_seq, x_ncol), x)
    idx <- oo[row_seq * x_ncol - nth + 1L]
    x[idx]
}

### An optimized implementation of rowSort(x)[ , ncol(x) - nth + 1L]
rowNthLargest <- function(x, nth=1L)
{
    stopifnot(is.matrix(x),
              storage.mode(x) %in% c("integer", "double"),
              is.numeric(nth))
    if (!is.integer(nth))
        nth <- as.integer(nth)

    .Call("C_rowNthLargest", x, nth, PACKAGE="sparseMatrixColVars")
}

