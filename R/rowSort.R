### =========================================================================
### rowSort() and related functions
### -------------------------------------------------------------------------


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rowSort()
###

## A simple but inefficient implementation of rowSort().
simple_rowSort <- function(x, decreasing=FALSE)
{
    sorted_rows <- lapply(seq_len(nrow(x)),
                          function(i) sort(x[i, ], decreasing=decreasing))
    ans <- matrix(unlist(sorted_rows), nrow=nrow(x), byrow=TRUE)
    rownames(ans) <- rownames(x)
    ans
}

### Setting 'decreasing' to TRUE has the effect of flipping the returned
### matrix with respect to the vertical axis. More precisely: if x1 is
### rowSort(x) then rowSort(x, decreasing=TRUE) should be identical to
### x1[ , rev(seq_len(ncol(x1)))]
rowSort <- function(x, decreasing=FALSE, use.radix.sort=NA)
{
    stopifnot(is.matrix(x),
              storage.mode(x) %in% c("integer", "double"),
              is.logical(decreasing), length(decreasing) == 1L,
                                      !is.na(decreasing),
              is.logical(use.radix.sort), length(use.radix.sort) == 1L)

    .Call("C_rowSort", x, decreasing, use.radix.sort,
                       PACKAGE="sparseMatrixColVars")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### rowNthLargest()
###
### rowNthLargest(x, nth) returns the n-th largest value in each row i.e.
### rowSort(x, decreasing=TRUE)[ , nth]
###

.normarg_nth <- function(nth, x_ncol)
{
    if (!isSingleNumber(nth))
        stop("'nth' must be a single integer")
    if (!is.integer(nth))
        nth <- as.integer(nth)
    if (nth < 1L || nth > x_ncol)
        stop("'nth' must be >= 1 and <= ncol(x)")
    nth
}

### A simple but inefficient implementation of rowNthLargest().
simple_rowNthLargest <- function(x, nth=1L)
{
    stopifnot(is.matrix(x))
    x_ncol <- ncol(x)
    nth <- .normarg_nth(nth, x_ncol)

    apply(x, 1,
          function(row) {
              n <- x_ncol - nth + 1L
              sort(row, partial=n)[n]
          })
}

### A fast pure base R implementation of rowNthLargest().
baseR_rowNthLargest <- function(x, nth=1L)
{
    stopifnot(is.matrix(x))
    x_nrow <- nrow(x)
    x_ncol <- ncol(x)
    nth <- .normarg_nth(nth, x_ncol)

    row_seq <- seq_len(x_nrow)
    ## Order first by row, then by value within the row.
    ## Note that 'rep.int(row_seq, x_ncol)' is equivalent to 'base::row(x)'
    ## but slightly faster.
    oo <- order(rep.int(row_seq, x_ncol), x)
    idx <- oo[row_seq * x_ncol - nth + 1L]
    setNames(x[idx], rownames(x))
}

### Optimized implementation.
### Supports an 'nth' of length the number of rows in 'x'.
rowNthLargest <- function(x, nth=1L)
{
    stopifnot(is.matrix(x),
              storage.mode(x) %in% c("integer", "double"),
              is.numeric(nth))
    if (!is.integer(nth))
        nth <- as.integer(nth)

    .Call("C_rowNthLargest", x, nth, PACKAGE="sparseMatrixColVars")
}

