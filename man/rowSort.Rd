\name{rowSort}

\alias{rowSort}
\alias{baseR_rowNthLargest}
\alias{rowNthLargest}

\title{Row-wise sort}

\description{
  \code{rowSort()} sorts the values within each row of a numeric matrix.
}

\usage{
rowSort(x, decreasing=FALSE)
}

\arguments{
  \item{x}{
    A numeric matrix.
  }
  \item{decreasing}{
    If \code{TRUE}, the values within each row are sorted in decreasing order.
    Note that this has the effect of flipping the returned matrix with
    respect to the vertical axis. More precisely: if \code{y} is
    \code{rowSort(x)} then \code{rowSort(x, decreasing=TRUE)} should
    be identical to \code{y[ , rev(seq_len(ncol(y)))]}.
  }
}

\value{
  A matrix of the same dimensions as \code{x} where the original values
  within each row are sorted.
}

\note{
  \code{NA}'s in the input are not properly handled at the moment.
}

\seealso{
  \itemize{
    \item The \code{\link[base]{sort}} function in the \pkg{base} package.
  }
}

\examples{
## A simple but inefficient implementation of rowSort():
simple_rowSort <- function(x, decreasing=FALSE)
{
    sorted_rows <- lapply(seq_len(nrow(x)),
                          function(i) sort(x[i, ], decreasing=decreasing))
    ans <- matrix(unlist(sorted_rows), nrow=nrow(x), byrow=TRUE)
    rownames(ans) <- rownames(x)
    ans
}

## With a matrix of integers
## -------------------------

set.seed(123)
m <- matrix(sample(15L, 72, replace=TRUE), ncol=6,
            dimnames=list(letters[1:12], LETTERS[1:6]))
m
rowSort(m)
rowSort(m, decreasing=TRUE)

## Sanity checks:
m1 <- simple_rowSort(m)
m2 <- rowSort(m)
stopifnot(identical(m1, m2))

m1r <- simple_rowSort(m, decreasing=TRUE)
m2r <- rowSort(m, decreasing=TRUE)
m2flipped <- m2[ , rev(seq_len(ncol(m2)))]
stopifnot(identical(m2flipped, m1r))
stopifnot(identical(m2flipped, m2r))

## With a matrix of doubles
## ------------------------

set.seed(123)
m <- matrix(runif(72), ncol=6,
            dimnames=list(letters[1:12], LETTERS[1:6]))
m
rowSort(m)
rowSort(m, decreasing=TRUE)

## Sanity checks:
m1 <- simple_rowSort(m)
m2 <- rowSort(m)
stopifnot(identical(m1, m2))

m1r <- simple_rowSort(m, decreasing=TRUE)
m2r <- rowSort(m, decreasing=TRUE)
m2flipped <- m2[ , rev(seq_len(ncol(m2)))]
stopifnot(identical(m2flipped, m1r))
stopifnot(identical(m2flipped, m2r))
}

\keyword{manip}
