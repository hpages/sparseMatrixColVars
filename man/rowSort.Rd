\name{rowSort}

\alias{simple_rowSort}
\alias{rowSort}

\alias{simple_rowNthLargest}
\alias{baseR_rowNthLargest}
\alias{rowNthLargest}

\title{Row-wise sort}

\description{
  \code{rowSort()} sorts the values within each row of a numeric matrix.

  \code{rowNthLargest()} returns the n-th largest value in each row of a
  numeric matrix.
}

\usage{
rowSort(x, decreasing=FALSE, use.radix.sort=NA)

rowNthLargest(x, nth=1L)
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
  \item{use.radix.sort}{
    Only supported when \code{x} is a matrix of integers (i.e. when
    \code{storage.mode(x) == "integer"}).

    If \code{TRUE}, a radix sort is used. If \code{FALSE}, \code{qsort()}
    from the standard C library is used. If \code{NA}, the sorting algorithm
    is automatically choosen based on the number of columns in the matrix:
    radix if more than 1024 columns and \code{qsort()} otherwise.
  }
  \item{nth}{
    The rank of the largest value to pick up in each row e.g. if set to
    1 the largest value is picked, if set to 2 the 2nd largest value is
    picked, etc...

    Must be a single integer or an integer vector of length the number of rows
    in \code{x}. Its value (or values) must be >= 1 and <= \code{ncol(x)}.
  }
}

\value{
  \code{rowSort()}: A matrix of the same dimensions as \code{x} where the
  original values are sorted within each row.

  \code{rowNthLargest()}: A numeric (or integer) vector with one element
  per row in \code{x}.
}

\note{
  \code{NA}'s in the input are not properly handled at the moment.
}

\seealso{
  \itemize{
    \item The \code{\link[base]{sort}} function in the \pkg{base} package.
  }
  \itemize{
    \item The \code{\link[matrixStats]{rowMaxs}} function in the
          \pkg{matrixStats} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## SIMPLE EXAMPLES
## ---------------------------------------------------------------------

## With a matrix of integers:
set.seed(123)
m1 <- matrix(sample(17L, 84, replace=TRUE), ncol=7,
             dimnames=list(letters[1:12], LETTERS[1:7]))
m1

rowSort(m1)
rowSort(m1, decreasing=TRUE)
rowNthLargest(m1)     # Largest value in each row.
rowNthLargest(m1, 4)  # 4th largest value in each row.
rowNthLargest(m1, c(7:2, 2:7))  # 7th largest value from 1st row,
                                # 6th from 2nd row,
                                # 5th from 3rd row, etc...

## With a matrix of doubles:
set.seed(123)
m2 <- matrix(runif(72, min=-50, max=50), ncol=6,
             dimnames=list(letters[1:12], LETTERS[1:6]))
m2

rowSort(m2)
rowSort(m2, decreasing=TRUE)
rowNthLargest(m2, 2)
rowNthLargest(m2, c(1:6, 1:6))

## ---------------------------------------------------------------------
## SANITY CHECKS
## ---------------------------------------------------------------------
library(matrixStats)  # for matrixStats::rowMaxs()

## Matrix of integers:
m1a <- simple_rowSort(m1)
m1b <- rowSort(m1)
stopifnot(identical(m1a, m1b))

m1c <- simple_rowSort(m1, decreasing=TRUE)
m1d <- rowSort(m1, decreasing=TRUE)
m1bflipped <- m1b[ , rev(seq_len(ncol(m1b)))]
stopifnot(identical(m1bflipped, m1c))
stopifnot(identical(m1bflipped, m1d))

res1a <- simple_rowNthLargest(m1)
res1b <- baseR_rowNthLargest(m1)
res1c <- rowSort(m1, decreasing=TRUE)[ , 1L]
res1d <- matrixStats::rowMaxs(m1)
res1e <- rowNthLargest(m1)
stopifnot(identical(res1a, res1b))
stopifnot(identical(res1a, res1c))
stopifnot(identical(unname(res1a), res1d))
stopifnot(identical(res1a, res1e))

res1f <- simple_rowNthLargest(m1, 4)
res1g <- baseR_rowNthLargest(m1, 4)
res1h <- rowSort(m1, decreasing=TRUE)[ , 4L]
res1i <- rowNthLargest(m1, 4)
stopifnot(identical(res1f, res1g))
stopifnot(identical(res1f, res1h))
stopifnot(identical(res1f, res1i))

idx <- cbind(seq_len(nrow(m1)), c(7:2, 2:7))
res1j <- rowSort(m1, decreasing=TRUE)[idx]
res1k <- rowNthLargest(m1, c(7:2, 2:7))
stopifnot(identical(res1j, unname(res1k)))

## Matrix of doubles:
m2a <- simple_rowSort(m2)
m2b <- rowSort(m2)
stopifnot(identical(m2a, m2b))

m2c <- simple_rowSort(m2, decreasing=TRUE)
m2d <- rowSort(m2, decreasing=TRUE)
m2bflipped <- m2b[ , rev(seq_len(ncol(m2b)))]
stopifnot(identical(m2bflipped, m2c))
stopifnot(identical(m2bflipped, m2d))

res2a <- simple_rowNthLargest(m2)
res2b <- baseR_rowNthLargest(m2)
res2c <- rowSort(m2, decreasing=TRUE)[ , 1L]
res2d <- matrixStats::rowMaxs(m2)
res2e <- rowNthLargest(m2)
stopifnot(identical(res2a, res2b))
stopifnot(identical(res2a, res2c))
stopifnot(identical(unname(res2a), res2d))
stopifnot(identical(res2a, res2e))

res2f <- simple_rowNthLargest(m2, 5)
res2g <- baseR_rowNthLargest(m2, 5)
res2h <- rowSort(m2, decreasing=TRUE)[ , 5L]
res2i <- rowNthLargest(m2, 5)
stopifnot(identical(res2f, res2g))
stopifnot(identical(res2f, res2h))
stopifnot(identical(res2f, res2i))

idx <- cbind(seq_len(nrow(m2)), c(1:6, 1:6))
res2j <- rowSort(m2, decreasing=TRUE)[idx]
res2k <- rowNthLargest(m2, c(1:6, 1:6))
stopifnot(identical(res2j, unname(res2k)))
}

\keyword{manip}
