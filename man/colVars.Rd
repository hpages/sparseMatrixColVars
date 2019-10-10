\name{colVars}

\alias{dgCMatrix_colVars}

\title{An optimized colVars() for dgCMatrix objects}

\description{
  An optimized \code{colVars()} for dgCMatrix objects implemented in C.
}

\usage{
dgCMatrix_colVars(x, na.rm=FALSE)
}

\arguments{
  \item{x}{
    A dgCMatrix object.
  }
  \item{na.rm}{
    If \code{TRUE}, missing values are excluded.
  }
}

\value{
  A numeric vector of length the number of columns in \code{x}.
}

\seealso{
  \itemize{
    \item The \link[Matrix]{dgCMatrix-class} class in the \pkg{Matrix} package.
  }
  \itemize{
    \item The \link[matrixStats]{colVars} function in the \pkg{matrixStats}
          package.
  }
}

\examples{
library(matrixStats)

## Testing on a 1e6 x 500 matrix with 7% non-zero values
set.seed(123)
m0 <- matrix(0L, nrow=1e6, ncol=500)
nonzeroes <- as.integer(length(m0) * 0.07)
i <- cbind(sample(nrow(m0), nonzeroes, replace=TRUE),
           sample(ncol(m0), nonzeroes, replace=TRUE))
m0[i] <- c(runif(nonzeroes - 300, min=-15, max=15), rep(NA, 300))
m <- as(m0, "dgCMatrix")
                                                          # Time in sec
system.time(cv1 <- apply(m0, 2, var))                     #     5.3
system.time(cv2 <- matrixStats::colVars(m0))              #     1.05
system.time(cv3 <- dgCMatrix_colVars(m))                  #     0.1
stopifnot(all.equal(cv1, cv2), all.equal(cv1, cv3))

system.time(cv1 <- apply(m0, 2, var, na.rm=TRUE))         #     6.9
system.time(cv2 <- matrixStats::colVars(m0, na.rm=TRUE))  #     1.6
system.time(cv3 <- dgCMatrix_colVars(m, na.rm=TRUE))      #     0.3
stopifnot(all.equal(cv1, cv2), all.equal(cv1, cv3))
}

\keyword{manip}
