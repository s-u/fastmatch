\name{coalesce}
\alias{coalesce}
\title{
  Create an index that groups unique values together
}
\description{
  \code{coalesce} makes sure that a given index vector is coalesced,
  i.e., identical values are grouped into contiguous blocks. This can be
  used as a much faster alternative to \code{\link{sort.list}} where the
  goal is to group identical values, but not necessarily in a
  pre-defined order. The algorithm is linear in the length of the vector.
}
\usage{
  coalesce(x)
}
\arguments{
  \item{x}{character, integer or real vector to coalesce}
}
\details{
  The current implementation takes two passes through the vector. In the
  first pass it creates a hash table for the values of \code{x} counting
  the occurrences in the process. In the second pass it assigns indices
  for every element based on the index stored in the hash table.

  The order of the groups of unique values is defined by the first
  occurence of each unique value, hence it is identical to the order of
  \code{\link{unique}}.

  One common use of \code{coalesce} is to allow the use of arbitrary
  vectors in \code{\link{ctapply}} via
  \code{ctapply(x[coalesce(x)], ...)}.
}
\value{
  Integer vector with the resulting permutation. \code{x[coalesce(x)]}
  gives \code{x} with contiguous unique values.
}
%\references{
%}
\author{
Simon Urbanek
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{unique}}, \code{\link{sort.list}}, \code{\link{ctapply}}
}
\examples{
i = rnorm(2e6)
names(i) = as.integer(rnorm(2e6))
## compare sorting and coalesce
system.time(o <- i[order(names(i))])
system.time(o <- i[coalesce(names(i))])

## more fair comparison taking the coalesce time (and copy) into account
system.time(tapply(i, names(i), sum))
system.time({ o <- i[coalesce(names(i))]; ctapply(o, names(o), sum) })

## in fact, using ctapply() on a dummy vector is faster than table() ...
## believe it or not ... (that that is actually wasteful, since coalesce
## already computed the table internally anyway ...)
ftable <- function(x) {
   t <- ctapply(rep(0L, length(x)), x[coalesce(x)], length)
   t[sort.list(names(t))]
}
system.time(table(names(i)))
system.time(ftable(names(i)))
}
\keyword{manip}
