\name{ctapply}
\alias{ctapply}
\title{
Fast tapply() replacement functions
}
\description{
  \code{ctapply} is a fast replacement of \code{tapply} that assumes
  contiguous input, i.e. unique values in the index are never speparated
  by any other values. This avoids an expensive \code{split} step since
  both value and the index chungs can be created on the fly. This
  makes it many orders of magnitude faster than the classical
  \code{lapply(split(), ...)} implementation.
}
\usage{
ctapply(X, INDEX, FUN, ..., MERGE=c, .SAFE=TRUE)
}
\arguments{
  \item{X}{an atomic object, typically a vector}
  \item{INDEX}{numeric or character vector of the same length as \code{X}}
  \item{FUN}{the function to be applied}
  \item{...}{additional arguments to \code{FUN}. They are passed as-is,
    i.e., without replication or recycling}
  \item{MERGE}{function to merge the resulting vector or \code{NULL} if
    the arguments to such a function are to be returned instead}
  \item{.SAFE}{logical, if \code{TRUE} then fresh regular vectors are
    allocated for each call of \code{FUN} which makes it safe to be
    used with any \code{FUN}. If \code{FALSE} then both the index and
    value vectors use cached resizable vectors, but this imposes
    constraints on \code{FUN} (see below).}
}
\details{
  Note that \code{ctapply} supports either integer, real or character
  vectors as indices (note that factors are integer vectors and thus
  supported, but you do not need to convert character vectors). Unlike
  \code{tapply} it does not take a list of factors - if you want to use
  a cross-product of factors, create the product first, e.g. using
  \code{paste(i1, i2, i3, sep='\01')} or multiplication - whetever
  method is convenient for the input types.

  \code{ctapply} requires the \code{INDEX} to contiguous. One (slow) way
  to achieve that is to use \code{\link{sort}} or \code{\link{order}}.

  If \code{.SAFE=FALSE} then both index and value vectors will be
  re-used in subsequent iterations and created as growable vectors,
  thus avoiding allocations at each iteration. However, this means that
  \code{FUN} may not directly store the value it is being passed, it can
  only compute on it and store/return derived values (e.g., it is safe
  to use with functions like \code{sum}). This approach can be faster
  for large \code{X} if \code{INDEX} consists of many small groups, but
  enable only if you have direct control over \code{FUN} and know that
  it is safe to use this way.
}
%\value{
%}
%\references{
%}
\author{
Simon Urbanek
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{tapply}}
}
\examples{
i = rnorm(4e6)
names(i) = as.integer(rnorm(1e6))
i = i[order(names(i))]
system.time(tapply(i, names(i), sum))
system.time(ctapply(i, names(i), sum))
}
\keyword{manip}
