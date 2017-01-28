\name{fmatch}
\alias{fmatch}
\alias{\%fin\%}
\alias{fmatch.hash}
\alias{fastmatch}
\title{
Fast match() replacement
}
\description{
\code{fmatch} is a faster version of the built-in \code{\link{match}}()
function. It is slightly faster than the built-in version because it
uses more specialized code, but in addition it retains the hash table
within the table object such that it can be re-used, dramatically reducing
the look-up time especially for large tables.

Although \code{fmatch} can be used separately, in general it is also
safe to use: \code{match <- fmatch} since it is a drop-in
replacement. Any cases not directly handled by \code{fmatch} are passed
to \code{match} with a warning.

\code{fmatch.hash} is identical to \code{fmatch} but it returns the table
object with the hash table attached instead of the result, so it can be
used to create a table object in cases where direct modification is
not possible.

\code{\%fin\%} is a version of the built-in \code{\link{\%in\%}} function
that uses \code{fmatch} instead of \code{\link{match}}().
}
\usage{
fmatch(x, table, nomatch = NA_integer_, incomparables = NULL)
fmatch.hash(x, table, nomatch = NA_integer_, incomparables = NULL)
x \%fin\% table
}
\arguments{
  \item{x}{values to be matched}
  \item{table}{values to be matched against}
  \item{nomatch}{the value to be returned in the case when no match is
    found. It is coerced to \code{integer}.}
  \item{incomparables}{a vector of values that cannot be matched. Any
    value other than \code{NULL} will result in a fall-back to
    \code{match} without any speed gains.}
}
\details{
  See \code{\link{match}} for the purpose and details of the
  \code{match} function. \code{fmatch} is a drop-in replacement for
  the \code{match} function with the focus on
  performance. \code{incomparables} are not supported by \code{fmatch}
  and will be passed down to \code{match}.

  The first match against a table results in a hash table to be computed
  from the table. This table is then attached as the \code{".match.hash"}
  attribute of the table so that it can be re-used on subsequent calls
  to \code{fmatch} with the same table.

  The hashing algorithm used is the same as the \code{match} function in
  R, but it is re-implemented in a slightly different way to improve its
  performance at the cost of supporting only a subset of types (integer,
  real and character). For any other types \code{fmatch} falls back to
  \code{match} (with a warning).
}
\value{
  \code{fmatch}: A vector of the same length as \code{x} - see
  \code{\link{match}} for details.

  \code{fmatch.hash}: \code{table}, possibly coerced to match the type
  of \code{x}, with the hash table attached.

  \code{\%fin\%}: A logical vector the same length as \code{x} - see
  \code{\link{\%in\%}} for details.

}
%\references{
%}
\author{
Simon Urbanek
}
\note{
\code{fmatch} modifies the \code{table} by attaching an attribute to
  it. It is expected that the values will not change unless that
  attribute is dropped. Under normal circumstances this should not have
  any effect from user's point of view, but there is a theoretical
  chance of the cache being out of sync with the table in case the table
  is modified directly (e.g. by some C code) without removing
  attributes.

  In cases where the \code{table} object cannot be modified (or such
  modification would not survive) \code{fmatch.hash} can be used to build
  the hash table and return \code{table} object including the hash
  table. In that case no lookup is done and \code{x} is only used to
  determine the type into which \code{table} needs to be coerced.
  
  Also \code{fmatch} does not convert to a common encoding so strings
  with different representation in two encodings don't match.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{match}}
}
\examples{
# some random speed comparison examples:
# first use integer matching
x = as.integer(rnorm(1e6) * 1000000)
s = 1:100
# the first call to fmatch is comparable to match
system.time(fmatch(s,x))
# but the subsequent calls take no time!
system.time(fmatch(s,x))
system.time(fmatch(-50:50,x))
system.time(fmatch(-5000:5000,x))
# here is the speed of match for comparison
system.time(base::match(s, x))
# the results should be identical
identical(base::match(s, x), fmatch(s, x))

# next, match a factor against the table
# this will require both x and the factor
# to be cast to strings
s = factor(c("1","1","2","foo","3",NA))
# because the casting will have to allocate a string
# cache in R, we run a dummy conversion to take
# that out of the equation
dummy = as.character(x)
# now we can run the speed tests
system.time(fmatch(s, x))
system.time(fmatch(s, x))
# the cache is still valid for string matches as well
system.time(fmatch(c("foo","bar","1","2"),x))
# now back to match
system.time(base::match(s, x))
identical(base::match(s, x), fmatch(s, x))

# finally, some reals to match
y = rnorm(1e6)
s = c(y[sample(length(y), 100)], 123.567, NA, NaN)
system.time(fmatch(s, y))
system.time(fmatch(s, y))
system.time(fmatch(s, y))
system.time(base::match(s, y))
identical(base::match(s, y), fmatch(s, y))

# this used to fail before 0.1-2 since nomatch was ignored
identical(base::match(4L, 1:3, nomatch=0), fmatch(4L, 1:3, nomatch=0))
}
\keyword{manip}
\keyword{logic}
