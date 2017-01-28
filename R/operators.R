#' Fast replacment for the \%in\% operator
#' 
#' Replacement for \code{\link[base]{\%in\%}}, based upon \code{\link{fmatch}}.
#' @param x Values to be matched.
#' @param table Values to be matched against.
#' @return A logical vector with the same length as \code{x}. It is \code{TRUE} 
#' wherever an element of \code{x} is a member of \code{table}.
#' @seealso \code{\link{fmatch}} and \code{\link[base]{match}}
#' @examples
#' x = sample(1e7, 1e7, replace = TRUE)
#' s = 1:100
#' system.time(s %in% x)
#' # A bit faster
#' system.time(s %fin% x)
#' # Subsequent call is very fast!
#' system.time(s %fin% x)
#' @export
`%fin%` <- function(x, table) fmatch(x, table, nomatch = 0L) > 0L
