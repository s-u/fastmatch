#' Fast replacment for the %in% operator
#' 
#' Replacement for \code{\link[base]{%in%}}, based upon \code{\link{fmatch}}.
#' @param x Values to be matched.
#' @param table Values to be matched against.
#' @return A logical vector with the same length as \code{x}. It is \code{TRUE} 
#' wherever an element of \code{x} is a member of \code{table}.
#' wherever an element of \code{x} is a member of \code{table}.
`%fin%` <- function(x, table) fmatch(x, table, nomatch = 0L) > 0L
