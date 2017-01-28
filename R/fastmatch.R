fmatch <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
  .Call(C_fmatch, x, table, nomatch, incomparables, FALSE)

fmatch.hash <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
  .Call(C_fmatch, x, table, nomatch, incomparables, TRUE)

`%fin%` <- function (x, table)
  .Call(C_fmatch, x, table, 0L, NULL, FALSE) > 0L
