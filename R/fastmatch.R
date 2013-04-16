fmatch <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
  .Call("fmatch", x, table, nomatch, incomparables, FALSE, PACKAGE = "fastmatch")

fmatch.hash <- function(x, table, nomatch = NA_integer_, incomparables = NULL)
  .Call("fmatch", x, table, nomatch, incomparables, TRUE, PACKAGE = "fastmatch")
