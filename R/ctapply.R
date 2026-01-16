ctapply <- function(X, INDEX, FUN, ..., MERGE=c, .SAFE=TRUE) .External(C_ctapply, parent.frame(), X, INDEX, FUN, MERGE, .SAFE, ...)
