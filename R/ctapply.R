ctapply <- function(X, INDEX, FUN, ..., MERGE=c) .External(C_ctapply, parent.frame(), X, INDEX, FUN, MERGE, ...)
