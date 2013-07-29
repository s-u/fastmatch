mk.hash <- function(x, size=256L, index=FALSE, values=NULL) .Call(mk_hash, x, size, index, values)

hash.index <- function(hash) .Call(get_table, hash)

map.values <- function(hash, keys) .Call(get_values, hash, keys)

append.hash <- function(hash, x, index=FALSE, values=NULL) .Call(C_append, hash, x, index, values)
