mk.hash <- function(x, size=256L, index=FALSE, values=NULL) .Call(mk_hash, x, index, size, values)

levels.fasthash <- function(x) .Call(get_table, x)

map.values <- function(hash, keys) .Call(get_values, hash, keys)

append.hash <- function(hash, x, index=TRUE, values=NULL) .Call(C_append, hash, x, index, values)
