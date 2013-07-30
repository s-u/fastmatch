/*
 *  fasthash: hash table
 *  This is very similar to fastmatch except that the payload
 *  is stored in the hash table as well and thus can be used to
 *  append values
 *
 *  Copyright (C) 2013  Simon Urbanek
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 */

#include "common.h"

/* for malloc/free since we handle our hash table memory separately from R */
#include <stdlib.h>
/* for hashing for pointers we need intptr_t */
#include <stdint.h>
/* for memcpy */
#include <string.h>

typedef struct hash {
    hash_index_t m, els;   /* hash size, added elements */
    hash_index_t max_load; /* max. load - resize when reached */
    int k, type;           /* used bits, payload type */
    void *src;             /* the data array of the hashed object */
    SEXP prot;             /* object to protect along whith this hash */
    SEXP parent;           /* hashed object */
    SEXP vals;             /* values vector if used as key/value storage */
    struct hash *next;
    hash_index_t ix[1];
} hash_t;

#define MAX_LOAD 0.85

/* create a new hash table with the given type and length.
   Implicitly calls allocVector(type, len) to create the storage
   of the newly added hash values
   NOTE: len is the *hash* size, so it should be 2 * length(unique(x))
   It will be rounded up to the next power of 2 */
static hash_t *new_hash(SEXPTYPE type, hash_index_t len) {
    hash_t *h;
    int k = 8; /* force a minimal size of 256 */
    hash_index_t m = 1 << k;    
    hash_index_t max_load;
    SEXP keys;
    while (m < len) { m *= 2; k++; }
    max_load = (hash_index_t) (((double) m) * MAX_LOAD);
    keys = allocVector(type, max_load);
    h = (hash_t*) calloc(1, sizeof(hash_t) + (sizeof(hash_index_t) * m));
    if (!h)
	Rf_error("unable to allocate %.2fMb for a hash table",
		 (double) sizeof(hash_index_t) * (double) m / (1024.0 * 1024.0));
    h->parent = keys;
    h->max_load = max_load;
    R_PreserveObject(h->parent);
    h->m = m;
    h->k = k;
    h->src = DATAPTR(h->parent);
    h->type = type;
    return h;
}

/* free the hash table (and all chained hash tables as well) */
static void free_hash(hash_t *h) {
    if (h->next) free_hash(h->next);
    if (h->prot) R_ReleaseObject(h->prot);
    R_ReleaseObject(h->parent);
    free(h);
}

/* R finalized for the hash table object */
static void hash_fin(SEXP ho) {
    hash_t *h = (hash_t*) EXTPTR_PTR(ho);
    if (h) free_hash(h);
}

/* pi-hash fn */
#define HASH(X) (3141592653U * ((unsigned int)(X)) >> (32 - h->k))

static int INCEL(hash_t *h) {
    if (h->els == h->max_load)
	Rf_error("Maximal hash load reached, resizing is currently unimplemented");
    return h->els++;
}

/* add an integer value to the hash */
static int add_hash_int(hash_t *h, int val) {
    int *src = (int*) h->src;
    hash_index_t addr = HASH(val);
#ifdef PROFILE_HASH
    int oa = addr;
#endif
    while (h->ix[addr] && src[h->ix[addr] - 1] != val) {
	addr++;
	if (addr == h->m) addr = 0;
    }
#ifdef PROFILE_HASH
    if (addr != oa) printf("%d: dist=%d (addr=%d, oa=%d)\n", val, addr - oa, addr, oa);
#endif
    if (!h->ix[addr]) {
	src[INCEL(h)] = val;
	h->ix[addr] = h->els;
    }
    return addr;
}

/* to avoid aliasing rules issues use a union */
union dint_u {
    double d;
    unsigned int u[2];
};

/* add the double value at index i (0-based!) to the hash */
static int add_hash_real(hash_t *h, double val_) {
    double *src = (double*) h->src;
    union dint_u val;
    int addr;
    /* double is a bit tricky - we nave to nomalize 0.0, NA and NaN */
    val.d = (val_ == 0.0) ? 0.0 : val_;
    if (R_IsNA(val.d)) val.d = NA_REAL;
    else if (R_IsNaN(val.d)) val.d = R_NaN;
    addr = HASH(val.u[0] + val.u[1]);
#ifdef PROFILE_HASH
    int oa = addr;
#endif
    while (h->ix[addr] && src[h->ix[addr] - 1] != val.d) {
	addr++;
	if (addr == h->m) addr = 0;
    }
#ifdef PROFILE_HASH
    if (addr != oa) printf("%g: dist=%d (addr=%d, oa=%d)\n", val.d, addr - oa, addr, oa);
#endif
    if (!h->ix[addr]) {
	src[INCEL(h)] = val.d;
	h->ix[addr] = h->els;
    }
    return addr;
}

/* add a R object to the hash */
static int add_hash_obj(hash_t *h, SEXP val) {
    int addr;
    SEXP *src = (SEXP*) h->src;
    intptr_t val_i = (intptr_t) val;
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
    addr = HASH((val_i & 0xffffffff) ^ (val_i >> 32));
#else
    addr = HASH(val_i);
#endif
#ifdef PROFILE_HASH
    int oa = addr;
#endif
    while (h->ix[addr] && src[h->ix[addr] - 1] != val) {
	addr++;
	if (addr == h->m) addr = 0;
    }
#ifdef PROFILE_HASH
    if (addr != oa) printf("%p: dist=%d (addr=%d, oa=%d)\n", val, addr - oa, addr, oa);
#endif
    if (!h->ix[addr]) {
	src[INCEL(h)] = val;
	h->ix[addr] = h->els;
    }
    return addr;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_int(hash_t *h, int val) {
    int *src = (int*) h->src;
    hash_index_t addr;
    addr = HASH(val);
    while (h->ix[addr]) {
	if (src[h->ix[addr] - 1] == val)
	    return h->ix[addr];
	addr++;
	if (addr == h->m) addr = 0;
    }
    return 0;
}

/* NOTE: we are returning a 1-based index ! */
static hash_index_t get_hash_real(hash_t *h, double val) {
    double *src = (double*) h->src;
    hash_index_t addr;
    union dint_u val_u;
    /* double is a bit tricky - we nave to normalize 0.0, NA and NaN */
    if (val == 0.0) val = 0.0;
    if (R_IsNA(val)) val = NA_REAL;
    else if (R_IsNaN(val)) val = R_NaN;
    val_u.d = val;
    addr = HASH(val_u.u[0] + val_u.u[1]);
    while (h->ix[addr]) {
	if (src[h->ix[addr] - 1] == val)
	    return h->ix[addr];
	addr++;
	if (addr == h->m) addr = 0;
    }
    return 0;
}

/* NOTE: we are returning a 1-based index ! */
static int get_hash_obj(hash_t *h, SEXP val_ptr) {
    SEXP *src = (SEXP *) h->src;
    intptr_t val = (intptr_t) val_ptr;
    hash_index_t addr;
#if (defined _LP64) || (defined __LP64__) || (defined WIN64)
    addr = HASH((val & 0xffffffff) ^ (val >> 32));
#else
    addr = HASH(val);
#endif
    while (h->ix[addr]) {
	if ((intptr_t) src[h->ix[addr] - 1] == val)
	    return h->ix[addr];
	addr++;
	if (addr == h->m) addr = 0;
    }
    return 0;
}

static SEXP asCharacter(SEXP s, SEXP env)
{
  SEXP call, r;
  PROTECT(call = lang2(install("as.character"), s));
  r = eval(call, env);
  UNPROTECT(1);
  return r;
}

/* there are really three modes:
   1) if vals in non-NULL then h->vals are populated with the
      values from vals corresponding to x as the keys
   2) if ix is non-NULL then ix is is populated with the
      indices into the hash table (1-based)
   3) if both are NULL then only the hash table is built */
static void append_hash(hash_t *h, SEXP x, int *ix, SEXP vals) {
    SEXPTYPE type = TYPEOF(x);
    R_xlen_t i, n = XLENGTH(x);
    if (type == INTSXP) {
	int *iv = INTEGER(x);
	if (vals)
	    for(i = 0; i < n; i++)
		SET_VECTOR_ELT(h->vals, h->ix[add_hash_int(h, iv[i])] - 1, VECTOR_ELT(vals, i));
	else if (ix)
	    for(i = 0; i < n; i++)
		ix[i] = h->ix[add_hash_int(h, iv[i])];
	else
	    for(i = 0; i < n; i++)
		add_hash_int(h, iv[i]);
    } else if (type == REALSXP) {
	double *dv = REAL(x);
	if (vals)
	    for(i = 0; i < n; i++)
		SET_VECTOR_ELT(h->vals, h->ix[add_hash_real(h, dv[i])] - 1, VECTOR_ELT(vals, i));
	else if (ix)
	    for(i = 0; i < n; i++)
		ix[i] = h->ix[add_hash_real(h, dv[i])];
	else
	    for(i = 0; i < n; i++)
		add_hash_real(h, dv[i]);
    } else {
	SEXP *sv = (SEXP*) DATAPTR(x);
	if (vals)
	    for(i = 0; i < n; i++)
		SET_VECTOR_ELT(h->vals, h->ix[add_hash_obj(h, sv[i])] - 1, VECTOR_ELT(vals, i));
	else if (ix)
	    for(i = 0; i < n; i++)
		ix[i] = h->ix[add_hash_obj(h, sv[i])];
	else
	    for(i = 0; i < n; i++)
		add_hash_obj(h, sv[i]);
    }
}

static hash_t *unwrap(SEXP ht) {
    hash_t *h;
    if (!inherits(ht, "fasthash"))
	Rf_error("Invalid hash object");
    h = (hash_t*) EXTPTR_PTR(ht);
    if (!h) /* FIXME: we should just rebuild the hash ... */
	Rf_error("Hash object is NULL - probably unserialized?");
    return h;
}

static SEXP chk_vals(SEXP vals, SEXP keys) {
    /* FIXME: requiring vals to be a list is not very flexible, but the
              easiest to implement. Anything else complicates the
	      append_hash() function enormously and would require
	      a separate solution for each combination of key and value types
    */
    if (vals == R_NilValue)
	vals = 0;
    else {
	if (TYPEOF(vals) != VECSXP)
	    Rf_error("`values' must be a list");
	if (XLENGTH(vals) != XLENGTH(keys))
	    Rf_error("keys and values vectors must have the same length");
    }
    return vals;
}

static void setval(SEXP res, R_xlen_t i, hash_index_t ix, SEXP vals)
{
    SET_VECTOR_ELT(res, i, (ix == 0) ? R_NilValue : VECTOR_ELT(vals, ix - 1));
}

/*---- API visible form R ----*/

SEXP mk_hash(SEXP x, SEXP sGetIndex, SEXP sValueEst, SEXP vals) {
    SEXP a, six;
    SEXPTYPE type;
    hash_t *h = 0;
    int np = 0, get_index = asInteger(sGetIndex) == 1;
    int *ix = 0;
    hash_index_t val_est = 0;

    if (TYPEOF(sValueEst) == REALSXP) {
	double ve = REAL(sValueEst)[0];
	if (ve < 0 || R_IsNaN(ve))
	    Rf_error("Invalid value count estimate, must be positive or NA");
	if (R_IsNA(ve)) ve = 0.0;
	val_est = ve;
    } else {
	int ve = asInteger(sValueEst);
	if (ve == NA_INTEGER) ve = 0;
	if (ve < 0)
	    Rf_error("Invalid value count estimate, must be positive or NA");
	val_est = ve;
    }

    vals = chk_vals(vals, x);

    /* implicitly convert factors/POSIXlt to character */
    if (OBJECT(x)) {
	if (inherits(x, "factor")) {
	    x = PROTECT(asCharacterFactor(x));
	    np++;
	} else if (inherits(x, "POSIXlt")) {
	    x = PROTECT(asCharacter(x, R_GlobalEnv)); /* FIXME: match() uses env properly - should we switch to .External ? */
	    np++;
	}
    }
    type = TYPEOF(x);

    /* we only support INT/REAL/STR */
    if (type != INTSXP && type != REALSXP && type != STRSXP && type != VECSXP)
	Rf_error("Currently supported types are integer, real, chracter vectors and lists");

    if (get_index) {
	ix = INTEGER(six = PROTECT(allocVector(INTSXP, XLENGTH(x))));
	np++;
    }

    /* FIXME: determine the proper hash size */
    if (!val_est) val_est = XLENGTH(x);
    /* check for overflow */
    if (val_est * 2 > val_est) val_est *= 2; 
    
    h = new_hash(TYPEOF(x), val_est);
    a = PROTECT(R_MakeExternalPtr(h, R_NilValue, R_NilValue));
    Rf_setAttrib(a, R_ClassSymbol, Rf_mkString("fasthash"));
    if (ix)
	Rf_setAttrib(a, install("index"), six);
    R_RegisterCFinalizer(a, hash_fin);
    np++;
    
#if HASH_VERBOSE
    Rprintf(" - creating new hash for type %d\n", type);
#endif
    append_hash(h, x, ix, vals);
    UNPROTECT(np);
    return a;
}

SEXP append(SEXP ht, SEXP x, SEXP sGetIndex, SEXP vals) {
    SEXP six;
    SEXPTYPE type;
    hash_t *h = 0;
    int np = 0;
    int *ix = 0;
    int get_index = (asInteger(sGetIndex) == 1);

    h = unwrap(ht);

    vals = chk_vals(vals, x);

    /* implicitly convert factors/POSIXlt to character */
    if (OBJECT(x)) {
	if (inherits(x, "factor")) {
	    x = PROTECT(asCharacterFactor(x));
	    np++;
	} else if (inherits(x, "POSIXlt")) {
	    x = PROTECT(asCharacter(x, R_GlobalEnv)); /* FIXME: match() uses env properly - should we switch to .External ? */
	    np++;
	}
    }
    type = TYPEOF(x);

    /* we only support INT/REAL/STR */
    if (type != INTSXP && type != REALSXP && type != STRSXP && type != VECSXP)
	Rf_error("Currently supported types are integer, real, chracter vectors and lists");

    if (get_index) { /* FIXME: long vec support? */
	ix = INTEGER(six = PROTECT(allocVector(INTSXP, LENGTH(x))));
	np++;
    }

    append_hash(h, x, ix, vals);
    if (np) UNPROTECT(np);
    return ix ? six : ht;
}

SEXP get_table(SEXP ht) {
    R_len_t n;
    R_xlen_t sz = sizeof(int);
    SEXP res;
    hash_t *h = unwrap(ht);
    n = h->els;
    res = allocVector(h->type, n);
    if (h->type == REALSXP) sz = sizeof(double);
    else if (h->type != INTSXP) sz = sizeof(SEXP);
    sz *= n;
    memcpy(DATAPTR(res), DATAPTR(h->parent), sz);
    return res;
}

SEXP get_values(SEXP ht, SEXP x) {
    SEXP res;
    SEXPTYPE type;
    hash_t *h = 0;
    int np = 0;

    h = unwrap(ht);

    if (!h->vals)
	Rf_error("This is not a key/value hash table");
    
    /* implicitly convert factors/POSIXlt to character */
    if (OBJECT(x)) {
	if (inherits(x, "factor")) {
	    x = PROTECT(asCharacterFactor(x));
	    np++;
	} else if (inherits(x, "POSIXlt")) {
	    x = PROTECT(asCharacter(x, R_GlobalEnv)); /* FIXME: match() uses env properly - should we switch to .External ? */
	    np++;
	}
    }
    type = TYPEOF(x);

    /* we only support INT/REAL/STR */
    if (type != INTSXP && type != REALSXP && type != STRSXP && type != VECSXP)
	Rf_error("Currently supported types are integer, real, chracter vectors and lists");
    
    {
	R_xlen_t i, n = XLENGTH(x);
	res = PROTECT(allocVector(VECSXP, n));
	np++;
	
	if (type == INTSXP) {
	    int *iv = INTEGER(x);
	    for (i = 0; i < n; i++)
		setval(res, i, get_hash_int(h, iv[i]), h->vals);
	} else if (type == REALSXP) {
	    double *rv = REAL(x);
	    for (i = 0; i < n; i++)
		setval(res, i, get_hash_real(h, rv[i]), h->vals);
	} else {
	    SEXP *rv = (SEXP*) DATAPTR(x);
	    for (i = 0; i < n; i++)
		setval(res, i, get_hash_obj(h, rv[i]), h->vals);
	}
    }
    UNPROTECT(np);
    return res;
}
