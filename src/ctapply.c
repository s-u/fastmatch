#include <stdlib.h>
#include <string.h>

#define USE_RINTERNALS 1
#include <Rinternals.h>

#include "rcompat.h"

#define MIN_CACHE 128

/* R 4.5.x flags SETLENGTH as illegal, but doesn't have growable API so cannot use .SAFE=FALSE */
#if R_VERSION >= R_Version(4,5,0) && R_VERSION < R_Version(4,6,0)
#define FORCE_SAFE 1
#endif

SEXP ctapply_(SEXP args) {
    SEXP rho, vec, by, fun, mfun, sSafe, cdi = 0, cdv = 0, tmp, acc, tail;
    int i = 0, n, cdlen, safe;
    
    args = CDR(args);
    rho = CAR(args); args = CDR(args);    
    vec = CAR(args); args = CDR(args);
    by  = CAR(args); args = CDR(args);
    fun = CAR(args); args = CDR(args);
    mfun= CAR(args); args = CDR(args);
    sSafe=CAR(args); args = CDR(args);
    tmp = PROTECT(allocVector(VECSXP, 3));
    acc = 0;
    safe = Rf_asInteger(sSafe);
#ifdef FORCE_SAFE
    if (!safe) {
	Rf_warning("This R version has made SETLENGTH illegal, but does not have growable vector API, use R < 4.5.0 or >= 4.6.0 to enable .SAFE=FALSE mode.");
	safe = 1;
    }
#endif
    if (TYPEOF(by) != INTSXP && TYPEOF(by) != REALSXP && TYPEOF(by) != STRSXP)
	Rf_error("INDEX must be either integer, real or character vector");
    if (TYPEOF(vec) != INTSXP && TYPEOF(vec) != REALSXP && TYPEOF(vec) != STRSXP && TYPEOF(vec) != VECSXP)
	Rf_error("X must be either integer, real, character or generic vector (list)");
    
    if ((n = LENGTH(vec)) != LENGTH(by)) Rf_error("X and INDEX must have the same length");
    while (i < n) {
	int i0 = i, N;
	SEXP eres;
	/* find the contiguous stretch */
	while (++i < n) {
	    if ((TYPEOF(by) == INTSXP && INTEGER(by)[i] != INTEGER(by)[i - 1]) ||
		(TYPEOF(by) == STRSXP && STRING_ELT(by, i) != STRING_ELT(by, i - 1)) ||
		(TYPEOF(by) == REALSXP && REAL(by)[i] != REAL(by)[i - 1]))
		break;
	}
	/* [i0, i - 1] is the interval to run on */
	N = i - i0;
	if (safe) { /* no caching, always allocate fresh vectors */
	    cdi = Rf_allocVector(TYPEOF(by), (cdlen = N));
	    cdv = Rf_allocVector(TYPEOF(vec), N);
#ifndef FORCE_SAFE
	} else {
	    /* allocate cache for both the vector and index */
	    if (!cdi) /* in theory we should duplicate also: || MAYBE_SHARED(cdv)) */ {
		/* NB: SET_VECTOR_ELT guarantees NAMED=1 so no need to tweak it */
		cdi = SET_VECTOR_ELT(tmp, 0, R_allocResizableVector(TYPEOF(by), (cdlen = ((N < MIN_CACHE) ? MIN_CACHE : N))));
		cdv = SET_VECTOR_ELT(tmp, 1, R_allocResizableVector(TYPEOF(vec), cdlen));
	    } else if (cdlen < N) {
		cdi = SET_VECTOR_ELT(tmp, 0, R_allocResizableVector(TYPEOF(by), (cdlen = N)));
		cdv = SET_VECTOR_ELT(tmp, 1, R_allocResizableVector(TYPEOF(vec), cdlen));
	    }
	    if (N != cdlen) {
		R_resizeVector(cdi, N);
		R_resizeVector(cdv, N);
	    }
#endif
	}
	/* Rprintf("%d-%d) maybe shared: index=%d, val=%d\n", (int)i0, (int)i, MAYBE_SHARED(cdi), MAYBE_SHARED(cdv)); */
	/* copy the index slice */
	if (TYPEOF(by) == INTSXP) memcpy(INTEGER(cdi), INTEGER(by) + i0, sizeof(int) * N);
	else if (TYPEOF(by) == REALSXP) memcpy(REAL(cdi), REAL(by) + i0, sizeof(double) * N);
	else if (TYPEOF(by) == STRSXP) memcpy(STRING_PTR_RW(cdi), STRING_PTR_RO(by) + i0, sizeof(SEXP) * N);
	/* copy the vector slice */
	if (TYPEOF(vec) == INTSXP) memcpy(INTEGER(cdv), INTEGER(vec) + i0, sizeof(int) * N);
	else if (TYPEOF(vec) == REALSXP) memcpy(REAL(cdv), REAL(vec) + i0, sizeof(double) * N);
	else if (TYPEOF(vec) == STRSXP) memcpy(STRING_PTR_RW(cdv), STRING_PTR_RO(vec) + i0, sizeof(SEXP) * N);
	else if (TYPEOF(vec) == VECSXP) memcpy(VECTOR_PTR_RW(cdv), VECTOR_PTR_RO(vec) + i0, sizeof(SEXP) * N);
	eres = eval(PROTECT(LCONS(fun, CONS(cdv, args))), rho);
	UNPROTECT(1); /* eval arg */
	/* if the result has NAMED > 1 then we have to duplicate it
	   see ctapply(x, y, identity). It should be uncommon, though
	   since most functions will return newly allocated objects

	   FIXME: check NAMED == 1 -- may also be bad if the reference is outside,
	   but then NAMED1 should be duplicated before modification so I think we're safe
	   NB: normally, this is always 0 since it is returned computed value
	*/
	/* Rprintf("NAMED(eres)=%d\n", NAMED(eres)); */
	if (NAMED(eres) > 1) eres = duplicate(eres);
	PROTECT(eres);
	if (!acc) tail = acc = SET_VECTOR_ELT(tmp, 2, list1(eres));
	else tail = SETCDR(tail, list1(eres));
	{
	    char cbuf[64];
	    const char *name = "";
	    if (TYPEOF(by) == STRSXP) name = CHAR(STRING_ELT(by, i0));
	    else if (TYPEOF(by) == INTSXP) {
		snprintf(cbuf, sizeof(cbuf), "%d", INTEGER(by)[i0]);
		name = cbuf;
	    } else { /* FIXME: this one is not consistent with R ... */
		snprintf(cbuf, sizeof(cbuf), "%g", REAL(by)[i0]);
		name = cbuf;
	    }
	    SET_TAG(tail, install(name));
	}
	UNPROTECT(1); /* eres */
    }
    UNPROTECT(1); /* tmp */
    if (!acc) return R_NilValue;
    acc = eval(PROTECT(LCONS(mfun, acc)), rho);
    UNPROTECT(1);
    return acc;
}
