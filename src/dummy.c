/* dummy symbols to keep superfluous CRAN checks happy
   (this package uses NAMESPACE C-level symbol registration
   but the checks don't get that) */

extern void R_registerRoutines(void);
extern void R_useDynamicSymbols(void);

void dummy(void) {
    R_registerRoutines();
    R_useDynamicSymbols();
}
