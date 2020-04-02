/*  Simon Moulds, January 2013 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP total(SEXP v, SEXP c) {

    R_len_t i, j;

    int *total;
    double *lu_vals, *lu_codes, lu, code;

    PROTECT(v = coerceVector(v, REALSXP)); 
    lu_vals = REAL(v);
 
    PROTECT(c = coerceVector(c, REALSXP));
    lu_codes = REAL(c);

    int ncell = length(v);
    int ncode = length(c);

    SEXP tot;
    PROTECT(tot = allocVector(INTSXP, ncode));
    total = INTEGER(tot);

    for (i = 0; i < ncode; i++) {
        total[i] = 0;
    }

    for (i = 0; i < ncode; i++) {
        code = lu_codes[i];
        for (j = 0; j < ncell; j++) {
            lu = lu_vals[j];
            if (!R_IsNA(lu) && lu == code) {
                total[i] += 1;
            }
        }
    }
   
    UNPROTECT(3);
    return(tot);

}

