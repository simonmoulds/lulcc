/* Author: Simon Moulds
   Date: 14/07/2014
   Version: 1.0
   Licence: GPL v3 */

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Rmath.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"

SEXP updatehist(SEXP lu0, SEXP lu1, SEXP h) { //, SEXP h, SEXP a) {

    R_len_t i;
    int *lu_t0, *lu_t1, *hist;
    int ncell = length(lu0);

    PROTECT(lu0 = coerceVector(lu0, INTSXP));
    lu_t0 = INTEGER(lu0);
 
    PROTECT(lu1 = coerceVector(lu1, INTSXP));
    lu_t1 = INTEGER(lu1);

    PROTECT(h = coerceVector(duplicate(h), INTSXP));
    hist = INTEGER(h);

    for (i = 0; i < ncell; i++) {
        if (lu_t0[i] == lu_t1[i]) {
            hist[i] += 1;
        } else {
            hist[i] = 0;
        }
    }
    
    UNPROTECT(3);
    return(h);

}
