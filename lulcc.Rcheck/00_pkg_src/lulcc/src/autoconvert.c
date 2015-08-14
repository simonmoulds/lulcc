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

SEXP autoconvert(SEXP lu, SEXP m, SEXP p, SEXP c) {

    R_len_t i, j;
    int *landuse, *mask, *alloc, code, ix, ixx, lu0, lu0_ix;
    double *prob, *codes, maxp;

    //SEXP dim = getAttrib(p, R_DimSymbol);
    //int ncell = INTEGER(dim)[0];   
    //int ncode = INTEGER(dim)[1];   
    int ncell = length(lu);
    int ncode = length(c);
    //double NAval = REAL(valNA)[0];

    PROTECT(lu = coerceVector(lu, INTSXP));
    landuse = INTEGER(lu);

    PROTECT(m = coerceVector(m, INTSXP));
    mask = INTEGER(m);

    PROTECT(p = coerceVector(p, REALSXP)); 
    prob = REAL(p);

    PROTECT(c = coerceVector(c, REALSXP));
    codes = REAL(c);
   
    SEXP al; 
    PROTECT(al = allocVector(INTSXP, ncell));
    alloc = INTEGER(al);
    
    /* Allocate automatic change */
    for (i = 0; i < ncell; i++) {
        lu0 = landuse[i];
        lu0_ix = -1;
        
        /* get index of land use in codes */
        for (j = 0; j < ncode; j++) {
            code = codes[j];
            if (code == lu0) {
                lu0_ix = j;
            }
        }

        /* if land use index doesn't change, break */
        if (lu0_ix == -1) {
            break;
        }

        ix = lu0_ix * ncell + i; 

        //if (prob[ix] == NAval) { 
        if (ISNA(prob[ix])) {
            maxp = -1; 

            for (j = 0; j < ncode; j++) { 
                ixx = j * ncell + i;
                code = codes[j];
         
                if (prob[ixx] > maxp) {
                    maxp = prob[ixx];
                    alloc[i] = code; /* convert cell to lu with the maximum probability NB this doesn't allow for the possibility that prob[ixx] == maxp which may be likely in some situations */
                } 
            } 

            if (maxp == -1) {
                alloc[i] = lu0; /* stay the same if no other change is permitted */
            }

        } else {

            if (mask[i] == 0) { /* restricted area */
                alloc[i] = lu0;
 
            } else {
	        //alloc[i] = NAval;
	        alloc[i] = NA_REAL;
            }

        }

    }

    UNPROTECT(5);
    return(al);

}
