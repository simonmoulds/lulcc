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

SEXP allow(SEXP lu, SEXP h, SEXP c, SEXP cd, SEXP r) {

    R_len_t i, j;
    int *landuse, *hist, *rules, *allow, *tmp_allow, code, ix, lu0, lu0_ix, lu_hist;
    double *codes, *changedir;

    //SEXP dim = getAttrib(p, R_DimSymbol);
    //int ncell = INTEGER(dim)[0];   
    //int ncode = INTEGER(dim)[1];
    int ncell = length(lu);
    int ncode = length(c);
    //double NAval = REAL(valNA)[0];   

    //PROTECT(p = coerceVector(duplicate(p), REALSXP)); 
    //prob = REAL(p);

    PROTECT(lu = coerceVector(lu, INTSXP));
    landuse = INTEGER(lu);
 
    PROTECT(h = coerceVector(h, INTSXP));
    hist = INTEGER(h);

    PROTECT(c = coerceVector(c, REALSXP));
    codes = REAL(c);

    PROTECT(cd = coerceVector(cd, REALSXP));
    changedir = REAL(cd);

    PROTECT(r = coerceVector(r, INTSXP));
    rules = INTEGER(r);

    SEXP al; 
    PROTECT(al = allocVector(INTSXP, ncell * ncode));
    allow = INTEGER(al);

    tmp_allow = (int *) malloc(ncode * sizeof(int));
      
    for (i = 0; i < ncell; i++) {

        if (!ISNA(landuse[i])) {
        
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
            
            lu_hist = hist[i];
            
            for (j = 0; j < ncode; j++) {     
                ix = lu0_ix * ncode + j;
		
                /* rule 1: prohibit land use transitions */
                if (rules[ix] == 1) {
                    tmp_allow[j] = 1;

		} else if (rules[ix] == 0) {
		    tmp_allow[j] = 0;

		/* rule 2: transitions only allowed if demand increasing/decreasing */
                } else if (rules[ix] == -1) { // land use can only change from lu0 if demand for lu0 decreasing
		    if (changedir[lu0_ix] < 0) {
		        tmp_allow[j] = 1;
		    } else {
                        tmp_allow[j] = 0;
		    }
		
		} else if (rules[ix] == -2) { // land use can only change to code[j] if demand for code[j] increasing  
                    if (changedir[j] > 0) {
		        tmp_allow[j] = 1;
		    } else {
		        tmp_allow[j] = 0;
		    }

		/* rule 3: time restriction */
                } else if (rules[ix] >= 100 && rules[ix] < 1000) {     
                    if (lu_hist < (rules[ix] - 100)) {
                        tmp_allow[j] = 1;
                    } else {
                        tmp_allow[j] = 0;
                    }

                } else if (rules[ix] >= 1000) {
                    if (lu_hist < (rules[ix] - 1000)) {
                        tmp_allow[j] = 0;
                    } else {
                        tmp_allow[j] = 1;
                    }
                	  
                } else {
                    tmp_allow[j] = 1;
                }

            }

            for (j = 0; j < ncode; j++) {
                ix = j * ncell + i;
                allow[ix] = tmp_allow[j];
            }
            
        }

    }

    UNPROTECT(6);
    return(al);

}

