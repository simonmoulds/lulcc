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

SEXP allocateclues(SEXP p, SEXP lu, SEXP d, SEXP c, SEXP jf, SEXP sf, SEXP mi, SEXP md, SEXP ad) {

    R_len_t i, j;
    int *alloc, count, ix, lu1_ix;
    double *prob, *pprob, *demand, *iter, *landuse, *total, *diff, *codes, lu1, tprob, absdiff, maxp, maxd, aved;

    //SEXP dim = getAttrib(p, R_DimSymbol);
    //int ncell = INTEGER(dim)[0];   
    //int ncode = INTEGER(dim)[1];   
    int ncell = length(lu);
    int ncode = length(c);

    PROTECT(p = coerceVector(p, REALSXP)); 
    prob = REAL(p);

    PROTECT(lu = coerceVector(lu, REALSXP));
    landuse = REAL(lu);
   
    PROTECT(d = coerceVector(d, REALSXP));
    demand = REAL(d);

    PROTECT(c = coerceVector(c, REALSXP));
    codes = REAL(c);

    double jitter_f = REAL(jf)[0]; // do these objects need to be protected?
    double scale_f = REAL(sf)[0];
    double maxiter = REAL(mi)[0];
    double maxdiff = REAL(md)[0];
    double avediff = REAL(ad)[0];

    iter = (double *) malloc(ncode * sizeof(double)); 
    total = (double *) malloc(ncode * sizeof(double));
    diff = (double *) malloc(ncode * sizeof(double));
    pprob = (double *) malloc(ncode * ncell * sizeof(double));
    
    SEXP al; 
    PROTECT(al = allocVector(INTSXP, ncell));
    alloc = INTEGER(al);
    
    for (i = 0; i < ncode; i++) {
        iter[i] = 0;
    }
  
    count = 0;

    GetRNGstate();

    do {

        /* jitter prob */
        for (i = 0; i < (ncode * ncell); i++) {
	    if (! R_IsNA(prob[i])) {
                pprob[i] = prob[i] + (-jitter_f + (jitter_f - (-jitter_f)) * unif_rand());
	    } else {
	        pprob[i] = prob[i];
	    }
        }

        for (i = 0; i < ncode; i++) {
            total[i] = 0;
        }
        
        for (i = 0; i < ncell; i++) {
	    //lu0 = landuse[i];
	    lu1 = landuse[i]; //lu0; // initial assumption: land use stays the same

            if (! R_IsNA(lu1)) {
                lu1_ix = -1;
                 
                /* Get land use code index */
                for (j = 0; j < ncode; j++) {
		    if (codes[j] == lu1) {
		        lu1_ix = j;
		    }  
                }

                if (lu1_ix == -1) {
		    error("land use map contains invalid codes");
                }

		/* Find land use type with highest probability */ 
                maxp = -999;
                for (j = 0; j < ncode; j++) {
                    ix = j * ncell + i;
                    if (! R_IsNA(pprob[ix])) {
		        tprob = pprob[ix] + iter[j];
                        if (maxp < tprob) {
			    maxp = tprob;
                            lu1_ix = j; 
                        }
                    }
                }

                lu1 = codes[lu1_ix]; // do something here: lu1_ix will be recycled from previous loop
                total[lu1_ix] += 1;
	    } 

            alloc[i] = lu1; 

        }
        
        for (i = 0; i < ncode; i++) {
            diff[i] = demand[i] - total[i];
        }

        maxd = 0;
        aved = 0;
        for (i = 0; i < ncode; i++) {
            if (diff[i] < 0) {
                absdiff = diff[i] * -1;
            } else {
                absdiff = diff[i];
            }

            if (maxd < absdiff) {
                maxd = absdiff;
            }

            aved = aved + diff[i] / ncode;
        }

        if (count >= maxiter) {
	    warning("Maximum number of iterations reached: returning early");
	    break;
        }
        
        for (i = 0; i < ncode; i++) {
            iter[i] = (iter[i] + diff[i] * scale_f);
        }
	
        //landuse = alloc;
        count += 1;

    } while (maxd > maxdiff || aved > avediff);

    PutRNGstate();

    free(iter);
    free(total);
    free(diff);
    free(pprob);

    UNPROTECT(5);
    return(al);

}

/* double fmin(double *x, int n) {	/\* Not tested *\/ */

/*     int i; */
/*     double min; */

/*     min = x[0]; */
/*     for (i = 0; i < n; i++) { */
/*         if (x[i] < min) { */
/* 	    min = x[i]; */
/* 	} */
/*     } */

/*     return(min); */

/* } */

/* double fmax(double *x, int n) { /\* Not tested *\/ */

/*     int i; */
/*     double max; */

/*     max = x[0]; */
/*     for (i = 0; i < n; i++) { */
/*         if (x[i] > max) { */
/* 	    max = x[i]; */
/* 	} */
/*     } */

/*     return(max); */

/* } */
