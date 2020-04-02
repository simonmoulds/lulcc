#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP allocateclues(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP allow(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP autoconvert(SEXP, SEXP, SEXP, SEXP);
extern SEXP total(SEXP, SEXP);
extern SEXP updatehist(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"allocateclues", (DL_FUNC) &allocateclues, 9},
    {"allow",         (DL_FUNC) &allow,         5},
    {"autoconvert",   (DL_FUNC) &autoconvert,   4},
    {"total",         (DL_FUNC) &total,         2},
    {"updatehist",    (DL_FUNC) &updatehist,    3},
    {NULL, NULL, 0}
};

void R_init_lulcc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
