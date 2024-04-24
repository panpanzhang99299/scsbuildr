#include <R.h>
#include <Rinternals.h> // for SEXP
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _scsbuildr_logdmvnorm(SEXP X, SEXP mean, SEXP Sigma);

static const R_CallMethodDef R_CallDef[] = {
    {"_scsbuildr_logdmvnorm",                 (DL_FUNC) &_scsbuildr_logdmvnorm,                   3},
    {NULL, NULL, 0}
};

void R_init_scsbuildr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
