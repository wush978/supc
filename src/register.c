#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP supc_clusterize(SEXP, SEXP);
extern SEXP supc_supc1_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP supc_test_dgemm(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"supc_clusterize", (DL_FUNC) &supc_clusterize, 2},
    {"supc_supc1_cpp",  (DL_FUNC) &supc_supc1_cpp,  6},
    {"supc_test_dgemm", (DL_FUNC) &supc_test_dgemm, 3},
    {NULL, NULL, 0}
};

void R_init_supc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
