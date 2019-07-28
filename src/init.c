#include <R.h>                                                         
#include <Rinternals.h>                                                
#include <stdlib.h> // for NULL                                        
#include <R_ext/Rdynload.h>                                            
                                                                       
/* FIXME:                                                              
   Check these declarations against the C/Fortran source code.         
*/                                                                     
                                                                       
/* .Call calls */                                                      
extern SEXP _obigeo_EndemicTable(SEXP, SEXP, int);
extern SEXP _obigeo_ChangeIndexTwo(SEXP, SEXP);
extern SEXP _obigeo_ChangeIndexThree(SEXP, SEXP, SEXP);
extern SEXP _obigeo_RangeRarity(SEXP, SEXP, SEXP, SEXP);                                           

static const R_CallMethodDef CallEntries[] = {
    {"_obigeo_EndemicTable", (DL_FUNC) &_obigeo_EndemicTable, 3},
    {"_obigeo_ChangeIndexTwo", (DL_FUNC) &_obigeo_ChangeIndexTwo, 2},
    {"_obigeo_ChangeIndexThree", (DL_FUNC) &_obigeo_ChangeIndexThree, 3},
    {"_obigeo_RangeRarity", (DL_FUNC) &_obigeo_RangeRarity, 4},
    {NULL, NULL, 0}
};

void R_init_obigeo(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

                  
