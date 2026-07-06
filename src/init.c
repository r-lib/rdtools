#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP c_rd_files(SEXP);
extern SEXP c_rd_aliases(SEXP);

static const R_CallMethodDef call_entries[] = {
  {"c_rd_files",   (DL_FUNC) &c_rd_files,   1},
  {"c_rd_aliases", (DL_FUNC) &c_rd_aliases, 1},
  {NULL, NULL, 0}
};

void R_init_rdtools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
