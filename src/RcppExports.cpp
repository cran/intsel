// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// proximalGraph
void proximalGraph(Rcpp::NumericVector& U, int& p, std::string& regul, Rcpp::IntegerMatrix& grp, Rcpp::IntegerMatrix& grpV, Rcpp::NumericVector& etaG, double lam);
RcppExport SEXP _intsel_proximalGraph(SEXP USEXP, SEXP pSEXP, SEXP regulSEXP, SEXP grpSEXP, SEXP grpVSEXP, SEXP etaGSEXP, SEXP lamSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type U(USEXP);
    Rcpp::traits::input_parameter< int& >::type p(pSEXP);
    Rcpp::traits::input_parameter< std::string& >::type regul(regulSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix& >::type grp(grpSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix& >::type grpV(grpVSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type etaG(etaGSEXP);
    Rcpp::traits::input_parameter< double >::type lam(lamSEXP);
    proximalGraph(U, p, regul, grp, grpV, etaG, lam);
    return R_NilValue;
END_RCPP
}
// l_ld
Rcpp::NumericVector l_ld(Rcpp::NumericVector& beta, Rcpp::NumericMatrix& x, Rcpp::NumericVector& y, Rcpp::NumericVector& w);
RcppExport SEXP _intsel_l_ld(SEXP betaSEXP, SEXP xSEXP, SEXP ySEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(l_ld(beta, x, y, w));
    return rcpp_result_gen;
END_RCPP
}
// intsel_cpp
Rcpp::List intsel_cpp(Rcpp::NumericMatrix& x, Rcpp::NumericVector& y, Rcpp::NumericVector& w, std::string& regul, Rcpp::NumericVector& lam, Rcpp::IntegerMatrix& grp, Rcpp::IntegerMatrix& grpV, Rcpp::IntegerVector& own_var, Rcpp::IntegerVector& N_own_var, Rcpp::NumericVector& etaG, Rcpp::NumericVector& init, double& init_stepsize, double& ls_shrink, double& partol, int& maxit, bool& verbose);
RcppExport SEXP _intsel_intsel_cpp(SEXP xSEXP, SEXP ySEXP, SEXP wSEXP, SEXP regulSEXP, SEXP lamSEXP, SEXP grpSEXP, SEXP grpVSEXP, SEXP own_varSEXP, SEXP N_own_varSEXP, SEXP etaGSEXP, SEXP initSEXP, SEXP init_stepsizeSEXP, SEXP ls_shrinkSEXP, SEXP partolSEXP, SEXP maxitSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type w(wSEXP);
    Rcpp::traits::input_parameter< std::string& >::type regul(regulSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix& >::type grp(grpSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix& >::type grpV(grpVSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type own_var(own_varSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type N_own_var(N_own_varSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type etaG(etaGSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type init(initSEXP);
    Rcpp::traits::input_parameter< double& >::type init_stepsize(init_stepsizeSEXP);
    Rcpp::traits::input_parameter< double& >::type ls_shrink(ls_shrinkSEXP);
    Rcpp::traits::input_parameter< double& >::type partol(partolSEXP);
    Rcpp::traits::input_parameter< int& >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< bool& >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(intsel_cpp(x, y, w, regul, lam, grp, grpV, own_var, N_own_var, etaG, init, init_stepsize, ls_shrink, partol, maxit, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_intsel_proximalGraph", (DL_FUNC) &_intsel_proximalGraph, 7},
    {"_intsel_l_ld", (DL_FUNC) &_intsel_l_ld, 4},
    {"_intsel_intsel_cpp", (DL_FUNC) &_intsel_intsel_cpp, 16},
    {NULL, NULL, 0}
};

RcppExport void R_init_intsel(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
