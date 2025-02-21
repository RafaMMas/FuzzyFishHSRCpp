// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// evaluateFIS_cpp
NumericMatrix evaluateFIS_cpp(DataFrame inputData, List membershipFunctions, NumericVector ruleConsequents, Nullable<List> inputLimits);
RcppExport SEXP _FuzzyFishHSRCpp_evaluateFIS_cpp(SEXP inputDataSEXP, SEXP membershipFunctionsSEXP, SEXP ruleConsequentsSEXP, SEXP inputLimitsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type inputData(inputDataSEXP);
    Rcpp::traits::input_parameter< List >::type membershipFunctions(membershipFunctionsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ruleConsequents(ruleConsequentsSEXP);
    Rcpp::traits::input_parameter< Nullable<List> >::type inputLimits(inputLimitsSEXP);
    rcpp_result_gen = Rcpp::wrap(evaluateFIS_cpp(inputData, membershipFunctions, ruleConsequents, inputLimits));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FuzzyFishHSRCpp_evaluateFIS_cpp", (DL_FUNC) &_FuzzyFishHSRCpp_evaluateFIS_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_FuzzyFishHSRCpp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
