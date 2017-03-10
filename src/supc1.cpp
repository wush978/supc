#include <memory>
#include <R_ext/BLAS.h>
#include <Rcpp.h>
#include "cblas_Rdgemm.h"

using namespace Rcpp;

struct T {

  Function RT;

  T(Function _RT) : RT(_RT) { }

  double operator()(int t) {
    return as<double>(RT(wrap<int>(t)));
  }
};

static void fill_sym_matrix(const int m, const double * pd, const double diag, std::vector<double>& retval) {
  for(int col = 0;col < m;col++) {
    retval[col * m + col] = diag;
    memcpy(retval.data() + col * m + col + 1, pd + (col * m - col * (col + 1) / 2), sizeof(double) * (m - 1 - col));
  }
}

// static void dsymm(const int m, const int n, const double * a, const double * b, double * retval, const CBLAS_SIDE side = CBLAS_SIDE::CblasLeft) {
//   const CBLAS_ORDER order = CBLAS_ORDER::CblasColMajor;
//   const CBLAS_UPLO uplo = CBLAS_UPLO::CblasLower;
//   int lda;
//   switch(side) {
//     case CBLAS_SIDE::CblasLeft: {
//       lda = std::max(1, m);
//       break;
//     }
//     case CBLAS_SIDE::CblasRight: {
//       lda = std::max(1, n);
//       break;
//     }
//   }
//   int ldb = std::max(1, m), ldc = std::max(1, m);
//   double alpha = 1.0, beta = 0.0;
//   ::cblas_dsymm(order, side, uplo, m, n, alpha, a, lda, b, ldb, beta, retval, ldc);
// }

static void dgemm(const int m, const int n, const double * a, const double * b, double * retval) {
  const CBLAS_ORDER order = CBLAS_ORDER::CblasColMajor;
  const CBLAS_TRANSPOSE trans_a = CBLAS_TRANSPOSE::CblasNoTrans, trans_b = CBLAS_TRANSPOSE::CblasNoTrans;
  if (::cblas_Rdgemm(order, trans_a, trans_b, m, n, m, 1.0, a, m, b, m, 0.0, retval, m) != 0) throw std::runtime_error("cblas_dgemm return non-zero");
}

// //[[Rcpp::export(".test.dsymm")]]
// void test_dsymm(NumericVector d, double diag, NumericMatrix x, NumericMatrix retval, bool side_is_left = true) {
//   //CBLAS_ORDER side = CBLAS_ORDER::CblasRowMajor;
//   int m = x.nrow(), n = x.ncol();
//   if (m != retval.nrow()) throw std::invalid_argument("Inconsistent");
//   if (n != retval.ncol()) throw std::invalid_argument("Inconsistent");
//   int dm;
//   if (side_is_left) dm = m; else dm = n;
//   std::vector<double> a(dm * dm, 0.0);
//   fill_sym_matrix(dm, &d[0], diag, a);
//   std::for_each(a.begin(), a.end(), [](double a_ele) {
//     Rcout << a_ele << ",";
//   });
//   Rcout << std::endl;
//   if (side_is_left) dsymm(m, n, a.data(), &x[0], &retval[0]); else dsymm(m, n, a.data(), &x[0], &retval[0], CBLAS_SIDE::CblasRight);
// }

//[[Rcpp::export(".test.dgemm")]]
void test_dgemm(NumericMatrix a, NumericMatrix b, NumericMatrix retval) {
  int m = a.nrow(), n = b.ncol();
  if (m != a.ncol()) throw std::invalid_argument("Inconsistent matrix dimension");
  if (m != b.nrow()) throw std::invalid_argument("Inconsistent matrix dimension");
  if (m != retval.nrow()) throw std::invalid_argument("Inconsistent matrix dimension");
  if (n != retval.ncol()) throw std::invalid_argument("Inconsistent matrix dimension");
  dgemm(m, n, &a[0], &b[0], &retval[0]);
}

//[[Rcpp::export(".supc1.cpp.internal")]]
NumericMatrix supc1_cpp(NumericMatrix x, double tau, Function RT, double tolerance, Function dist, bool verbose) {
  bool is_first = true;
  int t = 0;
  int m = x.nrow(), n = x.ncol();
  std::vector<double> f1(m * m, 0.0), f2(m * m, 0.0), d(m * (m - 1) / 2, 0.0), one_vector(m, 1.0), colsum(m, 0.0);
  T getT(RT);
  std::shared_ptr<NumericVector> pd(NULL);
  NumericMatrix retval1 = clone(x), retval2 = clone(x);
  bool retval_is_retval2 = true;
  while(true) {
    NumericMatrix * px, * pretval;
    if (retval_is_retval2) {
      px = &retval1;
      pretval = &retval2;
    } else {
      px = &retval2;
      pretval = &retval1;
    }
    if (verbose) Rcout << ".";
    pd.reset(new NumericVector(dist(*px)));
    if (pd->size() != d.size()) throw std::runtime_error("Inconsistent pd and d");
    double * ppd = &(pd->operator[](0)), * ppx = &(px->operator[](0)), * ppretval = &(pretval->operator[](0));
    double _T = getT(t++);
    if (verbose) Rcout << ".";
    for(int i = 0;i < d.size();i++) {
      if (ppd[i] > tau) {
        d[i] = 0.0;
      }
      else {
        d[i] = std::exp(- ppd[i] / _T);
      }
    }
    if (verbose) Rcout << ".";
    fill_sym_matrix(m, d.data(), 1.0, f1);
    if (verbose) Rcout << ".";
    // dsymm(m, 1, f1.data(), one_vector.data(), colsum.data());
    for(int j = 0;j < m;j++) {
      colsum[j] = 0;
      for(int i = 0;i < j;i++) {
        colsum[j] += f1[i * m + j];
      }
      for(int i = j;i < m;i++) {
        colsum[j] += f1[j * m + i];
      }
    }
    // if (verbose) Rcout << "arranging matrix ... ";
    // for(int i = 0;i < m;i++) {
    //   diag_matrix[i * m + i] = 1 / colsum[i];
    // }
    if (verbose) Rcout << ".";
    // dsymm(m, m, f1.data(), diag_matrix.data(), f2.data()); //, CBLAS_SIDE::CblasRight);
    for(int j = 0;j < m;j++) {
      for(int i = 0;i < j;i++) {
        f2[j * m + i] = f1[i * m + j] / colsum[i];
      }
      for(int i = j;i < m;i++) {
        f2[j * m + i] = f1[j * m + i] / colsum[i];
      }
    }
    if (verbose) Rcout << ".";
    dgemm(m, px->ncol(), f2.data(), ppx, ppretval);
    if (verbose) Rcout << ".";
    // check difference between px and pretval
    {
      double difference = 0.0;
      for(int i = 0;i < m * n;i++) {
        difference = std::max(difference, std::abs(ppx[i] - ppretval[i]));
      }
      if (verbose) Rprintf(" difference: %0.8f\n", difference);
      if (difference < tolerance) {
        // return px
        px->attr("dist") = *pd;
        px->attr("iteration") = wrap(t);
        return *px;
      }
    }
    retval_is_retval2 = !retval_is_retval2;
    is_first = false;
  }
}
