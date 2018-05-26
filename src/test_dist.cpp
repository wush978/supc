#include "cblas_R.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(".test.dist")]]
NumericVector test_dist(NumericMatrix x) {
  int n = x.ncol(), m = x.nrow();
  double *px = x.begin();
  if (m <= 1) return R_NilValue;
  if (n == 0) return R_NilValue;
  int retval_size;
  if (m % 2 == 0) {
    retval_size = m / 2;
    retval_size = retval_size * (m - 1);
  } else {
    retval_size = (m - 1) / 2;
    retval_size = retval_size * m;
  }
  NumericVector retval(retval_size);
  double *pr = &retval[0];
  static std::vector<int> ri, rj;
  ri.resize(retval_size);
  rj.resize(retval_size);
  {
    int q = 0;
    for(int i = 0;i < m;i++) {
      for(int j = i + 1;j < m;j++) {
        ri[q] = i;
        rj[q] = j;
        q++;
      }
    }
  }
#pragma omp parallel
  {
    std::vector<double> buffer(n);
    double *pb = &buffer[0];
#pragma omp for
    for(int q = 0;q < retval_size;q++) {
      double *p1 = px + ri[q];
      double *p2 = px + rj[q];
      cblas_Rdcopy(n, p1, m, pb, 1);
      cblas_Rdaxpy(n, -1, p2, m, pb, 1);
      pr[q] = cblas_Rdnrm2(n, pb, 1);
    }
  }
  return retval;
}
