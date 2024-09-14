#include <memory>
#if defined(_OPENMP)
#include <omp.h>
// let the number of thread become 1 on solaris
#if defined(__SUNPRO_CC)
#define SUPC_FORCE_SINGLE_THREAD num_threads(1)
#else
#define SUPC_FORCE_SINGLE_THREAD
#endif // __SUNPRO_CC
#endif // _OPENMP
#include "cblas_R.h"
#include <cfloat>
#include <Rcpp.h>

using namespace Rcpp;

struct T {

  Function RT;
  
  std::string msg;
  
  const char* unknown = "unknown error of T(t)";

  T(Function _RT) : RT(_RT), msg("Unknown error") { }

  double operator()(int t) {
    RObject r(RT(wrap<int>(t)));
    if (is<NumericVector>(r)) return as<double>(r);
    else if (is<CharacterVector>(r)) {
      CharacterVector sr(r);
      SEXP psr = wrap(sr);
      if (sr.size() == 0) {
        msg.assign(unknown);
      }
      else if (STRING_ELT(psr, 0) == NA_STRING) {
        msg.assign(unknown);
      } else {
        const char* s = CHAR(STRING_ELT(psr, 0));
        msg.assign(s);
      }
      return -1;
    } else {
      msg.assign("unknown error of T(t)");
      return -1;
    }
  }
};

/**
 * Because exception handling of Rcpp will trigger the segfault on solaris,
 * we will return a string instead and use R code to check.
 * Ref: <https://github.com/RcppCore/Rcpp/issues/1159>
 */
inline SEXP return_error_message(const T& getT) {
  return wrap(getT.msg);
}

inline bool is_equal(const R_xlen_t a, const std::size_t b) {
  if (a < 0) return false;
  std::size_t aa(a);
  return aa == b;
}

static void fill_sym_matrix(const int m, const double * pd, const double diag, std::vector<double>& retval) {
  for(int col = 0;col < m;col++) {
    retval[col * m + col] = diag;
    memcpy(retval.data() + col * m + col + 1, pd + (col * m - col * (col + 1) / 2), sizeof(double) * (m - 1 - col));
  }
}

static void dgemm(const int m, const int n, const double * a, const double * b, double * retval) {
  const CBLAS_ORDER order = CBLAS_ORDER::CblasColMajor;
  const CBLAS_TRANSPOSE trans_a = CBLAS_TRANSPOSE::CblasNoTrans, trans_b = CBLAS_TRANSPOSE::CblasNoTrans;
  if (::cblas_Rdgemm(order, trans_a, trans_b, m, n, m, 1.0, a, m, b, m, 0.0, retval, m) != 0) throw std::runtime_error("cblas_dgemm return non-zero");
}

static void print_dot() {
  Rcout << ".";
  Rcout.flush();
}

//[[Rcpp::export(".set_num_threads")]]
void set_num_threads(int x) {
#if defined(_OPENMP)
  omp_set_num_threads(x);
#endif
}

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
SEXP supc1_cpp(NumericMatrix x, double tau, Function RT, double tolerance, Function dist, bool verbose) {
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
    if (verbose) print_dot(); // 1
    pd.reset(new NumericVector(dist(*px)));
    if (!is_equal(pd->size(), d.size())) throw std::runtime_error("Inconsistent pd and d");
    double * ppd = &(pd->operator[](0)), * ppx = &(px->operator[](0)), * ppretval = &(pretval->operator[](0));
    double _T = getT(t++);
    if (_T <= 0) {
      return return_error_message(getT);
    }
    if (verbose) print_dot(); // 2
    for(std::size_t i = 0;i < d.size();i++) {
      if (ppd[i] > tau) {
        d[i] = 0.0;
      }
      else {
        d[i] = std::exp(- ppd[i] / _T);
      }
    }
    if (verbose) print_dot(); // 3
    fill_sym_matrix(m, d.data(), 1.0, f1);
    if (verbose) print_dot(); // 4
    for(int j = 0;j < m;j++) {
      colsum[j] = 0;
      for(int i = 0;i < j;i++) {
        colsum[j] += f1[i * m + j];
      }
      for(int i = j;i < m;i++) {
        colsum[j] += f1[j * m + i];
      }
    }
    if (verbose) print_dot(); // 5
    // dsymm(m, m, f1.data(), diag_matrix.data(), f2.data()); //, CBLAS_SIDE::CblasRight);
    for(int j = 0;j < m;j++) {
      for(int i = 0;i < j;i++) {
        f2[j * m + i] = f1[i * m + j] / colsum[i];
      }
      for(int i = j;i < m;i++) {
        f2[j * m + i] = f1[j * m + i] / colsum[i];
      }
    }
    if (verbose) print_dot(); // 6
    dgemm(m, px->ncol(), f2.data(), ppx, ppretval);
    if (verbose) print_dot(); // 7
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
  }
}

//[[Rcpp::export(".supc1.cpp2.internal")]]
SEXP supc1_cpp2(NumericMatrix x, double tau, Function RT, double tolerance, bool verbose) {
  int t = 0;
  int m = x.nrow(), n = x.ncol();
  std::vector<double> f1(m * m, 0.0), f2(m * m, 0.0), one_vector(m, 1.0), colsum(m, 0.0);
  // d is the distance matrix
  // d2 is the transformed matrix from d
  // acc_shift is the accumulated shift
  int d_size = m * (m - 1) / 2;
  std::vector<double> d(d_size, -1.0), d2(d_size);
  const double tau_squared = tau * tau + 100 * DBL_EPSILON;
  static std::vector<int> di, dj;
  { // construct mapping index
    di.resize(d_size);
    dj.resize(d_size);
    int q = 0;
    for(int i = 0;i < m;i++) {
      for(int j = i + 1;j < m;j++) {
        di[q] = i;
        dj[q] = j;
        q++;
      }
    }
  }

  T getT(RT);
  NumericMatrix retval1 = clone(x), retval2 = clone(x);
  bool retval_is_retval2 = true;
  NumericMatrix *px, *pretval;
  double *ppx, *ppretval, _T, difference;
  bool is_getT_error = false;
#pragma omp parallel SUPC_FORCE_SINGLE_THREAD
  {
#pragma omp master
    {
      if (verbose) {
#if defined(_OPENMP)
        Rcout << "The number of thread is: " << omp_get_num_threads() << std::endl;
#else
        Rcout << "OpenMP is disabled" << std::endl;
#endif
      }
    }
    std::vector<double> buffer(n);
    double *pb = &buffer[0], local_difference;
    while(true) {
#pragma omp master
      {
        if (retval_is_retval2) {
          px = &retval1;
          pretval = &retval2;
        } else {
          px = &retval2;
          pretval = &retval1;
        }
        ppx = px->begin();
        if (verbose) print_dot(); // 1
      } // omp master
      { // begin computing distance
#pragma omp barrier
#pragma omp for
        for(int q = 0;q < d_size;q++) {
          int i = di[q], j = dj[q];
          double *p1 = ppx + i,  *p2 = ppx + j, *pd = d.data() + q, element_distance;
          *pd = 0;
          for(int k = 0;k < n;k++) {
            element_distance = *p1 - *p2;
            *pd += element_distance * element_distance;
            if (*pd > tau_squared) {
              *pd = -1.0;
              break;
            }
            p1 += m;
            p2 += m;
          }
          if (*pd > 0) *pd = std::sqrt(*pd);
        } // omp for
      } // end computing distance
#pragma omp barrier
#pragma omp master
      { // prepare transformation
        ppx = px->begin();
        ppretval = pretval->begin();
        _T = getT(t++);
        if (_T <= 0) is_getT_error = true;
      }
      // transformation
#pragma omp barrier
      if (is_getT_error) {
        break;
      }
#pragma omp for
      for(int i = 0;i < d_size;i++) {
        if (d[i] < 0) d2[i] = 0;
        else {
          if (d[i] > tau) d2[i] = 0;
          else d2[i] = std::exp(- d[i] / _T);
        }
      }
#pragma omp master
      {
        if (verbose) print_dot(); // 2
      }
      {
#pragma omp barrier
#pragma omp for nowait 
        for(int col = 0;col < m;col++) {
          f1[col * m + col] = 1.0;
          memcpy(
            f1.data() + col * m + col + 1, 
            d2.data() + (col * m - col * (col + 1) / 2), 
            sizeof(double) * (m - 1 - col));
        }
      }
#pragma omp master
      {
        if (verbose) print_dot(); // 3
      }
#pragma omp barrier
#pragma omp for
      for(int j = 0;j < m;j++) {
        colsum[j] = 0;
        for(int i = 0;i < j;i++) {
          colsum[j] += f1[i * m + j];
        }
        for(int i = j;i < m;i++) {
          colsum[j] += f1[j * m + i];
        }
      }
#pragma omp master
      {
        if (verbose) print_dot(); // 4
      }
#pragma omp barrier
#pragma omp for
      for(int j = 0;j < m;j++) {
        for(int i = 0;i < j;i++) {
          f2[j * m + i] = f1[i * m + j] / colsum[i];
        }
        for(int i = j;i < m;i++) {
          f2[j * m + i] = f1[j * m + i] / colsum[i];
        }
      }
#pragma omp master
      {
        if (verbose) print_dot(); // 5
        dgemm(m, px->ncol(), f2.data(), ppx, ppretval);
        if (verbose) print_dot(); // 6
      // check difference between px and pretval
        difference = 0.0;
      }
      local_difference = 0;
#pragma omp barrier
#pragma omp for
      for(int i = 0;i < m;i++) {
        cblas_Rdcopy(n, ppx + i, m, pb, 1);
        cblas_Rdaxpy(n, -1, ppretval + i, m, pb, 1);
        for(int j = 0;j < n;j++) {
          local_difference = std::max(local_difference, std::abs(pb[j]));
        }
      }
#pragma omp critical
      {
        difference = std::max(difference, local_difference);
      }
#pragma omp barrier
#pragma omp master
      {
        if (verbose) Rprintf(" difference: %0.8f\n", difference);
      }
      {
        if (difference < tolerance) {
#pragma omp for
          for(int q = 0;q < d_size;q++) {
            int i = di[q], j = dj[q];
            double *p1 = ppx + i, *p2 = ppx + j, *pd = d.data() + q;
            if (*pd > 0) continue;
            cblas_Rdcopy(n, p1, m, pb, 1);
            cblas_Rdaxpy(n, -1, p2, m, pb, 1);
            *pd = cblas_Rdnrm2(n, pb, 1);
          } // omp for
        
#pragma omp master
          {
            px->attr("dist") = wrap(d);
            px->attr("iteration") = wrap(t);
          }
          break;
        }
#pragma omp master
        {
          retval_is_retval2 = !retval_is_retval2;
        }
      }
    } // while
  } // omp parallel
  if (is_getT_error) {
    return return_error_message(getT);
  }
  return *px;
}

// use this function in openmp parallel block
template<typename T, typename TV>
void parallel_fill(const int tid, const int tcount, const TV x, T& v) {
  auto chunksize = v.size() / tcount;
  auto begin = v.begin() + chunksize * tid;
  auto end = (tid + 1 == tcount) ? v.end() : begin + chunksize;
  std::fill(begin, end, x);
}

//[[Rcpp::export(".supc.random.cpp.internal")]]
SEXP supc_random_cpp(NumericMatrix x, double tau, Function RT, int k, List groups, double tolerance, bool verbose) {
  int t = 0;
  int m = x.nrow(), n = x.ncol();
  // group_idx is the vector to resample
  IntegerVector group_idx(m);
  {
    int j = 1;
    for(int i = 0;i < m;i++) {
      group_idx[i] = j;
      j++;
      if (j > k) j = 1;
    }
  }
  
  // All these variables will be re-initialized according to the group size
  std::vector<int> current_group_row_id;
  std::vector<double> f1, f2, one_vector, colsum, buf, buf2;
  // d is the distance matrix
  // d2 is the transformed matrix from d
  int d_size;
  auto get_d_size = [](const int group_size) {
    return group_size * (group_size - 1) / 2;
  };
  std::vector<double> d, d2;
  
  std::vector<int> di, dj;
  auto set_di_dj = [](const int group_size, const int d_size, std::vector<int>& di, std::vector<int>& dj) {
    di.resize(d_size);
    dj.resize(d_size);
    int q = 0;
    for(int i = 0;i < group_size;i++) {
      for(int j = i + 1;j < group_size;j++) {
        di[q] = i;
        dj[q] = j;
        q++;
      }
    }
  };

  T getT(RT);
  NumericMatrix retval1 = clone(x), retval2 = clone(x);
  bool retval_is_retval2 = true;
  NumericMatrix *px, *pretval;
  double *ppx, *ppretval, _T, difference;
  SEXP pidx;
  int* idx;
  int groups_counter = 0, group_size;
  double tau_squared = tau * tau + 100 * DBL_EPSILON;
  bool is_getT_error = false;
#pragma omp parallel SUPC_FORCE_SINGLE_THREAD
  {
    std::vector<double> buffer(n);
    double *pb = &buffer[0];
#if defined(_OPENMP)
    const int tid = omp_get_thread_num();
    const int tcount = omp_get_num_threads();
#else
    const int tid = 0;
    const int tcount = 1;
#endif
#pragma omp master
    {
      if (verbose) {
#if defined(_OPENMP)
        Rcout << "The number of thread is: " << tcount << std::endl;
#else
        Rcout << "OpenMP is disabled" << std::endl;
#endif
      }
    }

    while(true) {
#pragma omp master
      {
        if (retval_is_retval2) {
          px = &retval1;
          pretval = &retval2;
        } else {
          px = &retval2;
          pretval = &retval1;
        }
        ppx = px->begin();
        if (verbose) print_dot(); // 1
        // sampling
        if (groups_counter >= groups.size()) {
          groups.push_back(Rcpp::sample(group_idx, group_idx.size()));
        }
        pidx = VECTOR_ELT(wrap(groups), groups_counter);
        if (Rf_length(pidx) != m) throw std::runtime_error("Inconsistent group");
        idx = INTEGER(pidx);
        group_size = 0;
        ppx = px->begin();
        ppretval = pretval->begin();
        _T = getT(t++);
        if (_T <= 0) is_getT_error = true;
      } // omp master
#pragma omp barrier
      if (is_getT_error) {
        break;
      }
      for (int group_id = 1;group_id <= k;group_id++){
#pragma omp master
        {
          current_group_row_id.clear();
          group_size = 0;
          for(int i = 0;i < m;i++) {
            if (idx[i] == group_id) {
              group_size += 1;
              current_group_row_id.push_back(i);
            }
          }
          d_size = get_d_size(group_size);
          set_di_dj(group_size, d_size, di, dj);
          f1.resize(group_size * group_size);
          f2.resize(group_size * group_size);
          d.resize(d_size);
          d2.resize(d_size);
          one_vector.resize(group_size);
          colsum.resize(group_size);
          buf.resize(group_size * n);
          buf2.resize(buf.size());
        }
#pragma omp barrier
        { // initialization
          parallel_fill(tid, tcount, 0.0, f1);
          parallel_fill(tid, tcount, 0.0, f2);
          parallel_fill(tid, tcount, 0.0, d);
          parallel_fill(tid, tcount, 1.0, one_vector);
          parallel_fill(tid, tcount, 0.0, colsum);
        }
        { // begin computing distance
#pragma omp for
          for(int q = 0;q < d_size;q++) {
            int i = current_group_row_id[di[q]];
            int j = current_group_row_id[dj[q]];
            double *p1 = ppx + i, *p2 = ppx + j, *pd = d.data() + q, element_distance;
            *pd = 0;
            for(int k = 0;k < n;k++) {
              element_distance = *p1 - *p2;
              *pd += element_distance * element_distance;
              if (*pd > tau_squared) {
                *pd = -1.0;
                break;
              }
              p1 += m;
              p2 += m;
            }
            if (*pd < 0) {
              d2[q] = 0;
            } else {
              *pd = std::sqrt(*pd);
              if (d[q] > tau) d2[q] = 0;
              else d2[q] = std::exp(-d[q] / _T);
            }
          } // omp for
#pragma omp master
          {
            if (verbose) print_dot(); // 2
          }
#pragma omp for
          for(int col = 0;col < group_size;col++) {
            f1[col * group_size + col] = 1.0;
            memcpy(
              f1.data() + col * group_size + col + 1,
              d2.data() + (col * group_size - col * (col + 1) / 2),
              sizeof(double) * (group_size - 1 - col)
            );
          }
#pragma omp master
          {
            if (verbose) print_dot(); // 3
          }
#pragma omp barrier
#pragma omp for
          for(int j = 0;j < group_size;j++) {
            colsum[j] = 0;
            for(int i = 0;i < j;i++) {
              colsum[j] += f1[i * group_size + j];
            }
            for(int i = j;i < group_size;i++) {
              colsum[j] += f1[j * group_size + i];
            }
          }
#pragma omp master
          {
            if (verbose) print_dot(); // 4
          }
#pragma omp barrier
#pragma omp for
          for(int j = 0;j < group_size;j++) {
            for(int i = 0;i < j;i++) {
              f2[j * group_size + i] = f1[i * group_size + j] / colsum[i];
            }
            for(int i = j;i < group_size;i++) {
              f2[j * group_size + i] = f1[j * group_size + i] / colsum[i];
            }
          }
        } // end computing distance
#pragma omp master
        {
          if (verbose) print_dot(); // 5
        }
        { // original dgemm
#pragma omp for
          for(int q = 0;q < group_size;q++) {
            int row = current_group_row_id[q];
            cblas_Rdcopy(n, ppx + row, m, buf.data() + q, group_size);
          }
#pragma omp master
          {
            dgemm(group_size, n, f2.data(), buf.data(), buf2.data());
            if (verbose) print_dot(); // 6
          }
#pragma omp barrier
#pragma omp for
          for(int q = 0;q < group_size;q++) {
            int row = current_group_row_id[q];
            cblas_Rdcopy(n, buf2.data() + q, group_size, ppretval + row, m);
          }
        } // end dgemm
      } // for loop of group_id
      { // check difference between px and pretval
#pragma omp master
        {
          difference = 0.0;
        }
#pragma omp barrier
#pragma omp for reduction (max:difference)
        for(int i = 0;i < m;i++) {
          cblas_Rdcopy(n, ppx + i, m, pb, 1);
          cblas_Rdaxpy(n, -1, ppretval + i, m, pb, 1);
          for(int j = 0;j < n;j++) {
            difference = std::max(difference, std::abs(pb[j]));
          }
        }
#pragma omp master
        {
          if (verbose) Rprintf(" difference: %0.8f\n", difference);
        }
      } // end check difference between px and pretval
      { // check difference and tolerance
        if (difference < tolerance) {
          break;
        }
#pragma omp master
        {
          retval_is_retval2 = !retval_is_retval2;
          groups_counter++;
        }
      } // end check difference and tolerance
    } // while
  } // omp parallel
  if (is_getT_error) {
    return return_error_message(getT);
  }
  px->attr("iteration") = wrap(++groups_counter);
  List returned_groups(groups_counter);
  for(int i = 0;i < groups_counter;i++) {
    returned_groups[i] = groups[i];
  }
  px->attr("groups") = returned_groups;
  return *px;
}


//[[Rcpp::export(".test.runtime.nthread")]]
int test_runtime_nthread() {
  int nthread = 1;
#pragma omp parallel SUPC_FORCE_SINGLE_THREAD
  {
#pragma omp critical
    {
#if defined(_OPENMP)
      int tid_plus_1 = omp_get_thread_num() + 1;
      if (tid_plus_1 > nthread) nthread = tid_plus_1;
#endif
    }
  }
  return nthread;
}
