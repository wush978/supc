#include <Rcpp.h>
#include <cfloat>
using namespace Rcpp;

void get_sorted_index(const double* pr, const std::size_t nrow, std::vector<int>& result) {
  result.clear();
  result.resize(nrow, 0);
  for(int i = 0;i < nrow;i++) result[i] = i;
  std::sort(result.begin(), result.end(), [&pr](int i, int j) {
    return pr[i] < pr[j];
  });
}

//[[Rcpp::export(".get_sorted_index")]]
SEXP get_sorted_index(NumericVector input) {
  const double* pr = &input[0];
  const std::size_t nrow = input.size();
  std::vector<int> result;
  get_sorted_index(pr, nrow, result);
  return wrap(result);
}

void get_inverted_index_for_sorted_index(const int* sorted_index, const std::size_t nrow, std::vector<int>& result) {
  result.clear();
  result.resize(nrow);
  for(int i = 0;i < nrow;i++) result[sorted_index[i]] = i;
}

//[[Rcpp::export(".get_inverted_index_for_sorted_index")]]
SEXP get_inverted_index_for_sorted_index(IntegerVector input) {
  const int* pr = &input[0];
  const std::size_t nrow = input.size();
  std::vector<int> result;
  get_inverted_index_for_sorted_index(pr, nrow, result);
  return wrap(result);
}


/**
 * Select a dimension, sort the index according to the dimension, 
 * and use this to filter out the points whose distance is larger than threshold.
 */
//[[Rcpp::export(".clusterize")]]
SEXP clusterize(const NumericMatrix& X, double threshold, int reference_j = -1) {
  IntegerVector cluster(X.nrow());
  int last_cluster_id = 0;
  
  // The randomly selected dimension `reference_j`
  if (reference_j == -1) {
    reference_j = Rf_runif(0.0, X.ncol());
  }

  
  const double* pr = &X[0] + X.nrow() * reference_j;
  std::vector<int> sorted_index, inverted_index_for_sorted_index;
  {
    // The start of the reference column
    const std::size_t nrow = X.nrow();
    get_sorted_index(pr, nrow, sorted_index);
    get_inverted_index_for_sorted_index(sorted_index.data(), nrow, inverted_index_for_sorted_index);
  }

  // start clusterize
  std::vector<bool> visited(X.nrow(), false);
  std::vector<int> possible_candidates;
  std::vector<int> candidates;
  std::vector<bool> is_candidate(X.nrow(), false);
  
  const double *p = &X[0];
  int nrow = X.nrow(), ncol = X.ncol();
  double threshold_squared = threshold * threshold + DBL_EPSILON;
  auto is_neighbor = [&](int i, int j) {
    const double *pi = p + i;
    const double *pj = p + j;
    double tmp, distance = 0;
    for(int k = 0;k < ncol;k++) {
      tmp = (*pi) - (*pj);
      tmp = tmp * tmp;
      distance += tmp;
      if (distance > threshold_squared) return false;
      pi += nrow;
      pj += nrow;
    }
    return std::sqrt(distance) < threshold;
  };
  
  // use reference column to find possible candidates
  auto get_possible_candidates = [&](int current_index) {
    possible_candidates.clear();
    int current_location = inverted_index_for_sorted_index[current_index];
    double bound = pr[current_index] - threshold - DBL_EPSILON;
    for(int location = current_location - 1;location >= 0;location--) {
      if (pr[sorted_index[location]] < bound) break;
      possible_candidates.push_back(sorted_index[location]);
    }
    bound = pr[current_index] + threshold + DBL_EPSILON;
    for(int location = current_location + 1;location < X.nrow();location++) {
      if (pr[sorted_index[location]] > bound) break;
      possible_candidates.push_back(sorted_index[location]);
    }
    // left
  };
  // append real candidates from possible candidates to `candidates`
  auto filter_possible_candidates = [&](int current_index) {
    for(int index : possible_candidates) {
      if (visited[index]) continue;
      if (is_candidate[index]) continue;
      if (is_neighbor(current_index, index)) {
        candidates.push_back(index);
        is_candidate[index] = true;
      }
    }
  };
  
  for(int i = 0;i < X.nrow();i++) {
    if (visited[i]) continue;
    visited[i] = true;
    cluster[i] = ++last_cluster_id;
    candidates.clear();
    std::fill(is_candidate.begin(), is_candidate.end(), false);
    get_possible_candidates(i);
    filter_possible_candidates(i);
    while(!candidates.empty()) {
      int j = candidates.back();
      candidates.pop_back();
      if (visited[j]) continue;
      visited[j] = true;
      cluster[j] = cluster[i];
      get_possible_candidates(j);
      filter_possible_candidates(j);
    }
  }
  return cluster;
}
