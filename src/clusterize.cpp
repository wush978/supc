#include <Rcpp.h>
using namespace Rcpp;

/**
 * Randomly select a dimension, sort the index according to the dimension, 
 * and use this to filter out the points whose distance is larger than threshold.
 */
//[[Rcpp::export(".clusterize")]]
SEXP clusterize(const NumericMatrix& X, double threshold) {
  IntegerVector cluster(X.nrow());
  int last_cluster_id = 0;
  
  // The randomly selected dimension
  int reference_j = Rf_runif(0.0, X.ncol());
  // The start of the reference column
  const double* pr = &X[0] + X.nrow() * reference_j;
  
  // This vector will be sorted according to the value of the `reference_j`-th dimension
  std::vector<int> sorted_index(X.nrow(), 0);
  for(int i = 0;i < X.nrow();i++) sorted_index[i] = i;
  std::sort(sorted_index.begin(), sorted_index.end(), [&pr](int i, int j) {
    return pr[i] < pr[j];
  });
  std::map<int, int> inverted_index_for_sorted_index;
  for(int i = 0;i < X.nrow();i++) inverted_index_for_sorted_index[sorted_index[i]] = i;
  
  // start clusterize
  std::vector<bool> visited(X.nrow(), false);
  std::vector<int> possible_candidates;
  std::vector<int> candidates;
  
  const double *p = &X[0];
  int nrow = X.nrow(), ncol = X.ncol();
  double threshold_squared = threshold * threshold;
  auto is_neighbor = [&](int i, int j) {
    const double *pi = p + i;
    const double *pj = p + j;
    double tmp, distance = 0;
    for(int k = 0;k < ncol;k++) {
      tmp = (*pi) - (*pj);
      tmp = tmp * tmp;
      distance += tmp;
      if (distance > threshold_squared) return false;
    }
    return true;
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
      if (std::find(candidates.begin(), candidates.end(), index) != candidates.end()) continue;
      if (is_neighbor(current_index, index)) candidates.push_back(index);
    }
  };
  
  for(int i = 0;i < X.nrow();i++) {
    if (visited[i]) continue;
    visited[i] = true;
    cluster[i] = ++last_cluster_id;
    candidates.clear();
    
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