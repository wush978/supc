#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/make_shared.hpp>
#include <Rcpp.h>

using namespace boost;
typedef adjacency_list<vecS, vecS, undirectedS, property<vertex_index_t, int>, property<edge_index_t, int>> Graph;

using namespace Rcpp;

//[[Rcpp::export(".clusterize")]]
SEXP clusterize(NumericVector dist, double tolerance) {
  int n = std::sqrt(dist.length() * 2 + 0.25) + 0.5;
  if (n * (n - 1) != dist.size() * 2) {
    n += 1;
    if (n * (n - 1) != dist.size() * 2) {
      throw std::invalid_argument("Cannot calculate proper n");
    }
  }

  Graph g(n);
  {
    int counter = 0;
    for(int i = 0;i < n;i++) {
      for(int j = i + 1;j < n;j++) {
        if (dist[counter++] < tolerance) {
#ifdef SUPC_DEBUG
          Rcout << i << " <--> " << j << "\n";
#endif
          add_edge(i, j, g);
        }
      }
    }
  }
  IntegerVector retval(num_vertices(g));
  boost::connected_components(g, &retval[0]);
  std::map<int, int> table;
  for(int i : retval) {
    table[i] += 1;
  }
  std::map<int, std::vector<int> > mapping1;
  for(const auto& element : table) {
    mapping1[element.second].push_back(element.first);
  }
  std::map<int, int> mapping2;
  int cluster_index2 = 1;
  for(auto element = mapping1.rbegin();element != mapping1.rend();element++) {
    for(const int& cluster_index : element->second) {
      mapping2[cluster_index] = cluster_index2++;
    }
  }
  for(int i = 0;i < retval.size();i++) {
    retval[i] = mapping2[retval[i]];
  }
  return wrap(retval);
}
