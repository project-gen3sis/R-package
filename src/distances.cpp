// Copyright (c) 2020, ETH Zurich

#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(BH)]]
#include <boost/heap/fibonacci_heap.hpp>
#include <boost/array.hpp>


// node in graph, id's shall match cell id
struct node
{
  int id;
  double dist;
  node(int i, double d)
    : id(i),
      dist(d)
  { }

  // invert operator to get a min-heap out of the boost implementation (by default a max-heap)
  bool operator<(node const & rhs) const
  {
    return dist > rhs.dist;
  }
};

// [[Rcpp::export]]
Rcpp::NumericMatrix get_distance_matrix(const IntegerVector habitable_cells,
                                        const int num_cells,
                                        const IntegerVector dist_p,
                                        const IntegerVector dist_i,
                                        const NumericVector dist_x,
                                        const double max_distance) {
  int i,j;
  int n = habitable_cells.length();
  Rcpp::NumericMatrix distance_matrix(n, n);
  int *habitable_index = new int[num_cells];
  
  // initialize to -1, so that index will match actual position 0 to num_cells - 1
  std::fill(habitable_index, habitable_index + num_cells, -1);

  // assume habitable cells are ordered
  // 0 for inhabitable cells
  // index for habitable cells
  for(i = 0; i < n; i++) {
    habitable_index[habitable_cells[i] - 1] = i;
  }

  // initialize inf distances
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      distance_matrix(i,j) = R_PosInf;
    }
  }
  // set distance matrix for every node,
  // beware "of by one" for for habitable cells to indices
  for(i = 0; i < n; i++) {
    boost::heap::fibonacci_heap<node> fib_heap;
    boost::heap::fibonacci_heap<node>::handle_type *nodes = new boost::heap::fibonacci_heap<node>::handle_type[num_cells];

    int neighbours_found = 0;

    bool *visited = new bool[num_cells];
    std::fill(visited, visited + num_cells, false);

    int cell = habitable_cells[i] - 1;

    // push initial cell with distance 0
    nodes[cell] = fib_heap.push(node(cell, 0.0));

    while(!fib_heap.empty() && fib_heap.top().dist <= max_distance && neighbours_found < n){
      node current = fib_heap.top();
      visited[current.id] = true;
      if(habitable_index[current.id] != -1){
        distance_matrix(i, habitable_index[current.id]) = current.dist;
        neighbours_found++;
      }
      fib_heap.pop();
      // loop over neighbours
      for(j = dist_p[current.id]; j < dist_p[current.id + 1]; j++){
        int neighbour = dist_i[j];
        double neighbour_dist = dist_x[j];
        double new_dist = current.dist + neighbour_dist;
        if(!nodes[neighbour].node_){
          // neighbour not yet in heap, add with new dist
          nodes[neighbour] = fib_heap.push(node(neighbour, new_dist));
        } else if ( !visited[neighbour] && new_dist < (*nodes[neighbour]).dist ) {
          // neighbour is alread in there, check if distance decrease, update if necessary
          // fib_heap.increase is called as in increase priority
          (*nodes[neighbour]).dist = new_dist;
          fib_heap.increase(nodes[neighbour]);
        }
      }
    }
    delete[] nodes;
    delete[] visited;
  }
  delete[] habitable_index;
  
  rownames(distance_matrix) = as<CharacterVector>(habitable_cells);
  colnames(distance_matrix) = as<CharacterVector>(habitable_cells);
  return(distance_matrix);
}
