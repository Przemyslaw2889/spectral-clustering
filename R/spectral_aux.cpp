#include <Rcpp.h>
#include <cmath>
#include <algorithm>
using namespace Rcpp;


double squared_distance(NumericVector x, NumericVector y){
  int n = x.size();
  if(y.size() != n)
    throw std::runtime_error("Size of x must be equal to size of y!");
  
  double distance = 0;
  for(int i = 0; i < n; i++)
    distance += pow((x[i] - y[i]), 2);
    
  return distance;
}


// Code to find indexes of smallest elements in an array (using heap structure, because sorting is slow)
typedef std::pair<int, double> enumerated;

bool compare_value(enumerated first_pair, enumerated second_pair){
  return first_pair.second > second_pair.second;  // Note: '>' not '<'
}

NumericVector indexes_of_m_smallest(NumericVector x, int m){
  int n = x.size();
  
  std::vector<enumerated> pairs;
  for(int i = 0; i < n; i++){
    enumerated pair = std::make_pair(i, x[i]);
    pairs.push_back(pair);
  }
  
  std::make_heap(pairs.begin(), pairs.end(), compare_value);
  std::pop_heap(pairs.begin(), pairs.end(), compare_value);  
  pairs.pop_back();  // smallest value is always 0, we don't want that
  
  NumericVector indexes_of_smallest(m);
  for(int i = 0; i < m; i++){
    enumerated pair = pairs.front();
    indexes_of_smallest[i] = pair.first + 1;  // + 1 because R language
    std::pop_heap(pairs.begin(), pairs.end(), compare_value);
  }
  
  return indexes_of_smallest;
}


// [[Rcpp::export]]
NumericMatrix Mnn(NumericMatrix X, int M){
  int n = X.nrow();
  int d = X.ncol();
  
  NumericMatrix distances(n, n);
  NumericMatrix S(n, M);
  
  for(int i = 0; i < n; i++){
    for(int j = 0; j < n; j++)
      distances(i, j) = squared_distance(X(i, _), X(j, _));
    S(i, _) = indexes_of_m_smallest(distances(i, _), M);
  }
  
  return S;
}


// [[Rcpp::export]]
NumericMatrix get_adjacency_matrix(NumericMatrix S){
  int n = S.nrow();
  NumericMatrix G(n, n);

  for(int i = 0; i < n; i++)
    for(int j = i; j < n; j++)
      if(is_true(any(S(i, _) == j)) || is_true(any(S(j, _) == i))){
        G(i, j) = 1;
        G(j, i) = 1;
      }
  return G;
}

/*
X <- rbind(c(1, 2, 3), c(1, 2, 4), c(100, 100, 100), c(1, 2, 5), c(1, 2, 6), c(22, 23, 24))
Mnn(X, 2)
*/