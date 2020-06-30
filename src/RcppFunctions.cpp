// Copyright (c) 2020, ETH Zurich

// Author: Oskar Hagen (oskar@hagen.bio) partially addapted from Theo Gaboriau (theo.gaboriau@umontpellier.fr)

#include <Rcpp.h>
using namespace Rcpp;


// extracted from rcpp.gen.dist.clusters
// [[Rcpp::export]]
IntegerVector getEntities(NumericMatrix ma) {
  int rows = ma.rows();
  int entity = 0;
  IntegerVector entities(rows, 0);
  for (int i = 0; i < rows; i++) {
    if(entities[i]!=0){
      continue;
    }
    entity += 1;
    entities[i] = entity;
    for(int j = i+1; j < rows; j++) {
      bool cmp = true;
      for(int k = 0; k < rows; k++){
        cmp = cmp && (ma(k,i)==ma(k,j));
      }
      if(cmp){
        entities[j]= entity;
      }
    }
  }
  return(entities);
}


///////// part of tdbscan
// Find points closer than eps from P
// [[Rcpp::export]]
IntegerVector epsN(NumericVector LP, double d1){
  IntegerVector epsP = seq(1, LP.size());
  return epsP[LP <= d1];
}

///////// Why this conc if it is a function
//Concatenate IntegerVectors
// [[Rcpp::export]]
IntegerVector conc(IntegerVector x, IntegerVector y){
  IntegerVector out=no_init(x.size()+y.size());
  std::merge( x.begin(), x.end(), y.begin(), y.end(), out.begin() ) ;
  return out;
}

////////// what is this?
//Any function
// [[Rcpp::export]]
bool is_any_f(LogicalVector x){
  return is_true(any(x == false));
}

////////// output is a vector with suitable cells numbered by cluster name
// Expand cluster
// [[Rcpp::export]]
IntegerVector Tdbscan(NumericMatrix D, double d1, double minPts){
  int C = 0;
  int n = D.nrow();
  IntegerVector PC(n);
  LogicalVector PM(n);
  // Clustering procedure
  for(int i = 0; i < n; ++i){
    if(!PM[i]){
      PM[i] = true;
      NumericVector LP = D(i,_);
      IntegerVector NP = epsN(LP, d1) - 1;
      if(NP.size() < minPts){
        //Considered as noise
        PC[i] = 0;
      } else {
        //Define a new cluster
        C++;
        PC[i] = C;
        IntegerVector tovisit = NP;
        IntegerVector seeds = NP;
        //Select points that belong to that cluster
        while(is_any_f(PM[tovisit])){
          //Visiting neighbours
          int m = tovisit.size();
          for(int j = 0; j < m; ++j){
            int Pp = tovisit[j];
            if(!PM[Pp]){
              PM[Pp] = true;
              //Finding neighbours of Pp
              IntegerVector NPp = epsN(D(Pp,_), d1) - 1;
              NP = unique(conc(NP, NPp));
              if(NPp.size() >= minPts){
                seeds = unique(conc(seeds, NPp));
              }
            }
          }
          tovisit = setdiff(NP, tovisit);
        }
        //Rcout << "The cluster number " << C << " has " << NP.size() << " elements"<< std::endl;
        for(int k = 0; k < NP.size(); ++k){
          if(!(PC[NP[k]] > 0)){
            PC[NP[k]] = C;
          }
        }
      }
    }
  }
  return PC;
}

////////// output is a vector with suitable cells numbered by cluster name
// Expand cluster
// [[Rcpp::export]]
IntegerVector Tdbscan_variable(NumericMatrix D, NumericVector distances, double minPts){
  int C = 0;
  int n = D.nrow();
  IntegerVector PC(n);
  LogicalVector PM(n);
  // Clustering procedure
  for(int i = 0; i < n; ++i){
    if(!PM[i]){
      PM[i] = true;
      NumericVector LP = D(i,_);
      IntegerVector NP = epsN(LP, distances[i]) - 1;
      if(NP.size() < minPts){
        //Considered as noise
        PC[i] = 0;
      } else {
        //Define a new cluster
        C++;
        PC[i] = C;
        IntegerVector tovisit = NP;
        IntegerVector seeds = NP;
        //Select points that belong to that cluster
        while(is_any_f(PM[tovisit])){
          //Visiting neighbours
          int m = tovisit.size();
          for(int j = 0; j < m; ++j){
            int Pp = tovisit[j];
            if(!PM[Pp]){
              PM[Pp] = true;
              //Finding neighbours of Pp
              IntegerVector NPp = epsN(D(Pp,_), distances[Pp]) - 1;
              NP = unique(conc(NP, NPp));
              if(NPp.size() >= minPts){
                seeds = unique(conc(seeds, NPp));
              }
            }
          }
          tovisit = setdiff(NP, tovisit);
        }
        //Rcout << "The cluster number " << C << " has " << NP.size() << " elements"<< std::endl;
        for(int k = 0; k < NP.size(); ++k){
          if(!(PC[NP[k]] > 0)){
            PC[NP[k]] = C;
          }
        }
      }
    }
  }
  return PC;
}
