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


// Allopatry kernel
// [[Rcpp::export]]
Rcpp::IntegerVector AllopatryKernel(Rcpp::NumericMatrix dist1, double ds, Rcpp::IntegerVector Speciesj){
  if(ds > 0){
    //Selecting speciesj habitat (preparing the Distance_Matrix_internal to only the specific occupied____///////_______SUITABLE?  cells)
    int nrow = sum(Speciesj), ncol = sum(Speciesj);
    Rcpp::NumericMatrix dist1j(nrow,ncol);
    IntegerVector idx = seq(1,Speciesj.size()) -1;
    IntegerVector occ = idx[Speciesj ==1];
    Rcout << "idx = " << idx << std::endl;
    Rcout << "occ = " << occ << std::endl;
    for(int i = 0; i < occ.size(); ++i){
      for(int j = 0; j < occ.size(); ++j){
        dist1j(i,j) = dist1(occ[i],occ[j]);
      }
    }
    //Clustering
    IntegerVector clust = Tdbscan(dist1j, ds, 1);
    IntegerVector newsp = Speciesj;
    int j = 0;
    for(int i = 0; i < Speciesj.size(); ++i){
      if(Speciesj[i] == 1) {
        newsp[i] = clust[j];
        j++;
      }
    }
    return newsp;

  }
  else{
    return Speciesj;
  }
}

// Sympatry kernel
// [[Rcpp::export]]
Rcpp::IntegerVector SympatryKernel(Rcpp::IntegerVector Speciesj, double p, int NbAllo){
  //test if there is a sympatric event in each cell of the species range
  NumericVector clust = rbinom(sum(Speciesj),1, p) + 1;
  IntegerVector newsp = Speciesj;
  int j = 0;
  int k = 1 + NbAllo;
  for(int i = 0; i < Speciesj.size(); ++i){
    if(Speciesj[i] == 1){
      if(clust[j] == 1){
        newsp[i] = 1;
      }
      else{
        newsp[i] = k;
        k++;
      }
      j++;
    }
  }
  return newsp;
}

////   **************** dispersion of species.. taking one distribution
// Dispersion kernel
// [[Rcpp::export]]
Rcpp::IntegerVector DispersionKernel(Rcpp::NumericMatrix dist2, double d, IntegerVector Speciesj){
  // Build distance matrix specific to the species
  int nrow = sum(Speciesj), ncol = dist2.ncol();
  Rcpp::NumericMatrix dist2j(nrow,ncol);
  IntegerVector idx = seq(1,Speciesj.size()) -1;
  IntegerVector occ = idx[Speciesj ==1];
  for(int i = 0; i < occ.size(); ++i){
      dist2j(i,_) = dist2(occ[i],_);
  }
  // Find closest habitat at t+1
  IntegerVector Distmin(ncol);
  NumericVector Draw = rweibull(1,1,d);
  ///// looping for eevry single species
  for(int i = 0; i < ncol; ++i){
    double Min = min(dist2j(_,i));
    if(Min < Draw[0]){
      Distmin[i] = 1;
    }
    else{
      Distmin[i] = 0;
    }
  }
  return Distmin;
}


// Dispersion and Speciation for all species at time step i
// [[Rcpp::export]]
IntegerMatrix SpDiv(double ds, double d, double p, IntegerMatrix MatrixSpecies, NumericMatrix dist1, NumericMatrix dist2, int SpMax){
  int NSp = MatrixSpecies.ncol();
  /////////////////// only two parameters because you are declarng a matrix!
  IntegerMatrix MatrixSp(dist2.ncol(),SpMax);   ///////matrix with dist2 cols rows and number of species colls
  CharacterVector Spec(SpMax);
  int NbNewSp = 0;
  // Apply to all non extinct species
  for(int i = 0; i < NSp; i++){
    ///////////////THEO i dont get this
    IntegerVector Speciesj = MatrixSpecies(_,i);
    if(sum(Speciesj) != 0){
      // Speciation processes
      // Allopatry
      IntegerVector ClustAllo = AllopatryKernel(dist1, ds, Speciesj);
      int NbAllo = max(ClustAllo);
      // Sympatry
      IntegerVector ClustSym;
      if(p > 0){
        ClustSym = SympatryKernel(Speciesj, p, NbAllo);
      }
      else{
        ClustSym = ClustAllo;
      }
      int NbClust = max(ClustSym);
      //Rcout << "1 2 1 2 codecheck Dispersion ClustSym = " << sum(ClustSym) << std::endl;
      // Dispersion processes
      for(int j = 1; j <= NbClust; ++j){
        IntegerVector NewSp = clone(ClustSym);
        NewSp[NewSp != j] = 0;
        NewSp[NewSp == j] = 1;
        if(j == 1){
          // Disperse species
          MatrixSp(_,i) = DispersionKernel(dist2, d, NewSp);
          Spec(i) = "anc";
        }
        else{
          int idx = NSp + NbNewSp + j - 2;
          if(idx < SpMax){
            MatrixSp(_, idx) = DispersionKernel(dist2, d, NewSp);
            String ID = "a";
            ID += i + 1;
            ID += "_d";
            ID += j - 1;
            if(j <= NbAllo){
              ID += "_allo";
            }
            else{
              ID += "_sym";
            }
            Spec(idx) = ID;
          }
        }
      }
      NbNewSp += NbClust - 1;
    }
  }
  if(NSp + NbNewSp < SpMax){
    IntegerMatrix Out = MatrixSp(_, Range(0, NSp + NbNewSp - 1));
    CharacterVector colN = Spec[seq(0, NSp + NbNewSp - 1)];
    colnames(Out) = colN;
    return Out;
  }
  else{
    IntegerMatrix Out = MatrixSp;
    CharacterVector colN = Spec;
    colnames(Out) = colN;
    return Out;
  }
}


