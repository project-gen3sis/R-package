// Copyright (c) 2020, ETH Zurich

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(BH)]]
#include <boost/numeric/odeint.hpp>
#include <boost/array.hpp>

using namespace boost::numeric::odeint;

unsigned int n;
std::vector< std::vector<double> > out_x;
std::vector<double> out_t;

// general formulation
Rcpp::NumericMatrix g_a;
Rcpp::NumericVector g_r;

//vectorized formulation
std::vector<double> g;
std::vector<double> c; // intra species
std::vector<double> l; // inter species


//vectorized model
void vLV(const std::vector<double> &x,
         std::vector<double> &dxdt,
         double t) {
  double sum = 0;
  for(unsigned int i = 0; i < x.size(); ++i){
    sum += x[i];
  }
  for(unsigned int i = 0; i < x.size(); ++i){
    dxdt[i] = x[i] * (g[i] - c[i]*x[i] - l[i]*sum);
  }
}

// general matrix model
void LV (const std::vector<double> &x,
         std::vector<double> &dxdt,
         double t) {
  for (int row = 0; row < g_a.nrow(); ++row) {
    double c = 0;
    for (int col = 0; col < g_a.ncol(); ++col) {
      // Rcpp::numericMatrix is indexed like this
      // matrixName(row, column)
      c =  c + g_a(row, col) * x[col];
    }
    dxdt[row] = x[row] * (g_r[row] - c);
  }
}

// the observer
void write_LV (const std::vector<double> &x , const double t) {
  // debugging output
  // for (unsigned int i = 0; i < x.size(); ++i) {
  //   std::cout << x[i] << " ";
  // }
  // std::cout << std::endl;

  out_t.push_back(t);

  for (unsigned int i = 0; i < x.size(); ++i) {
    out_x[i].push_back(x[i]);
  }
}

// creates the output object
Rcpp::List createOutput() {

  Rcpp::List out;
  out("Time") = Rcpp::wrap(out_t);
  for (unsigned int i = 0; i != n; ++i)
  {
    auto cnam = std::string("X") + std::to_string(i + 1);
    out(cnam) = Rcpp::wrap(out_x[i]);
  }
  out.attr("class") = "data.frame";
  int rows_out = out_t.size();
  auto rn = Rcpp::IntegerVector::create(NA_INTEGER, -rows_out);
  out.attr("row.names") = rn;
  return out;
}


// function that will be called from R
// [[Rcpp::export]]
Rcpp::List integrateModelVectorized(Rcpp::NumericVector init,
                                    Rcpp::NumericVector g_in,
                                    Rcpp::NumericVector c_in,
                                    Rcpp::NumericVector l_in,
                                    double t0,
                                    double t1,
                                    double dt) {
  //global variables for the solver
  n = g_in.size();
  g = Rcpp::as< std::vector<double> >(g_in);
  c = Rcpp::as< std::vector<double> >(c_in);
  l = Rcpp::as< std::vector<double> >(l_in);

  //initialize out_x
  out_x = std::vector< std::vector<double> >(n);
  for (unsigned int i = 0; i < n; ++i) {
    out_x[i] = std::vector<double>();
  }
  // initialize/reset out_t
  out_t = std::vector<double>();

  //format initial values
  std::vector<double> v_init = Rcpp::as< std::vector<double> >(init);

  //call the solver
  integrate(vLV, v_init, t0, t1, dt, write_LV);

  //create and return R data.frame
  return createOutput();

}

// function that will be called from R
// [[Rcpp::export]]
Rcpp::List integrateModel(Rcpp::NumericVector init,
                          Rcpp::NumericVector r,
                          Rcpp::NumericMatrix a,
                          double t0,
                          double t1,
                          double dt) {
  n = r.size();

  // initialize out_x
  out_x = std::vector< std::vector<double> >(n);
  for (unsigned int i = 0; i < n; ++i) {
    out_x[i] = std::vector<double>();
  }

  // initialize/reset out_t
  out_t = std::vector<double>();

  // assign global variables, needed for function LV
  g_a = a;
  g_r = r;

  // init must be casted to std::vector for integrate function
  std::vector<double> v_init = Rcpp::as< std::vector<double> >(init);

  // for debugging: is the order of row/col iteration right?
  // for (int i = 0; i < n; ++i) {
  //   std::cout << r[i] << " ";
  // }
  // std::cout << std::endl;
  //
  // for (int row = 0; row < a.nrow(); ++row) {
  //   for (int col = 0; col < a.ncol(); ++col) {
  //     std::cout << a(row, col) << " ";
  //   }
  // }
  // std::cout << std::endl;


  // this is the integration step
  // the function LV does the work
  // the observer function write_LV catches the state of the
  // stepper and pushes it to the global output variables
  integrate(LV, v_init, t0, t1, dt, write_LV);

  return createOutput();
}
