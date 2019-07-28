#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector EndemicTable (NumericVector tax, NumericVector loc, int proportion){
	// with Rcpp sugar
	const int nTax = max(tax) +1;
	const int nLoc = max(loc) +1;

	// create a simple boolean matrix - initialized to 0?
	LogicalMatrix dist(nTax, nLoc);

	// fill it in
	const int datRows = tax.size();

	// fill the table
	for(int i=0; i<datRows; i++){
		dist(tax(i), loc(i)) = 1;
	}

	// calculate total occupancy
	NumericVector taxOccup(nTax);
	for(int i=0; i<nTax; i++){
		// with Rcpp sugar
		taxOccup(i) = sum(dist(i,_));
	}
	
	// calculate Endemism
	NumericVector Endemism(nLoc);
	for(int i=0; i<nLoc; i++){
		for(int j=0; j<nTax; j++){
			if(dist(j,i)==taxOccup(j)){
				Endemism(i)++;
			}

		}
		if(bool(proportion)){
			Endemism(i) = Endemism(i)/sum(dist(_,i)); 
		}
		
	}

	// temporary return
	return Endemism;	

}