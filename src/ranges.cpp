#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericVector ChangeIndexTwo(NumericVector one, NumericVector two){
	// the number of rows in the pseudotable (two ordered vectors)
	const int nRows = one.size();

	// initialize to non-realistic
	int lastOne =-1;
	int lastTwo =-1;

	// vector for the results
	NumericVector res(nRows);
	
	// count the number of entries found
	int posCounter = -1;

	// for every pseudorow
	for(int i=0;i<nRows;i++){

		// if one of the entries in the row is not the same as the previous
		if(lastOne!=one(i) || lastTwo!=two(i)){
			// that means a new unique entry is found
			posCounter++;

			// save the index
			res(posCounter) = i;

			// and the basis of comparison should be changed
			lastOne = one(i);
			lastTwo= two(i);
		
		}
	}

	// omit everything else
	NumericVector endRes(posCounter+1);
	for(int i=0; i<(posCounter+1);i++){
		endRes(i) = res(i);
	}
	
	return endRes;

}

//[[Rcpp::export]]
NumericVector ChangeIndexThree(NumericVector one, NumericVector two, NumericVector three){
	// the number of rows in the pseudotable (two ordered vectors)
	const int nRows = one.size();

	// initialize to non-realistic
	int lastOne =-1;
	int lastTwo =-1;
	int lastThree =-1;

	// vector for the results
	NumericVector res(nRows);
	
	// count the number of entries found
	int posCounter = -1;

	// for every pseudorow
	for(int i=0;i<nRows;i++){

		// if one of the entries in the row is not the same as the previous
		if(lastOne!=one(i) || lastTwo!=two(i) || lastThree!=three(i)){
			// that means a new unique entry is found
			posCounter++;

			// save the index
			res(posCounter) = i;

			// and the basis of comparison should be changed
			lastOne = one(i);
			lastTwo= two(i);
			lastThree= three(i);
		
		}
	}

	// omit everything else
	NumericVector endRes(posCounter+1);
	for(int i=0; i<(posCounter+1);i++){
		endRes(i) = res(i);
	}
	
	return endRes;

}

//[[Rcpp::export]]
NumericVector RangeRarity(NumericVector locVec, NumericVector taxVec, NumericVector propVec, NumericVector occup){
	// data must be ordered by locVec
	const int nRows = locVec.size();
	// the number of localities (cells)
	const int nLoc = max(locVec)+1;
	
	// final output - initialized to 
	NumericVector endRes(nLoc);

	// for every row
	for(int i=0; i<nRows; i++){
		endRes(locVec(i)) = endRes(locVec(i))+ ((1.0/occup(taxVec(i)))*propVec(i)); 
	}

	return endRes;

}