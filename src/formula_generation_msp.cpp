#include <vector>
#include <climits>
#include <iostream>
#include <math.h>
#include "Rcpp.h"
using namespace Rcpp;

long int gcd(long int a,long int b)
{
	return b == 0 ? a : gcd(b, a % b);
}

long int lcm(long int a, long int b)
{
	return (a*b)/gcd(a,b);
}


void findAllSol(long int M, int i, std::vector<long int>& components, long int** res_matrix,std::vector<int> c_comp, std::vector<std::vector<int> >& compomers)
{
	if(i== 0)
	{
		c_comp[0] = M/components[0];
		compomers.push_back(c_comp);
		return;
	}
	long int vlcm = lcm(components[0],components[i]);

	long int l = vlcm/components[i];
	for(int j =0; j<l; j++)
	{
		c_comp[i] = j;
		long int m = M-j*components[i];
		if(m >= 0)
		{
			long int r = m%components[0];
			long int lbound = res_matrix[r][i-1];
			int citer = 0;
			while(m>=lbound)
			{
				citer++;
				findAllSol(m, i-1, components, res_matrix, c_comp, compomers);
				m = m - vlcm;
				c_comp[i]=c_comp[i]+l;
			}
		}
	}
}




//Extended reisudal table function.
long int** MakeExtendedResidualTable(std::vector<long int>& components)
{
	int rows = components[0];
	int k = components.size();
	long int** ex_res_table = new long int*[rows];
	for (int i = 0; i < rows; ++i)
		ex_res_table[i] = new long int[k];
	//Initializing the table :
	for(int i=0; i < k; i++)
	{
		ex_res_table[0][i] = 0;
	}

	for(int i=0; i < k; i++)
	{
		ex_res_table[0][i] = 0;
		for(int r=1; r<rows; r++)
		{
			ex_res_table[r][i] = INT_MAX;
		}
	}

	long int d = 0;
	//Updating the table
	for(int i=1; i < k; i++)
	{
		d = gcd(components[0],components[i]);
		for(int t=0; t<d; t++)
		{
			int n = 0;
			for(int q=0; q<components[0]; q++)
			{
				if(q%d==t)
				{
					n = ex_res_table[q][i-1];
					break;
				}
			}
			ex_res_table[n % components[0]][i] = n;
			if(n<INT_MAX)
			{
				long int num = components[0]/d - 1;
				for(int ct = 0; ct < num; ct++)
				{
					n = n+components[i];
					long int r = n % components[0];
					long int a = ex_res_table[r][i-1];
					n = (a>=n ? n : a);
					ex_res_table[r][i] = n;
				}
			}
		}

	}
	return ex_res_table;
}


//Creating the table double precision.
std::vector<std::vector<int> > filteringCompositionsTol(std::vector<std::vector<int> >& solutions, std::vector<double>& components, double lb, double ub)
{
	std::vector<std::vector<int> > valid_solutions;
	for(size_t i = 0; i < solutions.size(); i++)
	{
		double vsum = 0;
		for(size_t j = 0; j < components.size(); j ++)
		{
			vsum += (solutions[i][j]*components[j]);
		}
		if(vsum <= ub && vsum >= lb)
		{
			valid_solutions.push_back(solutions[i]);
		}
	}
	return valid_solutions;
}


std::vector<std::vector<int> > decomposeMassCpp(double mass,double tolerance,int b, std::vector<double>& components)
{

	//creating the upper bounds to be searched.
	double ub = mass+tolerance;
	double lb = mass-tolerance;

	std::vector<long int> ceil_components;
	double b_delta = 0;
	for(size_t i=0; i<components.size(); i++)
	{
		double d_val = components[i]*b;
		long int ceil_li_val = (long int) ceil(d_val);
		double c_delta = ((double)(ceil_li_val - d_val))/(components[i]);
		ceil_components.push_back(ceil_li_val);
		if(c_delta>b_delta)
		{
			b_delta = c_delta;
		}
	}


	//Redefining the upper bound of the interval :
	int ub_int = (int)floor(b*ub+b_delta*ub);
	int lb_int = (int)ceil(lb*b);
	std::vector<std::vector<int> > solutions;

	//The extended residual table is calculated once for all.
	long int** res_matrix = MakeExtendedResidualTable(ceil_components);

	//Applying the algorithm on each integer.
	for(int i = lb_int; i< ub_int; i++)
	{
		std::vector<int> currentSol;
		for(size_t j = 0; j < components.size(); j++)
		{
			currentSol.push_back(0);
		}
		findAllSol((long int) i,components.size()-1, ceil_components, res_matrix ,currentSol, solutions);
	}
	solutions = filteringCompositionsTol(solutions, components, lb, ub);
	return solutions;
}

//Passing from a formula ot a raw formula.
// [[Rcpp::export]]
List decomposeMass(double mass,double tolerance, int b, std::vector<double> components)
{
	std::vector<std::vector<int> > res = decomposeMassCpp(mass,tolerance,b,components);

	//Allocating a list of the right size
	List return_r(res.size());
	for(size_t i=0; i<res.size(); i++)
	{
		return_r[i] = wrap(res[i]);
	}
	return return_r;
}



//We add the conversion form raw formula.
std::string to_str(IntegerVector formula){
	std::stringstream result;
	std::copy(formula.begin(), formula.end(), std::ostream_iterator<int>(result, "."));
	std::string res = result.str();
	return res;
}



// [[Rcpp::export]]
List formulaExtension(NumericVector masses,NumericVector mzlim, IntegerMatrix formula, LogicalVector hatoms, IntegerVector hhatom ){

	//vector of found masses formula
	//At the moment we consider masses as this vector
	std::set<double> found_masses(masses.begin(),masses.end());
	std::map<std::string,int> found_formula;

	//Adding the initial set of formula.


	//test changing the value of masses to a haschcode of formula

	double lim = mzlim[0];

	//0 no heteroAtom, 1 only one and 2 both (not recommended)
	int chatom = hhatom[0];

	//We initialize the new new list of formula.
	int nelems = formula.ncol();
	int nf = formula.nrow();

	int posnew=0;
	IntegerMatrix new_formula = formula(Range(0,nf-1),Range(0,nelems-1)); // change here (before:IntegerMatrix new_formula = formula(Range(0,2*nf-1),Range(0,nelems-1));)
    //Adding the current formula to the found formula.
	for(int i=0;i<nf;i++){
        IntegerVector tkey = formula(i , _);
        std::string tk = to_str(tkey);
        found_formula.insert(std::pair<std::string,int>(tk,i));
	}

    int crow = nf;
	//index of the newly found molecules
	int nitr = 0;
	do{
		if(nitr==100){
			List bug;
			return bug;
		}
		int old_size = masses.size();
		for(int fi = posnew ; fi < masses.size(); fi ++){
			//printf("%d %f %lld %d %lld %d\n",fi, masses[fi], masses.size(), hatoms[fi], hatoms.size(), new_formula.nrow());
			double mass = masses[fi];
			for(int pos=fi;pos<masses.size();pos++){
				double cmass = mass+masses[pos];
				//printf("cmass: %f\n", cmass);
				//We check if the mass is in the correct limit.
				if((cmass>lim) |((hatoms[fi]+hatoms[pos])>chatom)){ // pb ici parfois
					//printf("boubou\n");
					continue;
				}

				//We cheack that the formula does not already exist
				IntegerVector nkey = new_formula(pos , _)+new_formula(fi , _);
				std::string skey = to_str(nkey);

				const bool is_in_map = (found_formula.find(skey) != found_formula.end());
				// Rcout << "p ";

				if(is_in_map){
					continue;
				}else{
					//We first need to add the tuple of values to the first values
					found_formula.insert(std::pair<std::string,int>(skey,crow));


					//We resize the formula matrix if needed
					if(crow>=new_formula.nrow()){
						IntegerMatrix temp_formula(2*new_formula.nrow(),nelems);
						for(int j=0;j < new_formula.nrow();j++){
							IntegerVector temp_vec = new_formula(j,_);
							IntegerMatrix::Row zz = temp_formula(j,_);
							zz = temp_vec;
						}
						new_formula = temp_formula;
					}


					IntegerMatrix::Row n_f = new_formula(crow,_);
					crow++;
					n_f = new_formula(pos , _)+new_formula(fi , _);
					masses.push_back(cmass);
					hatoms.push_back(hatoms[fi]+hatoms[pos]); // NEW (pour avoir une info sur les hétéroatomes des masses ajoutées)

				}

			}
		}
		posnew=old_size;
	} while (posnew != masses.size());
	//We return a list containg the new formula and the new masses.
	List to_return;
	to_return["masses"] = masses[Range(0,crow-1)];
	to_return["formula"] = new_formula(Range(0,crow-1),Range(0,nelems-1));
	return(to_return);
}