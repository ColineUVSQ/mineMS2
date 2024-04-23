
#check existence of a molecular graph.
checkGraph <- function(fo,t_atoms=NULL,singleComponent=TRUE){
	if(is.null(t_atoms)) t_atoms <- tabAtoms()
	ref <- names(fo)
	pRef <- NULL
	if(is.null(ref)&length(fo)==nrow(t_atoms)){
		ref <- t_atoms$name
		pRef <- 1:length(ref)

	}else{
		pok <- ref %in% t_atoms$name
		if(any(!pok)){
			stop("invalid atoms furnished :",paste(ref[!pok],collapse = ",",sep=" "))
		}
		pRef <- match(ref[pok],t_atoms$name)

	}
	beta <- as.numeric(fo)
	vals <- t_atoms$valence[pRef]

	posbeta <- beta>0

	beta_vals <- beta*vals

	##conds_1
	sbv <- sum(beta_vals)
	if(sbv%%2==1) return(FALSE)

	##conds_2
	if(((sbv - 2*max(vals[posbeta]))<0)) return(FALSE)

	##conds_3
	if(singleComponent && (sbv-2*sum(beta)+2)<0) return(FALSE)
	return(TRUE)
}


###SUsed for golen rules checking.
checkSevenGoldenRules <- function(tAtoms,solution,tol=c("low","high"),rules=rep(TRUE,7)){
	tol <- match.arg(tol)
	###rules 1 naturally present in the algorithm.


	if(rules[2]){
		vval <- sum(tAtoms$valence*solution)
		n_atoms <- sum(solution)
		#Rule 2
		#.1 checking that the total valence is even.
		if(vval%%2==1)
			return(FALSE)

		#Checking the maximum valence.
		if(vval<2*max(tAtoms$valence[solution!=0])){
			return(FALSE)
		}

		#.2 checking the ratio of n atoms on valence.
		if(vval<(2*(n_atoms-1)))
			return(FALSE)
	}

	if(rules[4]){
		pH <- which(tAtoms$name=="H")
		pC <- which(tAtoms$name=="C")
		if(length(pH)==0|length(pC)==0){
			stop("H or C not found, impossible to use rule 4.")
		}
		bsup <- ifelse(tol=="low",2,3)
		bmin <- ifelse(tol=="low",0.5,0.2)
		if(bsup*solution[pH]>solution[pC]&
		   bmin*solution[pH]<bmin*solution[pC]){
			return(FALSE)
		}
	}
	return(TRUE)
}


#' findFormula function
#'
#'Fast formula generator using the algoirthm described by Bocker
#'2010. 100 times faster than rcdk.
#' @param mz The mass to be decomposed.
#' @param tol The tolerance in ppm.
#' @param atoms A of constraint on the number of atoms.
#' @param rules Which of the seven golden rules should be respected
#' @param mgraph Shall the molecular formula possible admit a molecular graph.
#' @param scomp Shall only single component molecular grpah be generated (Not H4O2 for example)
#' @return A list of the valid formula.
#' @export
#'
#' @examples
#' findFormula(34,tol=0.1,scomp=FALSE)
findFormula <-
	function(mz,
			 tol,
			 atoms = list(H = 50,
			 			 C = 25,
			 			 N = 10,
			 			 O = 20),
			 rules = rep(TRUE, 7),
			 mgraph = TRUE,
			 scomp) {
		df <- makeDfValidAtoms(atoms, mz)
		b <- 200
		allSol <- decomposeMass(mz,tol,as.integer(b),df$mass)
		to_keep <- numeric(0)
		if (length(allSol) == 0) {
			return(list())
		}
		##We filter out any solution which does not satisfy the criterion
		to_rm <- rep(FALSE, length(allSol))
		for (i in seq_along(allSol)) {
			for (j in 1:length(atoms)) {
				if (allSol[[i]][j] > atoms[[j]]) {
					to_rm[i] <- TRUE
					break
				}
			}
		}
		to_keep <- which(!to_rm)
		if (length(to_keep) == 0)
			return(list())
		allSol <- allSol[to_keep]
		to_keep <-
			sapply(
				allSol,
				checkSevenGoldenRules,
				tAtoms = df,
				tol = 'high',
				rules = rules
			)
		if (length(to_keep) > 0) {
			allSol <- allSol[to_keep]
		}
		if (length(allSol) == 0) {
			return(list())
		}
		resList <- vector(mode = "list", length = length(allSol))
		nsol <- 1
		for (i in 1:length(allSol)) {
			###We check that the formula should be kept.
			rb <- checkGraph(allSol[[i]],
							 t_atoms = df,
							 singleComponent = scomp)
			if (rb | !(mgraph)) {
				resList[[nsol]] <- allSol[[i]]
				names(resList[[nsol]]) <- df[, "name"]
				nsol <- nsol + 1
			}
		}
		return(resList[1:max(nsol-1,1)])
	}





formulaToString <- function(vformula,vnames = NULL){
	if(is.null(vnames))	vnames <- names(vformula)
	if(is.factor(vnames)) vnames <- as.character(vnames)
	vnums <- ifelse(vformula>1,vformula,"")
	vnames <- ifelse(as.character(vformula)=="0","",vnames)
	paste(paste(vnames,vnums,sep=""),collapse="")
}

stringToFormula <- function (fstring, vnames = character(0),rm_white=TRUE)
{
	fstring <- trimws(fstring)
	res <- formulaFromString(as.character(fstring),as.character(vnames))
	vres <- res[[2]]
	names(vres) <- res[[1]]
	vres
}

lossesFormulaGeneration <- function(mzrange,atoms=list("C"=15,"H"=50,"O"=20,"N"=6,"P"=2,"S"=1,"Cl"=1),catoms = 1,maxh=1){
	if(!(catoms %in% 0:2)) stop("catoms should be between 0 and 2.")
	
	tatoms <- makeDfValidAtoms(atoms)

	sig <- diff(mzrange)/2
	mu <- sig+mzrange[1]


	###Finding the formula (from the mass, give the possible formula)
	l_formula <- findFormula(mu,sig,atoms=atoms,mgraph=TRUE,scomp=TRUE)

	if(length(l_formula)==0) return(list())
	###Finding all the heteroatoms
	heteroAtoms <- as.character(tabAtoms()$name[5:11])

	###Now we get the position on the first found formula
	idxHAtoms <- na.omit(match(heteroAtoms,names(l_formula[[1]])))

	to_rm <- which(sapply(l_formula,function(x){sum(x[idxHAtoms]>0)>maxh}))
	if(length(to_rm)!=0){
		l_formula <- l_formula[-to_rm]
	}

	have_heteroatoms <- sapply(l_formula,function(x){any(x[idxHAtoms]>0)})

	mat_f <- do.call(rbind,l_formula)

	masses <- mat_f %*% tatoms$mass

	res <- formulaExtension(masses,mzrange[2],mat_f, have_heteroatoms,catoms )
	oh <- order(res$masses,decreasing=FALSE)
	res$masses <- res$masses[oh]
	res$formula <- res$formula[oh,]
	colnames(res$formula)<-tatoms$name

	return(res)
}

fragmentsFormulaGeneration <- function(mzrange,atoms=list("C"=15,"H"=50,"O"=20,"N"=6,"P"=2,"S"=1,"Cl"=1),catoms = 1,maxh=1){
	###Res a list with formula as columns and atoms names and formula as labels.

	if(!(catoms %in% 0:2)) stop("catoms should be between 0 and 2.")

	tatoms <- makeDfValidAtoms(atoms)

	sig <- diff(mzrange)/2
	mu <- sig+mzrange[1]


	###Finding the formula.
	l_formula <- findFormula(mu,sig,atoms=atoms,mgraph=TRUE,scomp=TRUE)

	if(length(l_formula)==0) return(list())
	###Finding all the heteroatoms
	heteroAtoms <- as.character(tabAtoms()$name[5:11])

	###Now we get the position on the first found formula
	idxHAtoms <- na.omit(match(heteroAtoms,names(l_formula[[1]])))

	to_rm <- which(sapply(l_formula,function(x){sum(x[idxHAtoms]>0)>maxh}))
	if(length(to_rm)!=0){
		l_formula <- l_formula[-to_rm]
	}

	mat_f <- do.call(rbind,l_formula)
	masses <- mat_f %*% tatoms$mass

	oh <- order(masses,decreasing=FALSE)
	res <- list()
	res$masses <- masses[oh]
	res$formula <- mat_f[oh,]
	colnames(res$formula)<-tatoms$name
	return(res)
}
