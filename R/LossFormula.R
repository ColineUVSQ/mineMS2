#' @include references.R
#' @include allGenerics.R
#' @include allClasses.R

REF_MINEMS2_ATOMS_NAMES <- tabAtoms()$name
REF_MINEMS2_HALOGENS <- tabAtoms()$name[tabAtoms()$halogen]


getAtomMass <- function(atoms){
  temp <- tabAtoms()
  temp$mass[temp$name %in% atoms]
}

calc_mass_raw <- function(row,atoms_m){
  sum(row*atoms_m)
}

calcMass<- function(lf,atoms_mass=NULL){
  if(is.null(atoms_mass)){
    atoms_mass <-getAtomMass(colnames(lf@formula))
  }
  apply(lf@formula,1,calc_mass_raw,atoms_m=atoms_mass)
}

LossFormulaFromSingleString <- function(formula,ref,sep="|"){
  allf <- str_split(formula,fixed(sep),simplify=TRUE)[1,]
  LossFormula(allf,ref=ref)
}

getAtomsFromRef <- function(ref){
  if(typeof(ref)=="list"){
    return(names(ref))
  }
  if(typeof(ref) %in% c("numeric","integer")){
    return(names(ref))
  }
  if(typeof(ref) == c("character")){
    return(ref)
  }
}


LossFormula <- function(formula=NULL,ref=NULL){
  ####We check the class of the arguments
  lf <- new("LossFormula")
  
  if(!is.null(ref)) ref<- getAtomsFromRef(ref)
  
  if(!is.null(formula))
  {
    na_tot <- (nchar(formula) == 0)
    na_c <- TRUE
    for(i in na_tot)
    {
      if (is.na(i) || i == FALSE)
      {
        na_c <- FALSE
      }
    }
  }
  
  if(is.null(formula)||((length(formula)==1) && (is.na(formula)))||(na_c)){

    if(is.null(ref)){
      lf@formula <- matrix(NA_integer_,nrow=0,ncol=0)
    }else{
      lf@formula <- matrix(NA_integer_,nrow=0,ncol=length(ref))
      colnames(lf@formula) <- ref
    }
  }else if(is.character(formula)){
    if(is.null(ref)){
      if(length(formula)==1){
        lf@formula <- t(sapply(formula,stringToFormula))
      }else{
        temp <- sapply(formula,stringToFormula,simplify=FALSE)
        common_labs <- unique(do.call(c,sapply(temp,names,simplify=FALSE)))
        lf@formula <- t(sapply(formula,stringToFormula,vnames=common_labs))
        
      }
    }else{
      lf@formula <- t(sapply(formula,stringToFormula,vnames=ref))
    }
  }else if(is.matrix(formula)){
    
    lf@formula <- formula
    if(!is.null(ref)){
      colnames(lf@formula) <- ref
    }
  }
  ###We always reorder the formula.
  vf <- match(colnames(lf@formula),REF_MINEMS2_ATOMS_NAMES)
  vf <- order(vf,decreasing = FALSE)
  lf@formula <- lf@formula[,vf,drop=FALSE]
  mode(lf@formula) <- "integer"
  row.names(lf@formula) <- NULL
  return(lf)
}

isKnown <- function(lf){
  nrow(lf@formula)==1
}

#' Return a string prerpesentation of a lossFormula object
#'
#' Return a string presentation of all the possible formula. Should not be used by the user.
#'
#' @param x A lossFormula object.
#'
#' @return A character vector with all the possible formula for a loss object.
setMethod("as.character","LossFormula",function(x){
  return(apply(x@formula,1,formulaToString))
})

###Return a Loss formula
combineLossFormula <- function(f1,f2,maxf=3){
            ####We combine all the elements 1 by one.
            if(ncol(f1@formula)!=ncol(f2@formula)) stop("Non compatible formula.")
            if((nrow(f1@formula)==0)|(nrow(f2@formula)==0)) return(LossFormula(ref=ncol(f1@formula)))
  
            maxf1 <- min(maxf,nrow(f1@formula))
            maxf2 <- min(maxf,nrow(f2@formula))
            ###product matrix
            tm <- t(apply(f1@formula[1:maxf1,,drop=FALSE],1,function(x,y){
              t(apply(y,1,function(w,z){
                w+z
              },z=x))
            },y=f2@formula[1:maxf2,,drop=FALSE]))
            # tm <- do.call(rbind,tm)
            tempm <- matrix(c(tm),byrow=FALSE,ncol=ncol(f1@formula))
            LossFormula(tempm,ref=colnames(f1@formula))
}


vecRDBE <- function(lfm){
  vmult <- rep(0,ncol(lfm))
  pC <- which(colnames(lfm)=="C")
  if(length(pC)!=0){
    vmult[pC] <- 1.0
  }
  pH <-  which(colnames(lfm)=="H")
  if(length(pH)!=0){
    vmult[pH] <- -0.5
  }
  pN <-  which(colnames(lfm)=="N")
  if(length(pN)!=0){
    vmult[pN] <- -0.5
  }
  PHalo <- which(colnames(lfm) %in% REF_MINEMS2_HALOGENS)
  if(length(PHalo)!=0){
    vmult[PHalo] <- -0.5
  }

  return(vmult)
}
calcRDBE <- function(lf,vmult=NULL){
  ###We match the input into each cathegory
  vmult <- vecRDBE(lf@formula)
  calcRDBE_raw(lf@formula,vmult)
}

calcRDBE_raw<- function(lfm,vmult){
  return((lfm %*% vmult)+1)
}


convertFormula <- function(x,y){
  vm <- match(names(x),colnames(y))
  val <- rep(0,ncol(y))
  val[vm] <- x
  as.integer(val)
}

orderByRDBE <- function(lf){
  rdbe <- calcRDBE(lf)
  lf@formula[order(rdbe,decreasing = TRUE)]
  lf
}


setMethod("%in%",signature=list(x="character",table="LossFormula"),function(x,table){
  vf <- stringToFormula(x,vnames=colnames(table@formula))
  any(apply(table@formula, 1, function(x, want) isTRUE(all.equal(x, want)), want = vf))
})

setMethod("%in%",signature=list(x="integer",table="LossFormula"),function(x,table){
  names(x) <- colnames(table@formula)
  any(apply(table@formula, 1, function(y, want) isTRUE(all.equal(y, want)),x))
})




####Methods to add a formula to LossFormula.
setMethod("addFormula",signature = list(x = "LossFormula",lf = "ANY"),function(x,lf){
  l_atoms <- colnames(x@formula)
  
  if(typeof(lf)=="character"){
    lf <- stringToFormula(lf,vnames=l_atoms)
  }else if(typeof(lf)=="numeric"){
    lf <- convertFormula(lf,x@formula)
  }else if(typeof(lf)=="integer"){
    lf <- convertFormula(lf,x@formula)
  }
  x@formula <- cbind(x@formula,lf)
  x
})


setMethod("addFormula",signature = list(x = "LossFormula",lf = "LossFormula"),function(x,lf){
  if(any(colnames(lf@formula)!=colnames(x@formula))) stop("Incompatible atoms in formula.")
  l_atoms <- colnames(lf@formula)
  
  idobj <- as.character(lf)
  idx <- as.character(x)
  
  pok <- !(idx %in% idobj)
  
  if(sum(pok)!=0){
    lf@formula <- rbind(lf@formula,x@formula[pok,,drop=FALSE])
  }
  lf
})



###Method to check that a formula is a subformula.

is_sub_raw <- function(rf1,rf2){
  all(rf1<=rf2)
}


###Vectorized implementation.
setMethod("isSubformula",signature = list(x="LossFormula",lf = "character"),function(x,lf,type=NULL){
  l_atoms <- colnames(x@formula)
  lf <- stringToFormula(lf,vnames=l_atoms)
  vr <-apply(x@formula,1,is_sub_raw,rf2=lf)
  vr
})

###We suppose that he columns are ordered correctly.
setMethod("isSubformula",signature = list(x="integer",lf = "LossFormula"),function(x,lf){
  # l_atoms <- colnames(x@formula)
  # lf <- stringToFormula(lf,vnames=l_atoms)
  vr <-apply(lf@formula,1,is_sub_raw,rf1=x)
  vr
})


setMethod("isSubformula",signature = list(x="LossFormula",lf="LossFormula"),function(x,lf,type=c("any","none")){
  type <- match.arg(type)
  l_atoms <- colnames(x@formula)
  agg <- function(x){x}
  if(type=="any"){
    agg <- any
  }
  
  tm <- t(apply(x@formula,1,function(x,y,agg){
    res <- apply(y,1,function(w,z){
      is_sub_raw(z,w)
    },z=x)
    
    agg(res)
  },y=lf@formula,agg=agg))
  
  agg(tm)
})




setMethod("idxFormula",signature=list(x="character",lf="LossFormula"),function(x,lf){
  vf <- stringToFormula(x,vnames=colnames(lf@formula))
  which(apply(lf@formula, 1, function(x, want) isTRUE(all.equal(x, want)), x))
})

setMethod("idxFormula",signature=list(x="integer",lf="LossFormula"),function(x,lf){
  names(x) <- colnames(lf@formula)
  which(apply(lf@formula, 1, function(x, want) isTRUE(all.equal(x, want)), want=x))
})

setMethod("idxFormula",signature=list(x="LossFormula",lf="LossFormula"),function(x,lf){
  which(apply(lf@formula, 1, function(x, want) isTRUE(all.equal(x, want)), x@formula))
})


#' Show lossFormula object
#'
#' Show the information about a lossFormula object.
#'
#' @param object A lossFormula object.
#'
#' @return None
setMethod("show","LossFormula",function(object){
  cat("A LossFormula object containing",nrow(object@formula),"formula with atoms",paste(colnames(object@formula),collapse = ","))
})

#' Return the number of possible formula for a loss
#'
#' Return the number of possible formula for a loss. Normally not used by the user.
#'
#' @param x A loss formula object.
#'
#' @return The number of possible formula for a loss.
setMethod(length,"LossFormula",function(x){
  nrow(x@formula)
})

#' Selecting a fornula among the different loss formula possible.
#'
#' @param x The Loss formula object
#' @param i The position of the selected formula
#' @param j Not used
#' @param ... Not used
#' @param drop Not used
#'
#' @return The formula as 1xatoms matrix
#' @export
#' 
#' @examples
#' print("Example to be put here")
setMethod('[',"LossFormula",function(x,i,j=NULL,...,drop=TRUE){
  x@formula <- x@formula[i,,drop=FALSE]
  return(x)
})
