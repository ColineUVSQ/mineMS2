####Implementation of the selection functions.


checkSize <- function(vec,pos){
  if(typeof(vec) %in% c("numeric","integer","character","logical")){
  	if(length(vec)<pos){
  		vec <- c(vec,do.call(typeof(vec),args=list(length(vec))))
  	}
  	return(vec)
  }
  
  if(is.data.frame(vec)){
    if(nrow(vec)<pos){
      vec <- vec[rep(1:(nrow(vec)),times=2),]
    }
    return(vec)
  }
  
  if(is.list(vec)){
    if(length(vec)<pos){
      vec <- c(vec,vector(mode="list",length=length(vec)))
    }
    return(vec)
  }
  
  stop("Unrecognized data type :",typeof(vec))
}

#' Selection function
#'
#' @param m2l An ms2Lib object.
#' @param ids Valid IDs of objects.
#' @param vals the type of value to be returned. Only certain combinations are allowed.
#'
#' @return The objects of the correspodning type.
#' @export
#'
#' @examples
#' print("Examples to be put here")
select <- function(m2l,ids,vals=c("P","S","L")){
	if(class(m2l)!="ms2Lib") stop("m2l should be an ms2Lib object.")

	vals <- match.arg(vals)

	ids <- sapply(ids,parseId,m2l=m2l,simplify = FALSE)

	type <- unique(sapply(ids,'[[',i=1))

	if(length(type)!=1) stop("All the ids should be of the same type.")

	nums <- sapply(ids,'[[',i=2)

	if(type=="spectra"){
		if(vals=="P"){
			return(sapply(nums,select.patterns.spectra,m2l=m2l,simplify = FALSE))
		}else{
			stop("not implemented")
		}
	}else if(type=="losses"){
		if(vals=="P"){
			return(sapply(nums,select.patterns.losses,m2l=m2l,simplify = FALSE))
		}else if(vals=="S"){
			return(sapply(nums,select.spectra.losses,m2l=m2l,simplify = FALSE))
		}else{
			stop("not implemented")
		}
	}else if(type=="patterns"){
		stop("not implemented")
	}else if(type=="fragments"){
		stop("not implemented")
	}
}

#' find patterns or spectra form a set of pattern or spectra ids
#'
#' find data of type "vals" containing all the ids listed in "ids"
#'
#' @param m2l an ms2Lib Object.
#' @param ids A list of ids to be searched for.
#' @param vals The type of the returned values.
#' @param ... supp arguments.
#'
#' @return The ids of the patterns or spectra cotaining all the ids.
#' @export
#'
#' @examples
#' print("examples to be put here")
find <- function(m2l,ids,vals=c("P","S"),...){
	if(class(m2l)!="ms2Lib") stop("m2l should be an ms2Lib object.")

	vals <- match.arg(vals)

	ids <- sapply(ids,parseId,m2l=m2l,simplify = FALSE)

	type <- unique(sapply(ids,'[[',i=1))

	if(length(type)!=1) stop("All the ids should be of the same type.")
	if(!(type %in% c("patterns","spectra"))){
		stop("ids should correspond to spectra or patterns.")
	}
	nums <- unique(sapply(ids,'[[',i=2))
	if((type=="spectra") && (vals=="P")){
		return(find.patterns.spectra(m2l,nums,...))
	}

	if((type=="patterns") && (vals=="S")){
		stop("Not implemented (soon)")
	}
	stop("Comparison between ",type," and ",vals," not implemented.")
}


find.patterns.spectra<- function(m2l,ids,reduced=FALSE,metric = c("coverage","size","none"),partial = TRUE){
  
  metric <- match.arg(metric)
  
	idsp <- vrange(m2l,"P",reduced)
	
	if((!hasCoverage(m2l@patterns[[1]])) & (metric=="coverage")) stop("use calculateCoverage function before searching with type to coverage") 
	
	info_patt_full <- data.frame(id=character(10),size=numeric(10),inter=numeric(10),stringsAsFactors = FALSE)
	info_patt_partial <- data.frame(id=character(10),size=numeric(10),inter=numeric(10),stringsAsFactors = FALSE)
	
	if(hasCoverage(m2l@patterns[[1]])){
	  info_patt_full$coverage <- numeric(10)
	  info_patt_partial$coverage <- numeric(10)
	}

	nsol <- 0
	nsol_partial <- 0
	for(ip in seq_along(idsp)){
		temp_pat <- m2l[idsp[ip]]
		occs <- mm2Occurences(temp_pat)
		vm <- (ids %in% occs[,"gid"])

		if(all(vm)){
			nsol <- nsol+1
			info_patt_full <- checkSize(info_patt_full,nsol)
			info_patt_full$id[nsol] <- idsp[ip]
			info_patt_full$size[nsol] <- vcount(temp_pat@graph)-1
			info_patt_full$inter[nsol] <- sum(vm)
			if(hasCoverage(m2l@patterns[[1]])){
			  info_patt_full$coverage[nsol] <- median(occs[,"coverage"])
			}
		}
		if(any(vm)){
		  nsol_partial <- nsol_partial+1
		  info_patt_partial <- checkSize(info_patt_partial,nsol_partial)
		  info_patt_partial$id[nsol_partial] <- idsp[ip]
		  info_patt_partial$size[nsol_partial] <- vcount(temp_pat@graph)-1
		  info_patt_partial$inter[nsol_partial] <- sum(vm)
		  
		  if(hasCoverage(m2l@patterns[[1]])){
		    info_patt_partial$coverage[nsol_partial] <- median(occs[,"coverage"])
		  }
		}
	}
	resids <- NULL
	if(nsol!=0){
	  info_patt_full <- info_patt_full[1:nsol,]
		if(metric != "none"){
		  resids <- info_patt_full$id[which.max(info_patt_full[[metric]])]
		}
	  if(metric == "none") resids <- info_patt_full$id[info_patt_full$inter>=2]
	}else{
		if(partial){
			if(nsol_partial != 0){
				resids <- info_patt_partial$id[which.max(info_patt_full[[metric]])]
			}
		}
	}
	
	return(resids)
}



###select.DATAQUERIED.IDSTYPE

all.patterns.spectra <- function(m2l,reduced=TRUE){
	ids <- vrange(m2l,"P",reduced=reduced)
	tp <- m2l[ids]
	sapply(patterns_from_spectra(tp,
								 length(m2l)),
		   function(x,ids){if(length(x)>0){
		   	return(ids[x])
		   	}else{
		   		return(x)
		   	}},ids=ids)
}




all.patterns.losses <- function(m2l,reduced=TRUE){
	ids <- vrange(m2l,"P",reduced=reduced)
	tp <- m2l[ids]
	num_losses <- nrow(mm2EdgesLabels(m2l))
	reslist <- vector(mode = "list",length = num_losses)
	for(i in seq_along(reslist)){
		reslist[[i]] <- character(3)
	}
	idxvec <- rep(1,num_losses)

	###Replace by C++ code.
	for(i in seq_along(tp)){
		labs <- edge_attr(mm2Graph(tp[[i]]),"lab")
		for(l in labs){
			reslist[[l]] <- checkSize(reslist[[l]],idxvec[l])
			reslist[[l]][idxvec[l]] <- ids[i]
			idxvec[l] <- idxvec[l]+1
		}
	}

	###pos processing
	for(i in seq_along(reslist)){
		if(idxvec[i]>1){
			reslist[[i]] <- reslist[[i]][1:(idxvec[i]-1)]
		}else{
			reslist[[i]] <- character(0)
		}
	}

	return(reslist)
}

select.patterns.spectra <- function(m2l,id){
	temp <- select_patterns_from_spectra(mm2Patterns(m2l),as.integer(id))
	if(length(temp)>0) return(paste("P",temp,sep=""))
}


select.patterns.losses <- function(m2l,id){
	temp <- which(sapply(mm2Patterns(m2l),function(x,id){
		id %in% edge_attr(mm2Graph(x),"lab")
	},id=id))
	if(length(temp)>0) return(paste("P",temp,sep=""))
}

select.spectra.losses <- function(m2l,id){
	temp <- which(sapply(mm2Dags(m2l),function(x,id){
		id %in% edge_attr(x,"lab")
	},id=id))
	if(length(temp)>0) return(paste("S",temp,sep=""))
}



###Return f1-score,precision,recall
f1.score <- function(id,idsref,m2l,full=FALSE){
	## id : ens de spectres (de loss graphs) dans lesquels sont retrouvés un pattern particulier 
	## idsref : ens de spectres qui appartiennent à une même composante de GNPS (clique, composante connexe ou arete de forte similarité)
	

	## adds: to take the right set of spectra
	if("N" %in% colnames(m2l@spectraInfo))
	{
		spectres <- m2l@spectraInfo['N']
		id <- spectres[rownames(spectres) %in% id,] 
	}

	#if(length(idsref) == 4 && idsref[1] == 161 && idsref[2] == 162 && idsref[3] == 572 && idsref[4] == 729 && length(id) == 4)
	#{
	#	message("Igor")
	#	print(id)
	#	print(idsref)
	#}

	###Calculating the intersection
	
	inter <- intersect(id,idsref)
	if(length(inter)==0) return(rep(NA_real_,4))
	if(full&(length(inter)!=length(id))) return(rep(NA_real_,4))
	union <- union(id,idsref)

	recall <- (length(inter)/length(idsref))
	precision <- (length(inter)/length(id))
	miss_rate <- (length(idsref)-length(inter))/length(idsref)

	return(c(2*recall*precision/(recall+precision),precision,recall,miss_rate))
}




checkFTerms <- function(seq_terms){
	REF_TERMS <- c("f1","precision","recall","miss","size")
	if(all(seq_terms %in% REF_TERMS)){
		return(seq_terms)
	}else{
		stop("Unknown term(s): ",paste(seq_terms[!(seq_terms %in% REF_TERMS)],sep=", "), "authorized terms are: ",paste(REF_TERMS,sep=", "))
	}
}


#' Find biggest F1 score
#'
#' Given a list of molecules ids as a character or integer input, find the pattern maximizing the F1 score of
#' the molecules.
#'
#' @param m2l An ms2Lib object.
#' @param ids Valid IDs of objects, it mays be a chracter vector with the prefix "P" or an integer or numeric vector
#' @param type Which criteria is used to determine the best match, F-score, precision or acccuracy
#' @param returnall Shall all the patterns with a similar F1 score be returned.
#' @param full Shall only full matches be returned.
#' @param reduced Shall only the reduced set of patterns be considered.
#' @details The computation of this F1 score.
#'
#' @return A data.frame containing two slots, id the ids of the elements and f1_score the f1 score of the elements
#' @export
#'
#' @examples
#' print("Examples to be put here")
find.patterns.class <- function(m2l,ids,type=c("f1","precision","size"),
								returnall=FALSE,full=TRUE,reduced=FALSE){

	criterion <- checkFTerms(type)
	
	idsp <- vrange(m2l,"P",reduced=reduced) ## all patterns
	###The P is removed if needed.
	if(is.character(ids)&&(startsWith(ids[1],"S"))) ids <- str_sub(ids,2)

	ids <- as.numeric(ids) ## une composante (clique, composante connexe ...)
	
	###We find the best matching ones
	vf1 <- sapply(idsp,function(x,idr,m2l,fullv){
		c(f1.score(unique(m2l[x]@occurences[,1]),idr,m2l,full=fullv),vcount(m2l[x]@graph))
	},idr=ids,m2l=m2l,fullv=full)
	
	rownames(vf1) <- c("f1","precision","recall","miss","size")
	
	###Two cases, single maximum return or the full list of ranked values returneed.
	to_return <- NULL

	###In the two cases only the non-na are considered
	pf1 <- which(!is.na(vf1["f1",]))

	if(length(pf1)==0) return(data.frame(id=character(0),f1=numeric(0),precision=numeric(0),
										 recall=numeric(0),miss=numeric(0),size=numeric(0)))
	to_return <- data.frame(id=idsp[pf1],f1=vf1["f1",pf1],
							precision=vf1["precision",pf1],recall=vf1["recall",pf1],
							miss=vf1["miss",pf1],size=vf1["size",pf1],stringsAsFactors = FALSE)

	to_return <- to_return[do.call(order, c(decreasing = TRUE, data.frame(to_return[,criterion]))),]

	if(!returnall){
		maxval <- to_return[1,criterion]
		posret <- 1
		while((posret < nrow(to_return)) && (!is.na(to_return[posret+1,1]))&&
			  all(to_return[posret+1,criterion] == maxval)){
			posret <- posret+1
		}
		to_return <- to_return[1:posret,]
	}
	return(to_return)
}
