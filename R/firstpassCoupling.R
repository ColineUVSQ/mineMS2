

###Convert components furnished to spectral ids.
convertComponent <- function(comp,ids){
	if(is.numeric(comp)){
		trad <- ids[comp]
		if(any(is.na(trad))) stop("Invalid component elements: ",comp[is.na(trad)])
	}else{
		trad <- match(comp,ids)
		if(any(is.na(trad))) stop("Invalid component elements: ",comp[is.na(trad)])
		return(paste("S",trad,sep=""))
	}
}


#' Find pattern explainig components
#'
#' Find pattern maximizing the socre givne in metrics with each components. Threshold may be used to indicate that not match
#' has been found. The majority
#'
#' @param m2l An ms2Lib object
#' @param components A list containing the components to be explained.
#' @param metric The metric used to match a component and the pattern occurence.
#' @param ref_label A set of reference label to be used if the extracted component corresponds to an id over the position of the MS-MS spetra in the file
#' @param threshold The threshold used to discriminate the dataset
#' @param reduced Shall only the filtered patterns be considered
#'
#' @return A list containing one data.frame by component including the following informations:
#'   \code{id} The id of the matched patterns.
#'   \code{f1} The F1-score between the pattern and the component.
#'   \code{accuracy} The accuracy between the pattern and the component.
#'   \code{recall} The recall between the pattern and the component.
#'   \code{miss} The miss rate between the pattern and the component.
#' @export
#'
#' @examples
#' #Loading the data
#' data(m2l)
#' 
#' #Finding explainig component 
#' fp <- findPatternsExplainingComponents(m2l,list(c(15,12)))
#' 
#' #plotting the explaining pattern
#' plot(m2l,fp[[1]]["id"])
findPatternsExplainingComponents <- function(m2l,components,metric=c("f1"),ref_label=NULL,threshold=NA_real_,
                                             reduced=TRUE){

	###We first convert the ids of the components to correct format.
  if(!is.null(ref_label)){
    components <- sapply(components,function(x,ref){
      ref[x]
    },ref=ref_label,simplify = FALSE)
  }

	###We determine the best format for each found pattern.
	bpat <- sapply(components,function(x,m2l,type,reduced){
		find.patterns.class(m2l,x,type = type,full = FALSE,reduced=reduced)
	},m2l=m2l,type=metric,simplify = FALSE,reduced=reduced)

	###Postprocessing
	return(bpat)
}
