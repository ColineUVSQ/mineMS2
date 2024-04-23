#' @include references.R
#' @include labels.R
#' @include LossFormula.R

###Motif plotting function uniquely.

GCD <- function (a, b, extended = FALSE) 
{
  stopifnot(is.numeric(a), is.numeric(b))
  if (length(a) == 1) {
    a <- rep(a, times = length(b))
  }
  else if (length(b) == 1) {
    b <- rep(b, times = length(a))
  }
  n <- length(a)
  e <- d <- g <- numeric(n)
  for (k in 1:n) {
    u <- c(1, 0, abs(a[k]))
    v <- c(0, 1, abs(b[k]))
    while (v[3] != 0) {
      q <- floor(u[3]/v[3])
      t <- u - v * q
      u <- v
      v <- t
    }
    e[k] <- u[1] * sign(a[k])
    d[k] <- u[2] * sign(a[k])
    g[k] <- u[3]
  }
  if (extended) {
    return(list(g = g, c = e, d = d))
  }
  else {
    return(g)
  }
}

LCM <- function (a, b) 
{
  stopifnot(is.numeric(a), is.numeric(b))
  if (length(a) == 1) {
    a <- rep(a, times = length(b))
  }
  else if (length(b) == 1) {
    b <- rep(b, times = length(a))
  }
  g <- GCD(a, b, extended = FALSE)
  return(a/g * b)
}


expandMatrixMult <- function(mat,mult_row=1,mult_col=1){
  te <- apply(mat,2,rep,each=mult_row,simplify=FALSE)
  t(apply(te,1,rep,each=mult_col))
}

layoutMatrix <- function(n,margin=NA)
{
  tm <- NULL
  
	if (n == 1) {
		tm <- (matrix(c(1)))
	}
	if (n == 2) {
		tm <- (matrix(c(1, 2), nrow = (2)))
	}
	if (n == 3) {
		tm <- (matrix(c(1, 2, 3), nrow = (3)))
	}
	if (n == 4) {
		tm <- (matrix(c(1, 2, 3, 4), nrow = (2), byrow = TRUE))
	}
	if (n == 5) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6), nrow = (2), byrow = TRUE))
	}
	if (n == 6) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6), nrow = (2), byrow = TRUE))
	}
	if (n == 7) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = (3),
					  byrow = TRUE))
	}
	if (n == 8) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = (2),
					  byrow = TRUE))
	}
	if (n == 9) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = (3),
					  byrow = TRUE))
	}
	if (n == 10) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
					  nrow = (4), byrow = TRUE))
	}
	if (n == 11) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
					  nrow = (4), byrow = TRUE))
	}
	if (n == 12) {
		tm <- (matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
					  nrow = (4), byrow = TRUE))
	}

  if(!is.na(margin)){
    ntot <- floor(1/margin)
    xtot <- LCM(ntot,ncol(tm))
    xmarg <- xtot%/%ntot
    xvmul <- xtot/ncol(tm)
    ytot <- LCM(ntot,nrow(tm))
    ymarg <- ytot%/%ntot
    yvmul <- ytot/nrow(tm)  
    tm <- expandMatrixMult(tm,yvmul,xvmul)  
    tmx <- matrix(1,nrow=nrow(tm),ncol=xmarg)
    tm <- cbind(tmx,tm+2)
    tmy <- matrix(2,nrow=ymarg,ncol=ncol(tm))
    tm <- rbind(tm,tmy)
  }
  return(tm)
}


###Function which map a pattern on an occurences.
matchMzs <- function(mz1,mz2,ppm=15,dmz=0.03){
	o1 <- order(mz1)
	o2 <- order(mz2)
	closeMatch(mz1[o1],mz2[o2],o1,o2,ppm,dmz)
}


###Mapp the peaks of a pattern on another values.
get_mapping <- function(mg,patg,loss_mass,root=0,tol=0.02,ppm=20){

	###Extracting the correct root idx
	rootidx <- V(mg)[root+1]

	all_mz <- vertex_attr(mg,"mz")

	###We get the mzvalue of all the value after the root
	rootmz <- all_mz[root+1]

	###Computing the root exts labels mg
	m0_idx <- mg[[rootidx,,edges=TRUE]][[1]]
	m0_labs <- edge_attr(mg,"lab",m0_idx)
	m0_mz <- loss_mass[m0_labs]

	###Computing the root exts labels of pattern
	p0_idx <- patg[[1,,edges=TRUE]][[1]]
	p0_labs <- edge_attr(patg,"lab",index=p0_idx)
	p0_mz <- loss_mass[p0_labs]

	###Now we just have to match the value
	res_match <- matchMzs(m0_mz,p0_mz,ppm,tol)

	###Normally there is a match for every value.
	###However if there is a mistake we remove it.
	nna <- which(!is.na(res_match))
	return(matrix(c(1,as.numeric(head_of(patg,p0_idx[res_match[nna]])),
					as.numeric(rootidx),as.numeric(head_of(mg,m0_idx[nna]))),
					byrow = TRUE,nrow=2))
}



#' plotting a fargPattern object.
#'
#' plot a fragPattern associated fragmentation
#' dag using igraph capabilities. This function should not be called directly by the user.
#' Call `plot(m2l,"x")` instead.
#'
#' @param x The pattern to plot.
#' @param y Not used at the moment.
#' @param title The title used for th eplot
#' @param edgeLabels A vector usually passed automatically by the plot method of the ms2Lib object.
#' @param dags A list of mass graphs on which the patterns has been calculated.
#' @param nodeLabel The type of vertex label to be plotted, default means that a will try to be 
#' determined while label show the raw label as integer.
#' @param edgeLabel The type of edges label to be plotted, default means that a will try to be 
#' determined while label show the raw label as integer.
#' @param atoms The used in the formula in the right order.
#' @param formula The formula of the associated dags.
#' @param mzdigits The number of digits included in the plot of mass differences
#' @param vertex_size The size of the vertices.
#' @param vertex_label_cex The size of the vertices label 
#' @param edge_label_cex The size of the edges label 
#' @param subNodes The subset of nodes ot be plotted if necessary
#' @param tkplot Shall an interactiv eplot be shown using tkplot.
#' @param ... Supplmentary arguments passed to the igraph plot function
#'
#' @return None
#'
#' @examples
#' print("Examples ot be put here.")
setMethod("plot", "fragPattern",
		  function(x,
		  		 y = NULL,
		  		 title=NULL,
		  		 edgeLabels = NULL,
		  		 dags = NULL,
		  		 nodeLabel = c("default", "label"),
		  		 edgeLabel = c("formula","mass","none"),
		  		 atoms=c("C","H","O","N","S","P"),
		  		 formula=NA_character_,
		  		 mzdigits = 3,
		  		 vertex_size = 50,
		  		 vertex_label_cex = 0.7, ## 1.2
		  		 edge_label_cex=0.7, ## 0.9
		  		 subNodes = NULL,
		  		 tkplot = FALSE,
		  		 ...) {

		    if(is.null(edgeLabels)) stop("Please use the plot function through the ms2Lib object (ex :plot(m2l,'P15'))")
		  	nodeLabel <- match.arg(nodeLabel)
		  	edgeLabel <- match.arg(edgeLabel)
		  	g <- mm2Graph(x)
		  	###We determine the edge label
		  	oformula <- formula[mm2Occurences(x)[,"gid"]]
		  	filled_formula <- which(sapply(oformula,function(x){(length(x)>0)})&
		  	                          sapply(oformula,function(x){!is.na(x)}))
		  	if(length(filled_formula)==0){
		  	  oformula <- list()
		  	}else{
		  	  oformula <- oformula[filled_formula]
		  	}
		  	
		  	labels <- makeLabelPattern(x,atoms,dags,edgeLabels,oformula=oformula)
		  	
		  	ledges <- NULL
		  	if(edgeLabel=="formula"){
		  	  ledges <- labels$edges
		  	}else if(edgeLabel=="none"){
		  	  ledges <- rep("",length(labels$edges))
		  	}else if(edgeLabel=="mass"){
		  	  ledges <- sprintf("%0.3f",edgeLabels$mz[edge_attr(g,"lab")])
		  	}

		  	###Title
		  	if(is.null(title)){
		  	  title <- paste("Pattern: ", mm2Name(x))
		  	}

		  	if (tkplot) {
		  		tkplot(
		  			g,
		  			layout = (layout_with_sugiyama(g)$layout),
		  			canvas.width = 600,
		  			canvas.height = 600,
		  			vertex.label = labels$vertices,
		  			vertex.size = vertex_size,
		  			edge.label = ledges,
		  			vertex.color = "orange",
		  			main="title",
		  			...
		  		)

		  	} else{
		  		plot.igraph(
		  			g,
		  			layout = (layout_with_sugiyama(g,maxiter=200)$layout),
					#layout = layout_with_fr(g),
		  			vertex.label = labels$vertices,
		  			vertex.size = vertex_size,
		  			vertex.label.cex=vertex_label_cex,
		  			edge.label.cex=edge_label_cex,
		  			edge.label = ledges,
		  			vertex.color = "orange",
		  			main=title,
		  			...
		  		)
		  	}
    return(invisible(list(g,labels$vertices,ledges)))
		  })
#' PLotting the occurences of a fragPattern object
#' 
#' Plot the occurences, the spectra overlayed with the matched peaks of a fragmentation pattern.
#'
#' @param m2l An ms2lib object.
#' @param pidx A pattern id or a frag_pattern object.
#' @param titles A vector of titles to be used. A default tile include id
#' and precursor will bne used by default
#' @param byPage The maximum number of spectra to be plotted by page.
#' @param subOccs Shall some specific occurences be ocnsidered over all the occurences.
#' @param highlights Shall the peaks covered by the pattern be highlighted.
#' @param commonAxis Shall all the spectra be plotted with a common x-axis.
#' @param ... supplementary function to be passed to theplot function.
#'
#' @return A list ocntainng the spectra and their coloring in RGB format
#' @export
#'
#' @examples
#' print("Examples to be put here")
setMethod("plotOccurences", "ms2Lib", function(m2l,
											   pidx,
											   titles = NULL,
											   byPage = 6,
											   subOccs=NULL,
											   highlights=TRUE,
											   commonAxis=FALSE,
											   ...) {
	###Verifying that a correct id have been queried.
  omar <- par("mar")
	fp <- NULL
	if (class(pidx) == "fragPattern") {
		fp <- pidx
	} else if (class(pidx) == "character") {
		if (length(pidx) > 1)
			stop("Only a single index may be plotted every times.")
		fp <- m2l[pidx]
		if (class(fp) != "fragPattern") {
			stop("Wrong id, only a fragPattern object may by used by plotOccurences.")
		}
	}

	if (is.null(titles)){
		if(all(startsWith(mm2Ids(m2l),"S"))){
			titles <- mm2SpectraInfos(m2l)$title
		}else{
			titles <- paste(mm2Ids(m2l)," (S",1:length(mm2Spectra(m2l)),")",sep="")
		}
	}
	
	###We reformat the title if necessary
	if(length(titles)!=length(mm2Spectra(m2l))){
	  ntitles <- rep("",length(mm2Spectra(m2l)))
	  ntitles[fp@occurences[,1]] <- titles
    titles <- ntitles
	}
	
	

	###Extracting the occurences,dags and the graph.
	occs <- mm2Occurences(fp)
	mgs <- mm2Dags(m2l)
	g <- mm2Graph(fp)

	###Mass loss are used for matching.
	loss_mz <- mm2EdgesLabels(m2l)$mz

	if(is.null(subOccs)){
		subOccs <- 1:nrow(occs)
	}else{
		if(all(subOccs>=1)&all(subOccs<=nrow(occs))){
			occs <- occs[subOccs,,drop=FALSE]
		}
	}

	###THE IDX IS HANDLED BY C++ AND SHOULD BE CORRECT.
	occs_gid <- occs[, 1]
	occs_pos <- occs[, 2]
	
	u_occs_gid <- tapply(occs[,2],INDEX = occs[,1],FUN = function(x){return(x)})
	ru_occs_gid <- as.numeric(names(u_occs_gid))
	##We build the colvec_idx
	col_idx_val <- tapply(1:nrow(occs),INDEX = occs[,1],FUN = function(x){return(x)})
	
	col_vec <- rainbow(length(ru_occs_gid))
	
	###We aggregate the spectra
	if(commonAxis){
	  lmat <- layoutMatrix(min(byPage, length(u_occs_gid )),margin = 0.07)
	}else{
	  lmat <- layoutMatrix(min(byPage, length(u_occs_gid )),margin=NA)
	}
	maxv <- max(lmat)-2
	layout(lmat)
	xlims <- NULL
	if(commonAxis){
	  xlims <- c(0,max(sapply(mgs[occs_gid],function(x) max(vertex_attr(x,"mz"))))*1.05)
	  
	}
	
	res_plot <- vector(mode="list",length=length(u_occs_gid))
	for (i in 1:length(u_occs_gid)) {
	  if((i %% 6) == 1){
	    if(i != 1 & commonAxis){
	      layout(1)
	      mtext(expression(bold("m/z")),side=1,padj=2)
	      mtext(expression(bold("intensity")),side=2,padj=-0.2)	
	      layout(lmat)
	    }
	    par(mar=c(0.1,0.1,0.1,0.1))
	    plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1),axes=FALSE,ann=FALSE)
	    plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(-1, 1),axes=FALSE,ann=FALSE)
	    par(mar=c(2.5,2,2,0.5))
	  }
	  gid <- ru_occs_gid[i]
	  all_pos <- u_occs_gid[[i]]
	  
	  all_maps <- sapply(all_pos,function(x,mg,loss_mz,mgs,g){
	    get_mapping(mg=mg, patg=g, loss_mass=loss_mz, root = x)
	  },mg=mgs[[gid]],g=g,loss_mz=loss_mz,simplify=FALSE)
	  
	  ###Plotting of the spectra
	  intv <- vertex_attr(mgs[[gid]], "rel_int")
	  mzs <- vertex_attr(mgs[[gid]], "mz")
	  ids <- V(mgs[[gid]])
	  
	  ###Peaks are split between matched and non matched.
	  matched_peaks_idx <- match(unique(unlist(sapply(all_maps,function(x){x[2,]},simplify=FALSE))), ids)
	  # browser()
	  non_matched_peaks_idx <- seq(1, length(mzs))[-matched_peaks_idx]
	  col_seq <- rep("#000000FF",length(mzs))
	  col_seq[matched_peaks_idx] <- col_vec[i]
	  res_plot[[i]] <- data.frame(int=intv,mz=mzs,col=col_seq)
	  
	  if(!commonAxis){
	    xlims <- c(0, max(mzs) * 1.05)
	    xlabtxt <- "m/z"
	    ylabtxt <- "intensity"
	  }else{
	    xlabtxt <- ""
	    ylabtxt <- ""
	  }
	  plot(
	    mzs[non_matched_peaks_idx],
	    intv[non_matched_peaks_idx],
	    type = "h",
	    col = "#000000FF",
	    lwd = 3,
	    xlim = xlims,
	    ylim = c(0, max(intv * 1.05)),
	    main = titles[gid],
	    xlab = xlabtxt,
	    ylab = ylabtxt,...
	  )
	  
	  ###We add the matched peaks.
	  ccol <- "#000000FF"
	  if(highlights){
	    ccol <- col_vec[i]
	  }
	  points(mzs[matched_peaks_idx],
	         intv[matched_peaks_idx],
	         type = "h",
	         col =ccol,
	         lwd = 3)
	}
	if((i%%6) != 1 & commonAxis){
	  layout(1)
	  mtext(expression(bold("m/z")),side=1,padj=2)
	  mtext(expression(bold("intensity")),side=2,padj=-0.2)	
	}
	par(mar=omar)
	return(invisible(res_plot))
})


