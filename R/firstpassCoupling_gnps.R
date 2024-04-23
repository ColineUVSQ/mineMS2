###This file is ot modify only if there is
ID_COL_GNPS <- "cluster index"


col_gnps <- function(){
  data.frame(name = c("lightyellow1", "yellow", "yellow1", "snow", "snow1", "floralwhite", 
                        "lemonchiffon", "lemonchiffon1", "cornsilk", "cornsilk1", "khaki1", 
                        "seashell", "seashell1", "lavenderblush", "lavenderblush1", "antiquewhite1", 
                        "papayawhip", "lightgoldenrod1", "blanchedalmond", "wheat1", 
                        "mistyrose", "mistyrose1", "bisque", "bisque1", "moccasin", "thistle1", 
                        "navajowhite", "navajowhite1", "peachpuff", "peachpuff1", "gold", 
                        "gold1", "burlywood1", "rosybrown1", "goldenrod1", "pink", "plum1", 
                        "darkgoldenrod1", "lightpink", "pink1", "lightpink1", "tan1", 
                        "orange", "orange1", "lightsalmon", "lightsalmon1", "salmon1", 
                        "darkorange", "orchid1", "palevioletred1", "sienna1", "coral", 
                        "chocolate1", "darkorange1", "coral1", "hotpink1", "indianred1", 
                        "hotpink", "tomato", "tomato1", "orangered", "orangered1", "brown1", 
                        "violetred1", "maroon1", "firebrick1", "deeppink", "deeppink1", 
                        "magenta", "magenta1", "red", "red1", "oldlace", "lightgoldenrodyellow", 
                        "linen", "antiquewhite", "salmon", "ghostwhite", "mintcream", 
                        "whitesmoke", "beige", "wheat", "sandybrown", "azure", "azure1", 
                        "honeydew", "honeydew1", "aliceblue", "khaki", "lightcoral", 
                        "ivory2", "lightyellow2", "yellow2", "snow2", "lemonchiffon2", 
                        "cornsilk2", "palegoldenrod", "khaki2", "seashell2", "lavenderblush2", 
                        "antiquewhite2", "lightgoldenrod", "lightgoldenrod2", "wheat2", 
                        "mistyrose2", "bisque2", "thistle2", "navajowhite2", "peachpuff2", 
                        "gold2", "burlywood2", "rosybrown2", "goldenrod2", "plum2", "darkgoldenrod2", 
                        "pink2", "lightpink2", "tan2", "orange2", "lightsalmon2", "violet", 
                        "salmon2", "orchid2", "palevioletred2", "sienna2", "chocolate2", 
                        "darkorange2", "hotpink2", "coral2", "indianred2", "tomato2", 
                        "orangered2", "brown2", "violetred2", "maroon2", "firebrick2", 
                        "deeppink2", "magenta2", "red2", "darksalmon", "lavender", "lightcyan", 
                        "lightcyan1", "azure2", "honeydew2", "mediumorchid1", "burlywood", 
                        "plum", "gainsboro", "palevioletred", "goldenrod", "orchid", 
                        "thistle", "lightgrey", "lightgray", "tan", "chocolate", "lightcyan2", 
                        "mediumorchid2", "violetred", "ivory3", "lightyellow3", "yellow3", 
                        "snow3", "lemonchiffon3", "cornsilk3", "khaki3", "seashell3", 
                        "lavenderblush3", "antiquewhite3", "lightgoldenrod3", "wheat3", 
                        "mistyrose3", "bisque3", "thistle3", "navajowhite3", "peachpuff3", 
                        "gold3", "burlywood3", "rosybrown3", "goldenrod3", "plum3", "darkgoldenrod3", 
                        "pink3", "lightpink3", "peru", "tan3", "orange3", "lightsalmon3", 
                        "salmon3", "orchid3", "palevioletred3", "sienna3", "chocolate3", 
                        "darkorange3", "hotpink3", "indianred", "coral3", "indianred3", 
                        "tomato3", "orangered3", "brown3", "violetred3", "maroon3", "firebrick3", 
                        "deeppink3", "magenta3", "red3", "darkolivegreen1", "lightsteelblue1", 
                        "mediumvioletred", "slategray1", "darkseagreen1", "azure3", "honeydew3", 
                        "olivedrab1", "lightblue1", "darkorchid1", "darkkhaki", "darkolivegreen2", 
                        "lightsteelblue2", "rosybrown", "paleturquoise1", "mediumorchid", 
                        "slategray2", "darkgoldenrod", "darkseagreen2", "lightcyan3", 
                        "mediumorchid3", "olivedrab2", "lightblue2", "darkorchid2", "firebrick", 
                        "lightskyblue1", "powderblue", "lightsteelblue", "maroon", "paleturquoise", 
                        "paleturquoise2", "greenyellow", "lightblue", "mediumpurple1", 
                        "darkgrey", "darkgray", "brown", "lightskyblue2", "darkolivegreen3", 
                        "lightsteelblue3", "sienna", "purple", "slategray3", "mediumpurple2", 
                        "darkseagreen3", "purple1", "palegreen1", "yellowgreen", "olivedrab3", 
                        "lightblue3", "darkorchid3", "darkorchid", "palegreen", "cadetblue1", 
                        "darkslategray1", "paleturquoise3", "darkviolet", "mediumpurple", 
                        "purple2", "palegreen2", "lightgreen", "darkseagreen", "cadetblue2", 
                        "darkslategray2", "lightskyblue3", "ivory4", "lightyellow4", 
                        "yellow4", "snow4", "lemonchiffon4", "cornsilk4", "seashell4", 
                        "khaki4", "lavenderblush4", "antiquewhite4", "lightgoldenrod4", 
                        "wheat4", "mistyrose4", "bisque4", "thistle4", "navajowhite4", 
                        "peachpuff4", "gold4", "burlywood4", "rosybrown4", "goldenrod4", 
                        "plum4", "darkgoldenrod4", "pink4", "lightpink4", "tan4", "orange4", 
                        "lightsalmon4", "salmon4", "orchid4", "palevioletred4", "sienna4", 
                        "saddlebrown", "chocolate4", "darkorange4", "coral4", "hotpink4", 
                        "indianred4", "tomato4", "orangered4", "brown4", "violetred4", 
                        "maroon4", "firebrick4", "deeppink4", "magenta4", "darkmagenta", 
                        "red4", "darkred", "blueviolet", "mediumpurple3", "skyblue1", 
                        "lightskyblue", "skyblue", "lightslateblue", "azure4", "honeydew4", 
                        "slateblue1", "aquamarine", "aquamarine1", "chartreuse", "chartreuse1", 
                        "skyblue2", "purple3", "lawngreen", "palegreen3", "mediumslateblue", 
                        "cadetblue3", "lightcyan4", "slateblue2", "mediumorchid4", "darkslategray3", 
                        "lightslategray", "lightslategrey", "aquamarine2", "chartreuse2", 
                        "slategray", "slategrey", "darkolivegreen4", "lightsteelblue4", 
                        "skyblue3", "slategray4", "olivedrab", "slateblue", "darkseagreen4", 
                        "olivedrab4", "dimgray", "dimgrey", "slateblue3", "lightblue4", 
                        "darkorchid4", "mediumaquamarine", "aquamarine3", "chartreuse3", 
                        "paleturquoise4", "cornflowerblue", "steelblue1", "lightskyblue4", 
                        "cadetblue", "mediumpurple4", "steelblue2", "darkolivegreen", 
                        "purple4", "seagreen1", "palegreen4", "cadetblue4", "darkslategray4", 
                        "steelblue3", "seagreen2", "skyblue4", "mediumturquoise", "royalblue1", 
                        "darkslateblue", "slateblue4", "steelblue", "aquamarine4", "chartreuse4", 
                        "seagreen3", "royalblue2", "royalblue", "turquoise", "mediumseagreen", 
                        "royalblue3", "steelblue4", "limegreen", "darkslategray", "darkslategrey", 
                        "seagreen", "seagreen4", "royalblue4", "forestgreen", "lightseagreen", 
                        "dodgerblue", "dodgerblue1", "dodgerblue2", "midnightblue", "dodgerblue3", 
                        "dodgerblue4", "cyan", "cyan1", "springgreen", "springgreen1", 
                        "green", "green1", "mediumspringgreen", "turquoise1", "cyan2", 
                        "springgreen2", "green2", "turquoise2", "darkturquoise", "cyan3", 
                        "springgreen3", "green3", "turquoise3", "deepskyblue", "deepskyblue1", 
                        "deepskyblue2", "deepskyblue3", "cyan4", "darkcyan", "springgreen4", 
                        "green4", "turquoise4", "deepskyblue4", "darkgreen", "blue", 
                        "blue1", "blue2", "mediumblue", "blue3", "blue4", "darkblue", 
                        "navy", "navyblue", "black"), rgb = c("#FFFFE0", "#FFFF00", "#FFFF00", "#FFFAFA", "#FFFAFA", 
                                                             "#FFFAF0", "#FFFACD", "#FFFACD", "#FFF8DC", "#FFF8DC", "#FFF68F", 
                                                             "#FFF5EE", "#FFF5EE", "#FFF0F5", "#FFF0F5", "#FFEFDB", "#FFEFD5", 
                                                             "#FFEC8B", "#FFEBCD", "#FFE7BA", "#FFE4E1", "#FFE4E1", "#FFE4C4", 
                                                             "#FFE4C4", "#FFE4B5", "#FFE1FF", "#FFDEAD", "#FFDEAD", "#FFDAB9", 
                                                             "#FFDAB9", "#FFD700", "#FFD700", "#FFD39B", "#FFC1C1", "#FFC125", 
                                                             "#FFC0CB", "#FFBBFF", "#FFB90F", "#FFB6C1", "#FFB5C5", "#FFAEB9", 
                                                             "#FFA54F", "#FFA500", "#FFA500", "#FFA07A", "#FFA07A", "#FF8C69", 
                                                             "#FF8C00", "#FF83FA", "#FF82AB", "#FF8247", "#FF7F50", "#FF7F24", 
                                                             "#FF7F00", "#FF7256", "#FF6EB4", "#FF6A6A", "#FF69B4", "#FF6347", 
                                                             "#FF6347", "#FF4500", "#FF4500", "#FF4040", "#FF3E96", "#FF34B3", 
                                                             "#FF3030", "#FF1493", "#FF1493", "#FF00FF", "#FF00FF", "#FF0000", 
                                                             "#FF0000", "#FDF5E6", "#FAFAD2", "#FAF0E6", "#FAEBD7", "#FA8072", 
                                                             "#F8F8FF", "#F5FFFA", "#F5F5F5", "#F5F5DC", "#F5DEB3", "#F4A460", 
                                                             "#F0FFFF", "#F0FFFF", "#F0FFF0", "#F0FFF0", "#F0F8FF", "#F0E68C", 
                                                             "#F08080", "#EEEEE0", "#EEEED1", "#EEEE00", "#EEE9E9", "#EEE9BF", 
                                                             "#EEE8CD", "#EEE8AA", "#EEE685", "#EEE5DE", "#EEE0E5", "#EEDFCC", 
                                                             "#EEDD82", "#EEDC82", "#EED8AE", "#EED5D2", "#EED5B7", "#EED2EE", 
                                                             "#EECFA1", "#EECBAD", "#EEC900", "#EEC591", "#EEB4B4", "#EEB422", 
                                                             "#EEAEEE", "#EEAD0E", "#EEA9B8", "#EEA2AD", "#EE9A49", "#EE9A00", 
                                                             "#EE9572", "#EE82EE", "#EE8262", "#EE7AE9", "#EE799F", "#EE7942", 
                                                             "#EE7621", "#EE7600", "#EE6AA7", "#EE6A50", "#EE6363", "#EE5C42", 
                                                             "#EE4000", "#EE3B3B", "#EE3A8C", "#EE30A7", "#EE2C2C", "#EE1289", 
                                                             "#EE00EE", "#EE0000", "#E9967A", "#E6E6FA", "#E0FFFF", "#E0FFFF", 
                                                             "#E0EEEE", "#E0EEE0", "#E066FF", "#DEB887", "#DDA0DD", "#DCDCDC", 
                                                             "#DB7093", "#DAA520", "#DA70D6", "#D8BFD8", "#D3D3D3", "#D3D3D3", 
                                                             "#D2B48C", "#D2691E", "#D1EEEE", "#D15FEE", "#D02090", "#CDCDC1", 
                                                             "#CDCDB4", "#CDCD00", "#CDC9C9", "#CDC9A5", "#CDC8B1", "#CDC673", 
                                                             "#CDC5BF", "#CDC1C5", "#CDC0B0", "#CDBE70", "#CDBA96", "#CDB7B5", 
                                                             "#CDB79E", "#CDB5CD", "#CDB38B", "#CDAF95", "#CDAD00", "#CDAA7D", 
                                                             "#CD9B9B", "#CD9B1D", "#CD96CD", "#CD950C", "#CD919E", "#CD8C95", 
                                                             "#CD853F", "#CD853F", "#CD8500", "#CD8162", "#CD7054", "#CD69C9", 
                                                             "#CD6889", "#CD6839", "#CD661D", "#CD6600", "#CD6090", "#CD5C5C", 
                                                             "#CD5B45", "#CD5555", "#CD4F39", "#CD3700", "#CD3333", "#CD3278", 
                                                             "#CD2990", "#CD2626", "#CD1076", "#CD00CD", "#CD0000", "#CAFF70", 
                                                             "#CAE1FF", "#C71585", "#C6E2FF", "#C1FFC1", "#C1CDCD", "#C1CDC1", 
                                                             "#C0FF3E", "#BFEFFF", "#BF3EFF", "#BDB76B", "#BCEE68", "#BCD2EE", 
                                                             "#BC8F8F", "#BBFFFF", "#BA55D3", "#B9D3EE", "#B8860B", "#B4EEB4", 
                                                             "#B4CDCD", "#B452CD", "#B3EE3A", "#B2DFEE", "#B23AEE", "#B22222", 
                                                             "#B0E2FF", "#B0E0E6", "#B0C4DE", "#B03060", "#AFEEEE", "#AEEEEE", 
                                                             "#ADFF2F", "#ADD8E6", "#AB82FF", "#A9A9A9", "#A9A9A9", "#A52A2A", 
                                                             "#A4D3EE", "#A2CD5A", "#A2B5CD", "#A0522D", "#A020F0", "#9FB6CD", 
                                                             "#9F79EE", "#9BCD9B", "#9B30FF", "#9AFF9A", "#9ACD32", "#9ACD32", 
                                                             "#9AC0CD", "#9A32CD", "#9932CC", "#98FB98", "#98F5FF", "#97FFFF", 
                                                             "#96CDCD", "#9400D3", "#9370DB", "#912CEE", "#90EE90", "#90EE90", 
                                                             "#8FBC8F", "#8EE5EE", "#8DEEEE", "#8DB6CD", "#8B8B83", "#8B8B7A", 
                                                             "#8B8B00", "#8B8989", "#8B8970", "#8B8878", "#8B8682", "#8B864E", 
                                                             "#8B8386", "#8B8378", "#8B814C", "#8B7E66", "#8B7D7B", "#8B7D6B", 
                                                             "#8B7B8B", "#8B795E", "#8B7765", "#8B7500", "#8B7355", "#8B6969", 
                                                             "#8B6914", "#8B668B", "#8B6508", "#8B636C", "#8B5F65", "#8B5A2B", 
                                                             "#8B5A00", "#8B5742", "#8B4C39", "#8B4789", "#8B475D", "#8B4726", 
                                                             "#8B4513", "#8B4513", "#8B4500", "#8B3E2F", "#8B3A62", "#8B3A3A", 
                                                             "#8B3626", "#8B2500", "#8B2323", "#8B2252", "#8B1C62", "#8B1A1A", 
                                                             "#8B0A50", "#8B008B", "#8B008B", "#8B0000", "#8B0000", "#8A2BE2", 
                                                             "#8968CD", "#87CEFF", "#87CEFA", "#87CEEB", "#8470FF", "#838B8B", 
                                                             "#838B83", "#836FFF", "#7FFFD4", "#7FFFD4", "#7FFF00", "#7FFF00", 
                                                             "#7EC0EE", "#7D26CD", "#7CFC00", "#7CCD7C", "#7B68EE", "#7AC5CD", 
                                                             "#7A8B8B", "#7A67EE", "#7A378B", "#79CDCD", "#778899", "#778899", 
                                                             "#76EEC6", "#76EE00", "#708090", "#708090", "#6E8B3D", "#6E7B8B", 
                                                             "#6CA6CD", "#6C7B8B", "#6B8E23", "#6A5ACD", "#698B69", "#698B22", 
                                                             "#696969", "#696969", "#6959CD", "#68838B", "#68228B", "#66CDAA", 
                                                             "#66CDAA", "#66CD00", "#668B8B", "#6495ED", "#63B8FF", "#607B8B", 
                                                             "#5F9EA0", "#5D478B", "#5CACEE", "#556B2F", "#551A8B", "#54FF9F", 
                                                             "#548B54", "#53868B", "#528B8B", "#4F94CD", "#4EEE94", "#4A708B", 
                                                             "#48D1CC", "#4876FF", "#483D8B", "#473C8B", "#4682B4", "#458B74", 
                                                             "#458B00", "#43CD80", "#436EEE", "#4169E1", "#40E0D0", "#3CB371", 
                                                             "#3A5FCD", "#36648B", "#32CD32", "#2F4F4F", "#2F4F4F", "#2E8B57", 
                                                             "#2E8B57", "#27408B", "#228B22", "#20B2AA", "#1E90FF", "#1E90FF", 
                                                             "#1C86EE", "#191970", "#1874CD", "#104E8B", "#00FFFF", "#00FFFF", 
                                                             "#00FF7F", "#00FF7F", "#00FF00", "#00FF00", "#00FA9A", "#00F5FF", 
                                                             "#00EEEE", "#00EE76", "#00EE00", "#00E5EE", "#00CED1", "#00CDCD", 
                                                             "#00CD66", "#00CD00", "#00C5CD", "#00BFFF", "#00BFFF", "#00B2EE", 
                                                             "#009ACD", "#008B8B", "#008B8B", "#008B45", "#008B00", "#00868B", 
                                                             "#00688B", "#006400", "#0000FF", "#0000FF", "#0000EE", "#0000CD", 
                                                             "#0000CD", "#00008B", "#00008B", "#000080", "#000080", "#000000"
                        ),stringsAsFactors = FALSE)
  }



generateCol <- function(ncomp){
  cgnps <- col_gnps()
  ###If needed we expand the values
  while(nrow(cgnps)<ncomp){
    cgnps <- rbind(cgnps,col_gnps())
  }
  vinter <- seq(1,nrow(cgnps),length=ncomp+1)
  cgnps[round(vinter+(vinter[2]-vinter[1])*0.5),1]
}


####Eventually first removing isolated points.
findAllCliques <- function(net_gnps,minSize = 3,vname="cluster index"){
	g <- induced_subgraph(net_gnps,V(net_gnps))

	big_clique <- largest_cliques(g)[[1]] ## first largest clique (clique maximum)

	pos_list <- 1
	list_cliques <- vector(mode="list",length=16) ## to store cliques

	while(length(big_clique)>=minSize){

		if(pos_list>length(list_cliques)){
			list_cliques <- c(list_cliques,vector(mode="list",length=length(list_cliques)))
		}
		list_cliques[[pos_list]] <- vertex_attr(g,name=vname,index=big_clique) ## name : attribute to retrieve, index : set of vertices
		pos_list <- pos_list+1
		###We remove the vertices
		g <- delete_vertices(g,big_clique)
		big_clique <- largest_cliques(g)[[1]]
	}

	return(list_cliques[1:max(pos_list-1,1)])
}


findConnectedComponents <- function(net,minSize = 2,vname="cluster index",...){
	comp <- components(net,mode="weak")
	lelem <- sapply(comp$csize,numeric,simplify=FALSE)

	vec_idx <- rep(1,length(lelem))


	###We create a component list.
	Vlist <- V(net)
	for(i in seq_along(comp$membership)){
		lelem[[comp$membership[i]]][vec_idx[comp$membership[i]]] <- i
		vec_idx[comp$membership[i]] <- vec_idx[comp$membership[i]]+1
	}

	###We remove all the components with a size inferior tp mineSize
	lelem <- lelem[sapply(lelem,length)>=minSize]

	lapply(lelem,function(x,vidx,vname,net){vertex_attr(net,vname,index=vidx[x])},
		   vidx=Vlist,vname=vname,net=net)
}

# rcomp <- findConnectedComponents(net_gnps)

#' Find the annotable component of a GNPS network.
#'
#' Extract the annotated compoents of a network, at the moment, the extracted compoents are limited ot the clique.
#'
#' @param net A gnps network
#' @param minSize The minimum size of the detected clique.
#' @param pairThreshold A threshold used to discard the edges to detect components.
#' @param vname The name of the exported index of the component as verte attribute.
#' @param eattr The name of the considered similarity measure on the network.
#' @param convert_component Shall the components be 
#'
#' @return A list of the componets to be checked.
#' @export
#'
#' @examples
#' print("Examples to be put here")
findGNPSComponents <- function(net,minSize = 3,pairThreshold=0.9,vname="cluster index",
                               eattr = "cosine_score", convert_component = TRUE){

  if(convert_component){
    
  }
  
	###Finding all cliques.
	cliques <- findAllCliques(net,minSize = minSize,vname = vname)

	###Finding all connected components.
	connected_components <- findConnectedComponents(net,vname=vname,minSize=2)

	###We check for all  the connected components that any of the connected_components is a clique.

	###All the components are compared sequentially.
	seqSizeClique <- sapply(cliques,length) ## cree une liste avec la taille des cliques
	seqSizeConnected <- sapply(connected_components,length) ## cree une liste avec la taille des composantes connexes

	maxSize <-max(seqSizeClique)

	to_rm <- numeric(0)

	###We compare all the clique of similar size
	for(s in maxSize:1){
		pCliques <- which(seqSizeClique==s) ## indice(s) des cliques de taille s
		if(length(pCliques)==0) next
		pConnected <- which(seqSizeConnected==s) ## indice(s) des ccmposantes connexes de taille s
		if(length(pConnected)==0) next


		###Else we do the comparison.
		for(p1 in pCliques){
			for(p2 in pConnected){
				if(setequal(cliques[[p1]],connected_components[[p2]]))
					to_rm <- c(to_rm,p2)
			}
		}
	}

	###We filter the connected components list.

	pComp <- 1:length(connected_components)

	if(length(to_rm)!=0)	pComp <- pComp[-to_rm]

	if(length(pComp)!=0) connected_components <- connected_components[pComp]


	####Similarities passing the pairThreshold threshold are extracted.

	###Removing all the vertex from cliques and 2 pairs connected components
	Vlist <- V(net)
	Vattr <- vertex_attr(net,vname)

  ## set of connected components of size 2
	pairs <- connected_components[sapply(connected_components,length)==2]

  # on concatène les cliques et les paires de sommets
	pcliques <- unique(do.call("c",c(cliques,pairs)))

	in_cliques <- match(Vattr,pcliques) ## vertices in cliques 
	out_cliques <- which(is.na(in_cliques)) ## vertices not in cliques 


	###All the edges are considered

	E_edges <-  E(net)[ Vlist %--% Vlist[out_cliques] ]
	E_edges <- E_edges[which(
		as.numeric(edge_attr(net,eattr,E_edges))>pairThreshold)]

	###Creating the components from the edges.
	highsim <- sapply(E_edges,function(x,net){
		c(head_of(net,x),tail_of(net,x))
	},net=net,simplify=FALSE)

	highsim <- sapply(highsim,function(x,net,vname){
		vertex_attr(net,vname,index=x)
	},net=net,vname=vname,simplify=FALSE)

	###Return the components.
	return(c(cliques,connected_components,highsim))
}


##Given a list of components, return for each vertices the index of the ocmpoents to which the vertex belong.


makeIdxTable <- function(components,maxVertices=70){

	res <- vector(mode="list",length=maxVertices)
	for(ir in seq_along(res)){
		res[[ir]] <- numeric(4)
	}
	seqidx <- rep(0,maxVertices)

	for(icomp in seq_along(components)){
		for(ic in components[[icomp]]){
			if((seqidx[ic]+1)>length(res[[ic]])){
				res[[ic]] <- c(res[[ic]],rep(NA_real_,length(res[[ic]])))
			}
			seqidx[ic] <- seqidx[ic]+1
			res[[ic]][seqidx[ic]] <- icomp

		}
	}

	for(ir in seq_along(res)){
		if(seqidx[ir]>=1){
		res[[ir]] <- res[[ir]][1:seqidx[ir]]
		}else{
			res[[ir]] <- numeric()
		}
	}
	return(res)
}


#' Annotation of GNPS network
#'
#' @param components The components of the GNPS network as integer.
#' @param net The GNPS network as an igraph object
#' @param patterns The set of patterns extract from an ms2Lib object.
#' @param copy Shall the igraph object be copied (recommended)
#' @param keepattr Which attributes sahll be kept on the ntwork
#' @param sep_infos The seprator used to separate the colors when multiple colors are associated ot a single node.
#'
#' @return The annotated netowrk.
#' @export
#'
#' @examples
#' print("Examples to be put here")
annotateNetwork <- function(components,net,patterns,copy=TRUE,
							keepattr = c("Compound_Name","SpectrumID","DefaultGroups",
										 "cluster index","precursor mass"),sep_infos=","){

  ##We always connect the component to the new referential
  components <- sapply(components,function(x,ref){
    match(x,ref)},ref=vertex_attr(net,"pat"),simplify = FALSE)
  
	if(!is.null(keepattr)){
		if(copy) net <- igraph::induced_subgraph(net, V(net))
		attrnames <- igraph::vertex_attr_names(net)
		to_rm <- attrnames[is.na(match(attrnames,keepattr))]

		for(trm in to_rm){
		 net <- igraph::delete_vertex_attr(net, trm)
		}
	}

	DEFAULT <- "white"
	# COLS_SEQ <- c("blue","green","green","orange","red","yellow","purple","cyan")
	COLS_SEQ <- generateCol(length(components))

	###We reorder every nodes order according to the definition
	ids_gnps <- as.numeric(vertex_attr(net,name = ID_COL_GNPS))

	resComp <- makeIdxTable(components,vcount(net))
	###Generating the col str.
	colstr <- sapply(resComp,function(x,colseq,defcol){
		if(length(x)>0){
		return(paste(colseq[x],collapse = ","))
		}else{
			return(defcol)
		}
	},defcol=DEFAULT,colseq=COLS_SEQ)

	legstr <- paste(paste('stripechart: colorlist="',colstr,sep=""),'"',sep="")
	ids <- sapply(patterns,function(x,sep){
		paste(x[,"id"],collapse=sep)
	},sep=sep_infos)

	names(legstr) <- as.character(ids_gnps)

	rlist <- list()
	for(ne in colnames(patterns[[1]])){
    if(ne == "id") ne2 <- "pat"
    else ne2 <- ne
		rlist[[ne2]] <- sapply(patterns,function(x,sep,cname){
			tempt <- x[,cname]
			if(is.numeric(tempt)) tempt <- sprintf("%0.2f",tempt)
			paste(tempt,collapse=sep)
		},sep=sep_infos,cname= ne)
	}

	###We extract the maximum F1-score from this dataset.
	maxF1 <- sapply(patterns,function(x,sep){
		max(x[,"f1"],collapse=sep)
	},sep=sep_infos)

	str_components <- sapply(components,function(x,sep){
		paste(x,collapse = sep)
	},sep=sep_infos)

	temp_leg <- sapply(resComp,function(x,attstr,sep){
		if(length(x)==0) return("")
		return(paste(attstr[x],collapse = sep))
	},attstr=str_components,sep="|")
	
	names(temp_leg) <- as.character(ids_gnps)
	
	net <- set_vertex_attr(graph = net,name = "components",
	                       value = unlist(temp_leg[as.character(ids_gnps)]))
	
	id_comp_gnps <- as.character(seq_along(components))


	temp_leg <- sapply(resComp,function(x,attstr,sep){
	  if(length(x)==0) return("")
	  return(paste(attstr[x],collapse = sep))
	},attstr=id_comp_gnps,sep="|")
	
	names(temp_leg) <- as.character(ids_gnps)
	
	net <- set_vertex_attr(graph = net,name = "IdxComponents",
	                       value = unlist(temp_leg[as.character(ids_gnps)]))


	net <- set_vertex_attr(graph = net,name = "colorComponents",
	                       value = unlist(legstr[as.character(ids_gnps)]))

	###We now add the supplementary informations

	for(ne in names(rlist)){
		temp_leg <- sapply(resComp,function(x,attstr,sep){
			if(length(x)==0) return("")
			return(paste(attstr[x],collapse = sep))
		},attstr=rlist[[ne]],sep=sep_infos)
		net <- set_vertex_attr(graph = net,name = ne,value = temp_leg[ids_gnps])
	}

	###The consensus F1 score.
	temp_leg <- sapply(resComp,function(x,attstr,sep){
		if(length(x)==0) return("")
		return(paste(attstr[x],collapse = sep))
	},attstr=maxF1,sep=sep_infos)
	net <- set_vertex_attr(graph = net,name = "maxF1",value = temp_leg[ids_gnps])
	return(net)
}



