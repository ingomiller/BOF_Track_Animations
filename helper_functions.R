###### MoveVis custom helper functions 


## helper function to change text manaully and add bold option
add_text_manual <- function(frames, labels, x, y, colour = "black", size = 3, fontface = "plain", type = "text", verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  
  if(!is.character(labels)) out("Argument 'labels' must be of type 'character'.", type = 3)
  if(!is.character(colour)) out("Argument 'colour' must be of type 'character'.", type = 3)
  if(!is.numeric(x)) out("Argument 'x' must be of type 'numeric'.", type = 3)
  if(!is.numeric(y)) out("Argument 'y' must be of type 'numeric'.", type = 3)
  if(!is.numeric(size)) out("Argument 'size' must be of type 'numeric'.", type = 3)
  if(!is.character(fontface)) out("Argument 'fontface' must be of type 'character'.", type = 3)
  
  ## check lengths
  check <- list("labels" = labels, "x" = x, "y" = y, "colour" = colour, "size" = size, "fontface" = fontface)
  data <- sapply(1:length(check), function(i){
    if(length(check[[i]]) == 1) v <- rep(check[[i]], length(frames)) else v <- check[[i]]
    if(length(v) != length(frames)) out(paste0("Length of argument ", names(check)[[i]], " must either be 1 or equal to the length of argument 'frames'."), type = 3)
    return(v)
  }, simplify = F)
  
  data.classes <- sapply(data, class)
  data <- as.data.frame(do.call(cbind, data), stringsAsFactors = F)
  for(i in 1:ncol(data)) class(data[,i]) <- data.classes[i]
  
  data <- split(data, seq(nrow(data)))
  
  add_gg(frames, gg = expr(annotate(type, x = data[[2]], y = data[[3]], label = data[[1]], 
                                    colour = data[[4]], size = data[[5]], fontface = data[[6]])), 
         data = data, type = type)
}






## updated scalebar funcion to change text 
add_scalebar_custom <- function(frames, distance = NULL, height = 0.015, position = "bottomleft", x = NULL, y = NULL, 
                         colour = "black", label_margin = 1.2, units = "km", text_size = 3, fontface = "plain", verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(moveVis.verbose = verbose)
  if(!inherits(frames, "moveVis")) out("Argument 'frames' needs to be of class 'moveVis'. See frames_spatial()).", type = 3)
  if(!is.character(position)) out("Argument 'position' needs to be of type 'character'.", type = 3)
  if(isFALSE(units == "km" | units == "miles")) out("Argument 'units' must either be 'km' or 'miles'.", type = 3)
  if(!is.numeric(text_size)) out("Argument 'text_size' must be of type 'numeric'.", type = 3)
  if(!is.character(fontface)) out("Argument 'fontface' must be of type 'character'.", type = 3)
  
  check.args <- list(distance = distance, x = x, y = y)
  catch <- lapply(seq(1, length(check.args)), function(i) if(!any(is.numeric(check.args[[i]]), is.null(check.args[[i]]))) out(paste0("Argument '", names(check.args)[[i]], "' needs to be of type 'numeric'."), type = 3))
  
  ## calculate gg plot dimensions
  gg.crs <- frames[[1]]$coordinates$crs
  gg.xy <- ggplot_build(frames[[1]])$data[[1]]
  
  .corner <- function(xy){
    list(bottomleft = c(min(xy$xmin), min(xy$ymin)), upperleft = c(min(xy$xmin), max(xy$ymax)),
         upperright = c(max(xy$xmax), max(xy$ymax)), bottomright = c(max(xy$xmax), min(xy$ymin)))
  }
  gg.corner <- .corner(gg.xy)
  
  # cross_dateline
  if(is.null(gg.crs)){
    gg.crs <- st_crs(4326)
    gg.xy_cdl <- gg.xy
    gg.xy_cdl$xmin[gg.xy_cdl$xmin < -180] <- -180
    gg.xy_cdl$xmax[gg.xy_cdl$xmax < -180] <- -180
    gg.xy_cdl$xmin[gg.xy_cdl$xmin > 180] <- 180
    gg.xy_cdl$xmax[gg.xy$xmax > 180] <- 180
    gg.corner_sf <- lapply(.corner(gg.xy_cdl), function(x) st_sfc(st_point(x), crs = gg.crs))
  } else {
    gg.corner_sf <- lapply(gg.corner, function(x) st_sfc(st_point(x), crs = gg.crs))
  }
  gg.dist <- list(x = as.numeric(suppressPackageStartupMessages(st_distance(gg.corner_sf$bottomleft, gg.corner_sf$bottomright, by_element = T)))/1000,
                  y = as.numeric(suppressPackageStartupMessages(st_distance(gg.corner_sf$bottomleft, gg.corner_sf$upperleft, by_element = T)))/1000)
  
  ## calculate axis distances
  if(units == "miles") gg.dist <- lapply(gg.dist, function(x) x/1.609344 )
  gg.diff <- list(x = max(gg.xy$xmax) - min(gg.xy$xmin), y = max(gg.xy$ymax) - min(gg.xy$ymin))
  
  ## calculate scale distance
  if(!is.null(distance)){scale.dist <- distance}else{
    scale.dist <- digits <- 0
    while(scale.dist == 0){
      scale.dist <- round((gg.dist$x*0.2), digits = digits)
      digits <- digits+1
    }
  }
  
  # round to even
  if(scale.dist > 10) scale.dist <- round(scale.dist/2)*2
  
  scale.diff <- gg.diff$x*((scale.dist)/gg.dist$x)
  
  ## calculate scale position
  gg.margin <- list(bottomleft = unlist(gg.diff)*0.1,
                    upperleft = c(x = gg.diff$x*0.1, y = gg.diff$y*-(0.1+height)),
                    upperright = c(x = (gg.diff$x*-0.1)-scale.diff, y = gg.diff$y*-(0.1+height)),
                    bottomright = c(x = (gg.diff$x*-0.1)-scale.diff, y = gg.diff$y*0.1))
  
  scale.outer <- if(all(!is.null(x), !is.null(y))) c(x, y) else gg.corner[[position]] + gg.margin[[position]]
  scale.outer <- rbind.data.frame(scale.outer, c(scale.outer[1], (scale.outer[2] + (gg.diff$y*height))), 
                                  c(scale.outer[1]+scale.diff, (scale.outer[2] + (gg.diff$y*height))), c(scale.outer[1]+scale.diff, scale.outer[2]))
  colnames(scale.outer) <- c("x", "y")
  
  ## calculate inner scale position
  scale.inner <- scale.outer
  scale.inner[1:2,1] <- scale.inner[1:2,1] + (scale.diff/2)
  
  ## calculate annotation position
  text.margin <- (max(scale.outer$y) - min(scale.outer$y))*label_margin
  text.data <- cbind.data.frame(x = c(min(scale.outer$x), min(scale.inner$x), max(scale.outer$x)),
                                y = (min(scale.outer$y)-text.margin),
                                label = paste0(c(0, scale.dist/2, scale.dist), " ", units),
                                col = colour, text_size = text_size, fontface = fontface, stringsAsFactors = F)
  
  add_gg(frames, gg = expr(list(geom_polygon(aes_string(x = "x", y = "y"), data = scale.outer, fill = "white", colour = "black"), 
                                geom_polygon(aes_string(x = "x", y = "y"), data = scale.inner, fill = "black", colour = "black"),
                                geom_text(aes_string(x = "x", y = "y", label = "label", color = "col", size = "text_size", fontface = "fontface"), 
                                          data = text.data, size = text.data$text_size, fontface = text.data$fontface, colour = text.data$col))),
         scale.outer = scale.outer, scale.inner = scale.inner, text.data = text.data)
}
