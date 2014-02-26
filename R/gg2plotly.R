#' Convert R pch point codes to plotly "symbol" codes.
pch2symbol <- c("0"="square",
                "1"="circle",
                "2"="triangle-up",
                "3"="cross",
                "4"="x",
                "5"="diamond",
                "6"="triangle-down",
                "15"="square",
                "16"="circle",
                "17"="triangle-up",
                "18"="diamond",
                "19"="circle",
                "20"="circle",
                "22"="square",
                "23"="diamond",
                "24"="triangle-up",
                "25"="triangle-down",
                "o"="circle",
                "O"="circle",
                "+"="cross")

#' Convert ggplot2 aes to plotly "marker" codes.
aes2marker <- c(alpha="opacity",
                pch="symbol",
                size="size",
                colour="color",
                TODO="line", ## line color, size, and dash
                linetype="dash",
                shape="symbol")

#' Convert R lty line type codes to plotly "dash" codes.
lty2dash <- c("solid",
              "dot",
              "dash",
              "longdash",
              "dashdot",
              "longdashdot")

#' Convert a ggplot to a list.
#' @param p ggplot2 plot.
#' @return list representing a ggplot.
#' @export
gg2list <- function(p){
  plist <- list()
  plistextra <- ggplot2::ggplot_build(p)
  for(sc in plistextra$plot$scales$scales){
    if(sc$scale_name == "manual"){
      plist$scales[[sc$aesthetics]] <- sc$palette(0)
    }else if(sc$scale_name == "brewer"){
      plist$scales[[sc$aesthetics]] <- sc$palette(length(sc$range$range))
    }else if(sc$scale_name == "hue"){
      plist$scales[[sc$aesthetics]] <- sc$palette(length(sc$range$range))
    }else if(sc$scale_name == "linetype_d"){
      plist$scales[[sc$aesthetics]] <- sc$palette(length(sc$range$range))
    }else if(sc$scale_name == "alpha_c"){
      plist$scales[[sc$aesthetics]] <- sc$palette(sc$range$range)
    }else if(sc$scale_name == "size_c"){
      plist$scales[[sc$aesthetics]] <- sc$palette(sc$range$range)
    }else if(sc$scale_name == "gradient"){
      plist$scales[[sc$aesthetics]] <- ggplot2:::scale_map(sc, ggplot2:::scale_breaks(sc))
    }
  }
  for(i in seq_along(plistextra$plot$layers)){
    ## This is the layer from the original ggplot object.
    L <- plistextra$plot$layers[[i]]

    ## for each layer, there is a correpsonding data.frame which
    ## evaluates the aesthetic mapping.
    df <- plistextra$data[[i]]

    ## This extracts essential info for this geom/layer.
    g <- layer2list(L, df, plistextra$panel$ranges[[1]])
    
    ## Idea: use the ggplot2:::coord_transform(coords, data, scales)
    ## function to handle cases like coord_flip. scales is a list of
    ## 12, coords is a list(limits=list(x=NULL,y=NULL)) with class
    ## e.g. c("cartesian","coord"). The result is a transformed data
    ## frame where all the data values are between 0 and 1.
    
    ## TODO: coord_transform maybe won't work for 
    ## geom_dotplot|rect|segment and polar/log transformations, which
    ## could result in something nonlinear. For the time being it is
    ## best to just ignore this, but you can look at the source of
    ## e.g. geom-rect.r in ggplot2 to see how they deal with this by
    ## doing a piecewise linear interpolation of the shape.

    g$data <- ggplot2:::coord_transform(plistextra$plot$coord, g$data,
                                        plistextra$panel$ranges[[1]])
    plist$geoms[[i]] <- g
  }
  # Export axis specification as a combination of breaks and
  # labels, on the relevant axis scale (i.e. so that it can
  # be passed into d3 on the x axis scale instead of on the 
  # grid 0-1 scale). This allows transformations to be used 
  # out of the box, with no additional d3 coding. 
  theme.pars <- ggplot2:::plot_theme(p)  
  
  ## Flip labels if coords are flipped - transform does not take care
  ## of this. Do this BEFORE checking if it is blank or not, so that
  ## individual axes can be hidden appropriately, e.g. #1.
  ranges <- plistextra$panel$ranges[[1]]
  if("flip"%in%attr(plistextra$plot$coordinates, "class")){
    temp <- plistextra$plot$labels$x
    plistextra$plot$labels$x <- plistextra$plot$labels$y
    plistextra$plot$labels$y <- temp
  }
  is.blank <- function(el.name){
    x <- ggplot2::calc_element(el.name, p$theme)
    "element_blank"%in%attr(x,"class")
  }
  plist$axis <- list()
  for(xy in c("x","y")){
    s <- function(tmp)sprintf(tmp, xy)
    plist$axis[[xy]] <- ranges[[s("%s.major")]]
    plist$axis[[s("%slab")]] <- if(is.blank(s("axis.text.%s"))){
      NULL
    }else{
      ranges[[s("%s.labels")]]
    }
    plist$axis[[s("%srange")]] <- ranges[[s("%s.range")]]
    plist$axis[[s("%sname")]] <- if(is.blank(s("axis.title.%s"))){
      ""
    }else{
      plistextra$plot$labels[[xy]]
    }
    plist$axis[[s("%sline")]] <- !is.blank(s("axis.line.%s"))
    plist$axis[[s("%sticks")]] <- !is.blank(s("axis.ticks.%s"))
  }
  
  plist$legend <- getLegendList(plistextra)
  if(length(plist$legend)>0){
    plist$legend <- plist$legend[which(sapply(plist$legend, function(i) length(i)>0))]
  }  # only pass out legends that have guide = "legend" or guide="colorbar"
  
  # Remove legend if theme has no legend position
  if(theme.pars$legend.position=="none") plist$legend <- NULL
  
  if("element_blank"%in%attr(theme.pars$plot.title, "class")){
    plist$title <- ""
  } else {
    plist$title <- plistextra$plot$labels$title
  }
  
  plist
}

#' Convert a layer to a list. Called from gg2list()
#' @param l one layer of the ggplot object
#' @param d one layer of calculated data from ggplot2::ggplot_build(p)
#' @param ranges axes ranges
#' @return list representing a layer, with corresponding aesthetics, ranges, and groups.
#' @export
layer2list <- function(l, d, ranges){
  g <- list(geom=l$geom$objname,
            data=d)
  g$aes <- sapply(l$mapping, function(k) as.character(as.expression(k))) # needed for when group, etc. is an expression

  ## use un-named parameters so that they will not be exported
  ## to JSON as a named object, since that causes problems with
  ## e.g. colour.
  g$params <- l$geom_params
  for(p.name in names(g$params)){
    names(g$params[[p.name]]) <- NULL
  }

  ## Convert complex ggplot2 geoms so that they are treated as special
  ## cases of basic geoms. In ggplot2, this processing is done in the
  ## draw method of the geoms.

  ## Every plotly trace has one of these types
  ## type=scatter,bar,box,histogramx,histogram2d,heatmap

  ## for type=scatter, you can define
  ## mode=none,markers,lines,lines+markers where "lines" is the
  ## default for 20 or more points, "lines+markers" is the default for
  ## <20 points. "none" is useful mainly if fill is used to make area
  ## plots with no lines.

  ## marker=list(size,line,color="rgb(54,144,192)",opacity,symbol)

  ## symbol=circle,square,diamond,cross,x,
  ## triangle-up,triangle-down,triangle-left,triangle-right

  geom <- function(gname){
    g$geom == gname
  }
  g$geom <- if(geom("abline")){
    # "Trick" ggplot coord_transform into transforming the slope and intercept
    g$data[,"x"] <- ranges$x.range[1]
    g$data[,"xend"] <- ranges$x.range[2]
    g$data[,"y"] <- g$data$slope*ranges$x.range[1]+g$data$intercept
    g$data[,"yend"] <-  g$data$slope*ranges$x.range[2]+g$data$intercept
    g$data <- as.data.frame(g$data)
    if(g$aes[["group"]]=="1"){ 
      # ggplot2 defaults to adding a group attribute
      # which misleads for situations where there are 
      # multiple lines with the same group. 
      # if the group attribute conveys no additional 
      # information, remove it.
      ## TODO: Figure out a better way to handle this...
      g$aes <- g$aes[-which(names(g$aes)=="group")]
    } 
    "segment"
  } else if(geom("point")){
    # Fill set to match ggplot2 default of filled in circle. 
    if(!"fill"%in%names(g$data) & "colour"%in%names(g$data)){
      g$data[["fill"]] <- g$data[["colour"]]
    }
    "point"
  } else if(geom("ribbon")){
    # Color set to match ggplot2 default of fill with no outside border.
    if("fill"%in%names(g$data) & !"colour"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
    }
    "ribbon"
  } else if(geom("density") | geom("area")){
    "ribbon"
  } else if(geom("tile") | geom("raster") | geom("histogram") ){
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g$data) & "fill"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g$data)) g$data[["size"]] <- 0 
    }
    "rect"
  } else if(geom("bar")){
    "rect"
  } else if(g$geom=="bin2d"){
    stop("TODO")
  } else if(geom("boxplot")){
    stop("boxplots are not supported. Workaround: rects, lines, and points")
    ## TODO: boxplot support. But it is hard since boxplots are drawn
    ## using multiple geoms and it is not straightforward to deal with
    ## that using our current JS code. There is a straightforward
    ## workaround: combine working geoms (rects, lines, and points).

    g$data$outliers <- sapply(g$data$outliers, FUN=paste, collapse=" @ ") 
    # outliers are specified as a list... 
  } else if(geom("violin")){
    x <- g$data$x
    vw <- g$data$violinwidth
    xmin <- g$data$xmin
    xmax <- g$data$xmax
    g$data$xminv <- x-vw*(x-xmin)
    g$data$xmaxv <- x+vw*(xmax-x)
    newdata <- ddply(g$data, .(group), function(df){
      rbind(arrange(transform(df, x=xminv), y), arrange(transform(df, x=xmaxv), -y))
                })
    newdata <- ddply(newdata, .(group), function(df) rbind(df, df[1,]))
    g$data <- newdata
    "polygon"
  } else if(geom("step")){
    datanames <- names(g$data)
    g$data <- ddply(g$data, .(group), function(df) ggplot2:::stairstep(df))
    "path"
  } else if(geom("contour") | g$geom=="density2d"){
    g$aes[["group"]] <- "piece"
    "path"
  } else if(geom("freqpoly")){
    "line"
  } else if(geom("quantile")){
    "path"
  } else if(geom("hex")){
    ## TODO: for interactivity we will run into the same problems as
    ## we did with histograms. Again, if we put several
    ## clickSelects/showSelected values in the same hexbin, then
    ## clicking/hiding hexbins doesn't really make sense. Need to stop
    ## with an error if showSelected/clickSelects is used with hex.
    g$aes[["group"]] <- "group"
    dx <- ggplot2::resolution(g$data$x, FALSE)
    dy <- ggplot2::resolution(g$data$y, FALSE) / sqrt(3) / 2 * 1.15
    hex <- as.data.frame(hexcoords(dx, dy))[,1:2]
    hex <- rbind(hex, hex[1,]) # to join hexagon back to first point
    g$data$group <- as.numeric(interaction(g$data$group, 1:nrow(g$data)))
    ## this has the potential to be a bad assumption - 
    ##   by default, group is identically 1, if the user 
    ##   specifies group, polygons aren't possible to plot
    ##   using d3, because group will have a different meaning
    ##   than "one single polygon".
    newdata <- ddply(g$data, .(group), function(df){
      df$xcenter <- df$x
      df$ycenter <- df$y
      cbind(x=df$x+hex$x, y=df$y+hex$y, df[,-which(names(df)%in%c("x", "y"))])
    })
    g$data <- newdata
    # Color set to match ggplot2 default of tile with no outside border.
    if(!"colour"%in%names(g$data) & "fill"%in%names(g$data)){
      g$data[["colour"]] <- g$data[["fill"]]
      # Make outer border of 0 size if size isn't already specified.
      if(!"size"%in%names(g$data)) g$data[["size"]] <- 0 
    }
    "polygon"
  } else if(g$geom %in% c("polygon")) {
    ## all other geoms are basic, and keep the same name.
    g$geom
  } else {
    stop("unsupported geom ", g$geom)
  }

  ## For ggplot2 polygons, change convert groups to vectors with NA.
  if(geom("polygon")){
    poly.list <- split(g$data, g$data$group)
    poly.na.df <- data.frame()
    for(i in seq_along(poly.list)){
      poly.na.df <- rbind(poly.na.df, poly.list[[i]], NA)
    }
    g$data <- poly.na.df
  }

  ## Check g$data for color/fill - convert to hexadecimal so JS can
  ## parse correctly.
  for(color.var in c("colour", "color", "fill")){
    if(color.var %in% names(g$data)){
      g$data[,color.var] <- toRGB(g$data[,color.var])
    }
  }

  if(any(g$data$size == 0, na.rm=TRUE)){
    warning(sprintf("geom_%s with size=0 will be invisible",g$geom))
  }

  tr <- list()
  for(name in c("x", "y", "text")){
    if(name %in% names(g$data)){
      tr[[name]] <- g$data[[name]]
    }
  }
  ## Add plotly type/mode info based on geom type.
  if(geom("point")){
    tr$type <- "scatter"
    tr$mode <- "markers"
  }
  for(name in names(aes2marker)){
    plotly.name <- aes2marker[[name]]
    take.from <- if(name %in% g$params){
      g$params
    } else if(name %in% names(g$data)){
      g$data
    }
    tr$marker[[plotly.name]] <- take.from[[name]]
  }
  g$trace <- tr
  g
}

#' Get legend information.
#' @param plistextra output from ggplot2::ggplot_build(p)
#' @return list containing information for each legend
#' @export
getLegendList <- function(plistextra){
  plot <- plistextra$plot
  scales <- plot$scales
  layers <- plot$layers
  default_mapping <- plot$mapping
  theme <- ggplot2:::plot_theme(plot)
  position <- theme$legend.position
  # by default, guide boxes are vertically aligned
  theme$legend.box <- if(is.null(theme$legend.box)) "vertical" else theme$legend.box
  
  # size of key (also used for bar in colorbar guide)
  theme$legend.key.width <- if(is.null(theme$legend.key.width)) theme$legend.key.size
  theme$legend.key.height <- if(is.null(theme$legend.key.height)) theme$legend.key.size
  # by default, direction of each guide depends on the position of the guide.
  theme$legend.direction <- if(is.null(theme$legend.direction)){
    if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
      switch(position[1], top =, bottom = "horizontal", left =, right = "vertical")
    else
      "vertical"
  }
  # justification of legend boxes
  theme$legend.box.just <-
    if(is.null(theme$legend.box.just)) {
      if (length(position) == 1 && position %in% c("top", "bottom", "left", "right"))
        switch(position, bottom =, top = c("center", "top"), left =, right = c("left", "top"))
      else
        c("center", "center")
    } 
  
  position <- theme$legend.position
  guides <- plyr::defaults(plot$guides, guides(colour="legend", fill="legend"))
  labels <- plot$labels
  gdefs <- ggplot2:::guides_train(scales = scales, theme = theme, guides = guides, labels = labels)
  if (length(gdefs) != 0) {
    gdefs <- ggplot2:::guides_merge(gdefs)
    gdefs <- ggplot2:::guides_geom(gdefs, layers, default_mapping)
  } else (ggplot2:::zeroGrob())
  names(gdefs) <- sapply(gdefs, function(i) i$title)
  lapply(gdefs, getLegend)
}

#' Function to get legend information for each scale
#' @param mb single entry from ggplot2:::guides_merge() list of legend data
#' @return list of legend information, NULL if guide=FALSE.
getLegend <- function(mb){
  guidetype <- mb$name
  ## The main idea of legends:
  
  ## 1. Here in getLegend I export the legend entries as a list of
  ## rows that can be used in a data() bind in D3.

  ## 2. In add_legend in the JS code I create a <table> for every
  ## legend, and then I bind the legend entries to <tr>, <td>, and
  ## <svg> elements.
  geoms <- sapply(mb$geoms, function(i) i$geom$objname)
  cleanData <- function(data, key, geom, params){
    if(nrow(data)==0) return(data.frame()); # if no rows, return an empty df.
    if("guide"%in%names(params)){
      if(params[["guide"]]=="none") return(data.frame()); # if no guide, return an empty df
    } 
    data$order <- 1:nrow(data)
    data <- merge(data, key)
    data <- data[order(data$order),]
    if(!".label"%in%names(data)) return(data.frame()); # if there are no labels, return an empty df.
    if(nrow(data)==0) return(data.frame());
    data <- data[,which(colSums(!is.na(data))>0)] # remove cols that are entirely na
    if("colour"%in%names(data)) data[["colour"]] <- toRGB(data[["colour"]]) # color hex values
    if("fill"%in%names(data)) data[["fill"]] <- toRGB(data[["fill"]]) # fill hex values
    names(data) <- paste(geom, names(data), sep="") # aesthetics by geom
    names(data) <- gsub(paste(geom, ".", sep=""), "", names(data), fixed=TRUE) # label isn't geom-specific
    data
  }
  dataframes <- lapply(mb$geoms, function(i) cleanData(i$data, mb$key, i$geom$objname, i$params))
  dataframes <- dataframes[which(sapply(dataframes, nrow)>0)]
  # Check to make sure datframes is non-empty. If it is empty, return NULL.
  if(length(dataframes)>0) {
    data <- merge_recurse(dataframes)
  } else return(NULL)
  data <- lapply(nrow(data):1, function(i) as.list(data[i,]))
  if(guidetype=="none"){
    NULL
  } else{
    list(guide = guidetype, 
         geoms = geoms, 
         title = mb$title, 
         entries = data)
  }
}

#' Convert R colors to RGB hexadecimal color values
#' @param x character
#' @return hexadecimal color value (if is.na(x), return "none" for compatibility with JavaScript)
#' @export
toRGB <- function(x){
  rgb.matrix <- col2rgb(x)
  rgb.text <- apply(rgb.matrix, 2, paste, collapse=",")
  rgb.css <- sprintf("rgb(%s)", rgb.text)
  ifelse(is.na(x), "none", rgb.css)
}

#' Function to merge a list of data frames (from the reshape package)
#' @param dfs list of data frames
#' @param ... other arguments to merge
#' @return data frame of merged lists
merge_recurse = function (dfs, ...) 
{
  if (length(dfs) == 1) {
    dfs[[1]]
  }
  else if (length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all.x = TRUE, sort = FALSE, ...)
  }
  else {
    merge(dfs[[1]], Recall(dfs[-1]), all.x = TRUE, sort = FALSE, 
          ...)
  }
}

