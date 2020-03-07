#' ggVennDiagram
#'
#' @param x list of items
#' @param n.sides set how many points been generated for one ellipse, the more points, the better resolution.
#' @param label select one from c("count","percent","both")
#' @param label_alpha set 0 to remove label background
#' @param category.names default is names(x)
#' @param ... Other arguments passed on to the polygon layer.
#' @param lty line type of polygons
#' @param color line color of polygons
#'
#' @return A ggplot object
#' @export
#' @examples
#' x <- list(A=1:5,B=2:7,C=3:6,D=4:9)
#' ggVennDiagram(x)  # 4d venn
#' ggVennDiagram(x[1:3])  # 3d venn
#' ggVennDiagram(x[1:2])  # 2d venn
ggVennDiagram <- function(x,
                          category.names=names(x),
                          n.sides=3000,
                          label="both",
                          label_alpha=0.5,
                          lty=1,
                          color="grey",...){
  dimension <- length(x)

  suppressWarnings(
    if (dimension == 4){
      draw_4d_venn(x, n.sides=n.sides,category.names=category.names,label = label, label_alpha=label_alpha, lty=lty,color=color,...)
    }
    else if (dimension == 3){
      draw_3d_venn(x, n.sides=n.sides,category.names=category.names,label = label, label_alpha=label_alpha,lty=lty,color=color,...)
    }
    else if (dimension == 2){
      draw_2d_venn(x, n.sides=n.sides,category.names=category.names,label = label, label_alpha=label_alpha,lty=lty,color=color,...)
    }
    else{
      stop("Only support 2-4 dimension venn diagram.")
    }
  )
}

#' get a list of items in Venn regions
#' @inheritParams ggVennDiagram
#'
#' @return a list of region items
#' @export
get_region_items <- function(x, category.names=names(x)){
  if (!identical(category.names,names(x))){
    message("This function returns a list named by alphabet letters")
    message("The mapping between letters and categories is as follows:")
    message(paste(paste(names(x),category.names,sep = ": "), collapse = "\n"))
  }

  dimension <- length(x)
  if (dimension == 4){
    four_dimension_region_items(x)
  }
  else if (dimension == 3){
    three_dimension_region_items(x)
  }
  else if (dimension == 2){
    two_dimension_region_items(x)
  }
  else{
    stop("Only support 2-4 dimension venn diagram.")
  }
}

map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

#' plot codes
#'
#' @param region_data a list of two dataframes, which were used to plot polygon and label latter.
#' @param category name of Set
#' @param counts counts of items for every combinations
#' @inheritParams ggVennDiagram
#'
#' @import ggplot2
#'
#' @return ggplot object
plot_venn <- function(region_data, category,
                      counts, label,
                      label_alpha, label_color,
                      font, cf, ...){

  polygon <- region_data[[1]]
  center <- region_data[[2]]

  data_merged <- merge(polygon,
                      counts)

  category$label <- as.character(category$label)

  data_merged$group_new <- stringr::str_replace_all(string = data_merged$group,
                                                    c(A = paste0(category$label[1],"\n"),
                                                      B = paste0(category$label[2],"\n"),
                                                      C = paste0(category$label[3],"\n"),
                                                      D = category$label[4]))

  #category$label = gsub('(.{1,20})', '\\1\n', category$label)

  data_merged$group_new <- gsub("\n$", "", data_merged$group_new)

  #myCols <- gbl$functions$color.functions[[lcl$aes$spectrum]](n = nrow(pievec))
  # library(ggVennDiagram)
  #genes <- paste("gene",1:1000,sep="")
  #set.seed(20190708)
  #x <- list(A=sample(genes,300),B=sample(genes,525),C=sample(genes,440),D=sample(genes,350))

  data_merged$color <- as.character(map2color(data_merged$count,
                                              cf(max(data_merged$count))))

  poly2col <- unique(data_merged[,c("group", "color")])

  p <- ggplot() + aes_string("x","y") +
    geom_text(aes(label = label),
              data = category,
              size=6,
              fontface = "bold",
              color = "black",
              hjust = "inward",
              vjust = "inward") +
    geom_polygon(aes(key = (group_new),
                     text = (group_new),
                     fill = (color)),
                 data = data_merged,
                 ...) +
    coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none",
                   text=ggplot2::element_text(
                     family = font$family),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) +
    scale_fill_identity()

  if (is.null(label)){
    return(p)
  }
  else{
    counts <- counts %>%
      mutate(percent=paste(round(.data$count*100 / sum(.data$count),digits = 2),"%",sep="")) %>%
      mutate(label = paste(.data$count,"\n","(",.data$percent,")",sep=""))
    data <- merge(counts,center)
    data <- merge(data, poly2col)
    data$color <- ggdark::invert_color(data$color)

    if (label == "count"){
      p <- p + geom_text(aes_string(label = "count",
                                    color = "color"),
                         data = data,
                         alpha = label_alpha)
    } else if (label == "percent"){
      p <- p + geom_text(aes_string(label="percent",
                                    color = "color"),
                         data = data,
                         alpha = label_alpha)
    }
    else if (label == "both"){
      p <- p + geom_text(aes_string(label="label",
                                    color="color"),
                         data = data,
                         size=5,
                         alpha = label_alpha)
    }
    p + scale_color_identity()

  }
}

#' generating a circle
#'
#' @param x,y center of circle
#' @param r radius of circle
#' @param n points (resolution)
#'
#' @return a data.frame representing circle position
circle <- function(x,y,r,n=1000){
  angles <- seq(0,2*pi,length.out = n)
  xv <- cos(angles) * r + x
  yv <- sin(angles) * r + y
  xv <- round(xv,6)
  yv <- round(yv,6)
  data.frame(x=xv,y=yv)
}
