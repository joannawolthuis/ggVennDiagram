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

  data_merged$group <- stringr::str_replace_all(string = data_merged$group,  c(A = paste0(category$label[1],"\n"),
                                                                               B = paste0(category$label[2],"\n"),
                                                                               C = paste0(category$label[3],"\n"),
                                                                               D = category$label[4]))
  data_merged$group <- gsub("\n$", "", data_merged$group)

  myCols <- gbl$functions$color.functions[[lcl$aes$spectrum]](n = nrow(pievec))

  p <- ggplot() + aes_string("x","y") +
    geom_text(aes(label = label),
              data = category,
              fontface = "bold",
              color = ggdark::invert_color(myCols),
              hjust = "inward",
              vjust = "inward") +
    geom_polygon(aes(key = (group),
                     text = (group),
                     fill = count),
                 data = data_merged, ...) +
    coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none",
                   text=ggplot2::element_text(#size=font$ax.num.size,
                     family = font$family),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::scale_fill_gradientn(colours = cf(256)) +
    ggplot2::coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE)

  if (is.null(label)){
    return(p)
  }
  else{
    counts <- counts %>%
      mutate(percent=paste(round(.data$count*100 / sum(.data$count),digits = 2),"%",sep="")) %>%
      mutate(label = paste(.data$count,"\n","(",.data$percent,")",sep=""))
    data <- merge(counts,center)
    if (label == "count"){
      p + geom_text(aes(label=count), data = data, alpha = label_alpha, color = label_color)
    }
    else if (label == "percent"){
      p + geom_text(aes_string(label="percent"),data = data, alpha = label_alpha, color = label_color)
    }
    else if (label == "both"){
      p + geom_text(aes_string(label="label"),data = data,alpha = label_alpha,color = label_color)
    }
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
