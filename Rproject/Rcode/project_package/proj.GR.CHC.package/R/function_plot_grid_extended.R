#' plot_grid_extended
#'
#' Allows arranging a list of plots with differing/partially non existing legends into
#' a single plot.
#'
#' @details Extends upon the plot_grid function in the cowplot package. It will
#' create one plot grid for the plots and one for the legends and then stick those together
#'
#' @param plotlist list of plots to arrange. By default, align="hv"
#' @param relative_heights relative heights of the plots (rel_heights argument in plot_grid).
#' The length of this vector is equal to the number of plots in the grid
#' @param relative_widths relative widths of plots vs legends. The length of this vector
#' is always 2.
#'
#' @return an arranged plot
#' @export
#'
#' @examples plot_grid_extended(list(p1,p2,p3),c(1,1,0.5),c(1,0.3))
plot_grid_extended=function(plotlist,relative_heights=rep(1,length(plotlist)),relative_widths=c(1,0.5)){

  #a helping function that slightly adjusts get_legend so that it doesnt break
  #when no legend exists.
  get_legend_if_exists= function (plot)
  {
    grobs <- plot_to_gtable(plot)$grobs
    legendIndex <- which(sapply(grobs, function(x) x$name) ==
                           "guide-box")
    if (length(legendIndex) == 1) {
      legend <- grobs[[legendIndex]]
    }
    else {#return an empty object if no legend
      legend=ggplot()
    }
  }

  #decomposing ggplots into plots and legends
  plots=list()
  legends=list()
  for (i in 1:length(plotlist)){
    plots[[i]]=plotlist[[i]]+theme(legend.position="none")
    legends[[i]]=get_legend_if_exists(plotlist[[i]])
  }

  plots_gridded=plot_grid(plotlist=plots,align="hv",ncol=1,rel_heights=relative_heights)
  legends_gridded=plot_grid(plotlist=legends,ncol=1,rel_heights=relative_heights)

  outplot=plot_grid(plots_gridded,legends_gridded,rel_widths=relative_widths)
  return(outplot)
}
