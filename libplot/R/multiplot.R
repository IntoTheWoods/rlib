multiplot <- function(ggplots=list(), ncol=1, size="first") {
  nrow = ceiling(length(ggplots)/ncol)
  num_plots = ncol*nrow
  if (length(ggplots) < num_plots) {
    for (i in (length(ggplots)+1):num_plots) {
      ggplots[[i]] = ggplot()+theme_classic()
    }
  }
  grobs = lapply(ggplots, ggplotGrob)
  
  for (i in 1:nrow) {
    plot_row =  do.call(cbind, c(grobs[(ncol*i-ncol+1):(ncol*i)], size=size))
    if (exists("plot_table")) {
      plot_table = do.call(rbind, c(list(plot_table, plot_row), size=size))
    } else {
      plot_table = plot_row
    }
  }

  grid.newpage()
  grid.draw(plot_table)
}

