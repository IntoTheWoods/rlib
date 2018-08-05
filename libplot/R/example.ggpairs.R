example.ggpairs = function() {
require(datasets)
data("swiss")
require(GGally)
require(ggplot2)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(swiss,columns = 1:4, lower = list(continuous = my_fn))
g



require(GGally)
data(tips, package="reshape")

ggpairs(data=tips, # data.frame with variables
        columns=1:3, # columns to plot, default to all.
        title="tips data", # title of the plot
        colour = "sex") # aesthetics, ggplot2 style
}
