pca.plot <- function(x, groups) {
  n <- ncol(x) 
  if(!(n %in% c(2,3))) { # check if 2d or 3d
    stop("x must have either 2 or 3 columns")
  }
  
  #fit <- hclust(dist(x), method="complete") # cluster
  #groups <- cutree(fit, k=nGroup)
  
  if(n == 3) { # 3d plot
    rgl::plot3d(x, col=groups, type="s", size=1, axes=F)
    rgl::axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
    rgl::grid3d("x")
    rgl::grid3d("y")
    rgl::grid3d("z")
  } else { # 2d plot
    maxes <- apply(abs(x), 2, max)
    rangeX <- c(-maxes[1], maxes[1])
    rangeY <- c(-maxes[2], maxes[2])
    plot(x, col=groups, pch=19, xlab=colnames(x)[1], ylab=colnames(x)[2], xlim=rangeX, ylim=rangeY)
    lines(c(0,0), rangeX*2)
    lines(rangeY*2, c(0,0))
  }
}

