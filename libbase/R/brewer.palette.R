brewer.palette = function(n,set) {
      palette = RColorBrewer::brewer.pal(n, set)
      if (n == 1 | n == 2) {
        palette = palette[1:n] 
      }

      palette=rep(palette,ceiling(n/length(palette)))[1:n]

      palette
}
