## takes a hclust object as input


ggdend <- function(df) {
  
  df = dendro_data(as.dendrogram(hc))$segments
  
  
  ggplot() +
    geom_segment(data = df, aes(x=x, y=y, xend=xend, yend=yend)) +
    labs(x = "", y = "") + theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  
}


