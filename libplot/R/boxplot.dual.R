boxplot.dual = function(data=data, x="Tissue", y="FPKM", logy="logFPKM", fill="Tissue") {
  plot.linear = ggplot(data, aes_string(x=x, y=y, fill=fill)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(-5,128))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1)) +
  theme(axis.text.y = element_text(size = 14)) +
  guides(fill=F) +
  theme(plot.margin = unit(c(0,1,1,3), "cm"))
# geom_point(position = position_jitterdodge()) +

plot.log = ggplot(data, aes_string(x=x, y=logy, fill=fill)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(7,15))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x = element_blank(), axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.text.y = element_text(size = 14)) +
  guides(fill=F) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
# geom_point(position = position_jitterdodge()) +

#multiplot(plot.log, plot.linear,cols=1)
  
grid.newpage()
g = do.call(rbind, c(lapply(list(plot.log, plot.linear), ggplotGrob), size="last"))
g$heights[g$layout$t[grep("panel", g$layout$name)]] <- unit(c(1,2), "null")
grid.draw(g)


}


