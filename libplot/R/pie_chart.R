library(scales)
my.colors = c(rev(RColorBrewer::brewer.pal(9,"YlGnBu")),RColorBrewer::brewer.pal(9,"YlOrRd")[2:9])

df <- data.frame(
  group = c("Surgery + Platinum-based Chemo",
            "Platinum-based Chemo",
            "Supportive Care",
            "Surgery Only",
            "Clinical Trial",
            "Other"),
  value = c(47,33,8,5,7,1)
)

df <- data.frame(
  group = c("Chemo (+/- Radiotherapy",
            "Surgery and Chemo",
            "Supportive Care",
            "Surgery + Radiotherapy",
            "Surgery",
            "Radiotherapy"),
  value = c(49,31,7,6,5,3)
)

df <- data.frame(
  group = c(
    "Colorectal",
    "Pancreatic",
    "NSCLC",
    "Gastric",
    "Ovarian",
    "Renal cell carcinoma",
    "Sarcoma",
    "Cholangiocarcinoma",
    'Head and neck',
    "Melanoma",
    "Mesothelioma"
  ),
  value = c(8,6,5,3,2,2,2,1,1,1,1)
)


my.colors = my.colors[1:length(df$value)]

df$group=factor(df$group,levels=df$group)

head(df)

library(ggplot2)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

value=df$value
text_position = sum(value)-(value/2 + c(0, cumsum(value)[-length(value)]))
text_position

text_label = ifelse(value > 5,percent(value/100),"")
text_label = ifelse(value > 2,value,"")
#text_label = value

pie = ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=my.colors) + 
  blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = text_position, label = text_label),
            size=5, x=1.2)
pie
