qc_plot = function(meta) {

##############################################################################

## FovRatio

meta$FovRatio_color = ifelse(meta$FovRatio > 0.85, "#00a3cc","#cc0000")

plot.FovRatio = ggplot(meta, aes(x = sample_name, y = FovRatio, fill = FovRatio_color)) + 
  geom_bar(stat="identity", width=0.5) + coord_flip() +
  scale_fill_manual(values = meta$FovRatio_color, guide=F) +
  #labels=c("Pass","Fail"),
  #guide = guide_legend(reverse=TRUE, title="Field of View")) +
  geom_hline(yintercept=0.85) +
  ggtitle("Field of View") +
  theme(#panel.background = element_blank(),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=16), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size=16),
        axis.title.y = element_blank())

#print(plot.FovRatio)

## BindingDensity

meta$BindingDensity_color = case_when(
  meta$BindingDensity > 2.2 ~ "#cc0000",
  meta$BindingDensity > 1.8 & meta$BindingDensity <= 2.2 ~"#e6e600",
  meta$BindingDensity <= 1.8 ~ "#00a3cc",
  TRUE ~ "#000000"
)

plot.BindingDensity = ggplot(meta, aes(x = sample_name, y = BindingDensity, fill = BindingDensity_color)) + 
  geom_bar(stat="identity", width=0.5) + coord_flip() +
  scale_fill_manual(values = meta$BindingDensity_color, guide=F) +
  #labels=c("Pass","Fail"),
  #guide = guide_legend(reverse=TRUE, title="Field of View")) +
  geom_hline(yintercept=2.2) +
  geom_hline(yintercept=1.8) +
  ggtitle("Binding Density") +
  theme(#panel.background = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size=16),
        axis.title.y = element_blank())

#print(plot.BindingDensity)

## pos.norm.factor

meta$pos.norm.factor_color = case_when(
  meta$pos.norm.factor > 3 | meta$pos.norm.factor < 0.3 ~ "#cc0000",
  TRUE ~ "#00a3cc"
)

plot.pos.norm.factor = ggplot(meta, aes(x = sample_name, y = pos.norm.factor, fill=pos.norm.factor_color)) + 
  geom_bar(stat="identity", width=0.5) + 
  coord_flip(ylim = c(0,5)) +
  scale_fill_manual(values = levels(factor(meta$pos.norm.factor_color)), guide=F) +
  #labels=c("Pass","Fail"),
  #guide = guide_legend(reverse=TRUE, title="Positive Control\nNormalization")) +
  geom_hline(yintercept=0.3) +
  geom_hline(yintercept=3) +
  ggtitle("Positive Control Normalization") +
  theme(#panel.background = element_blank(), 
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size=16),
        axis.title.y = element_blank())

#print(plot.pos.norm.factor)



## MaxNegative

meta$MaxNegative_color = case_when(
  meta$MaxNegative > 8 ~ "#cc0000",
  meta$MaxNegative > 6.5 & meta$MaxNegative <=8 ~ "#e6e600",
  TRUE ~ "#00a3cc"
)

plot.MaxNegative = ggplot(meta, aes(x = sample_name, y = MaxNegative, fill=MaxNegative_color)) + 
  geom_bar(stat="identity", width=0.5) + 
  coord_flip(ylim = c(0,15)) +
  scale_fill_manual(values = levels(factor(meta$MaxNegative_color)), guide=F) +
  #labels=c("Pass","Fail","Borderline"),
  #guide = guide_legend(reverse=TRUE, title="Positive Control\nNormalization")) +
  geom_hline(yintercept=6) +
  geom_hline(yintercept=8) +
  ggtitle("Max Negative Control") +
  theme(#panel.background = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size=16),
        axis.title.y = element_blank())

#print(plot.MaxNegative)

## rna.content

meta$rna.content_color = case_when(
  meta$rna.content < 50 ~ "#cc0000",
  meta$rna.content < 250 ~ "#e6e600",
  meta$rna.content < 500 ~ "#0033cc",
  TRUE ~ "#00a3cc"
)

plot.rna.content = ggplot(meta, aes(x = sample_name, y = log10(rna.content), fill=rna.content_color)) + 
  geom_bar(stat="identity", width=0.5) + 
  #  coord_flip(ylim = c(0,300)) +
  coord_flip() +
  scale_fill_manual(values = levels(factor(meta$rna.content_color)), guide=F) +
  #labels=c("Medium","High"),
  #guide = guide_legend(reverse=TRUE, title="Positive Control\nNormalization")) +
  geom_hline(yintercept=1.69897) +
  geom_hline(yintercept=2) +
  geom_hline(yintercept=2.39794) +
  geom_hline(yintercept=2.69897) +
  ggtitle("Estimated RNA Amount") +
  theme(#panel.background = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5, size=16),
        axis.title.y = element_blank())

#print(plot.rna.content)


#multiplot(ggplots = list(plot.FovRatio, plot.BindingDensity, plot.pos.norm.factor, plot.MaxNegative, plot.rna.content), ncol = 5, size="last")
multiplot(ggplots = list(plot.FovRatio, plot.BindingDensity, plot.MaxNegative, plot.rna.content), ncol = 4, size="last")


}



