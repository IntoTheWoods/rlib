factor2numeric = function(x) {
  x = as.factor(x)
  levels(x) = 1:length(levels(x))
  as.numeric(x)
}
