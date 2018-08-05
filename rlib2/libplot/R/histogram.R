histogram = function(data) {
  if (class(data)=="matrix") {
    data=as.vector(data)
  }

  ggplot(NULL, aes(x=data)) + geom_histogram()
}
