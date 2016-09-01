packages = c("devtools","reshape2","ggplot2","RSQLite")

for (package in packages) {
  install.packages(package)
}

devtools::install_github("klutometis/roxygen")

