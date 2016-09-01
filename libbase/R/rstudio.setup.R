packages = c("devtools")

for (package in packages) {
  install.packages(package)
}

devtools::install_github("klutometis/roxygen")

