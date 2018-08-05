reload.package = function(package_name) {
  package_name = as.character(substitute(package_name))
  package_name = paste0("/data/opt/rlib/",package_name)
  install.packages(package_name, type="src", repos=NULL)
  devtools::reload(package_name)
}

