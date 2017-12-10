
#' @title floodmap check
#' @name floodmap
#' @aliases floodmap-package
#' @docType package
#' @keywords floodmap dbf check
#' @useDynLib floodmap
#' @importFrom Rcpp sourceCpp
#' @importFrom readr read_lines
#' @import stringr
#' @import ggplot2 magrittr data.table
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("floodmap", libpath)
}

#' @title initial floodmap package,
#' @description install packages dependency
#' @examples
#' init.install()
#' @export
init.install <- function(){
  pkgs <- c("readr", "openxlsx", "readxl", "ggplot2", "plyr", "data.table",
            "stringr", "magrittr", "maptools", "rgeos", "snow")
  for (pkg in pkgs){
    if (!require(pkg, character.only = TRUE)){
      install.packages(pkg, repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
    }
  }
  cat(sprintf("All packages needed have been existed.\n"))
}

#' @title dir.sort
#' @description modified original dir function. only used fo floodmap Project
#' @export
dir.sort <- function(indir, pattern = "*.dbf$", full.names = T, sort = TRUE, ...){
  fnames <- dir(indir, pattern = pattern, full.names = full.names, ...)
  #reorder fnames according scheme number
  if (sort) fnames <- fnames[order(as.numeric(str_extract(basename(fnames), "\\d{1,2}")))]
  fnames#QUICKLY RETURN
}
