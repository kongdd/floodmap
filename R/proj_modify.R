proj <- 'GEOGCS["CGCS_2000",DATUM["D_2000",SPHEROID["S_2000",6378137.0,298.2572221010041]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
#' @title prj.modify
#' @description modify projection into CGCS_2000
#' @param indir the six part directory
#' @export
prj.modify <- function(indir){
  fnames <- dir(indir, pattern = "*.prj$", full.names = T, recursive = T)
  # fileNames <- gsub(".shp$", "", files)
  for (i in seq_along(fnames)){
    write.table(proj, file = fnames[i], col.names = F, row.names = F, quote = F)
  }
}

#' @title prj.check
#' @description check projection is or not CGCS_2000
#' @param indir the six part directory
#' @importFrom readr read_table
#' @examples
#' prj.check(indir)
#' @export
prj.check <- function(indir){
  fnames <- dir(indir, pattern = "*.prj$", full.names = T, recursive = T)
  prjs <- lapply(fnames,
                 function(fname) str_split(read_table(fname, col_names = F, progress = F)[[1]], '"')[[1]][2]) %>% unlist()
  data.frame(filename = basename(fnames), prj = prjs)#quickly return
  # Id <- which(prjs != "CGCS_2000")
  # if (length(Id) > 0){
  #   data.frame(filename = basename(fnames[Id]), prj = prjs[Id])#quickly return
  # }else{
  #   cat("All projection correct!\n")
  # }
}

# lapply(fnames, function)
# x = read_table(fnames[1], col_names = F)[[1]]
# str_split(x, '"')[[1]][2]
