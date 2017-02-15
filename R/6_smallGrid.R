## 6.3 ztu, rewrite shp dbf files
#' @name get_maskshp
#' @title get_maskshp
#' @description based ztu directory shapefiles clip grid.shp in 6.2
#' @param onlyYMSS use ymss.shp or ddsj, ymls, ymss to clip
#' @importFrom maptools readShapePoly writePolyShape
#' @export
#' @return maskshp.shp at outdir directory
get_maskshp <- function(ztu, fn = "maskshp",outdir = "FloodScheme/", onlyYMSS = T){
  # create outdir if don't have
  if (!dir.exists(outdir)) dir.create(outdir)

  maskshp <- paste0(outdir, fn, ".shp")
  # cat("[1] getting dbf value...")
  x <- reaDBF_List2xlsx(ztu, save = F)
  N <- length(x)/3#scheme nums
  NGRID <- unique(sapply(x, nrow)) #check if grid num is same
  if (length(NGRID) > 1){
    print(NGRID)
    stop("SCHEME GRIDS NUM IS NOT SAME. PLEASE CHECK!\n")
  }else{
    cat(sprintf("SCHEME GRIDS NUM IS SAME: NGRID = %d...\n", NGRID))
  }

  ## only consider ymss grid
  if (onlyYMSS){
    VALUE <- sapply(x[(2*N + 1):(3*N)], function(x) x$VALUE) %>% apply(., 1, sum, na.rm = T)
  }else{
    VALUE <- sapply(x, function(x) x$VALUE) %>% apply(., 1, sum, na.rm = T)
  }
  IdRemain <- which(VALUE > 0)

  shp <- dir(ztu, pattern = "*.shp$", full.names = T)[1]#get ddsj1.shp
  ## copy prj file from ddsj1.shp
  prj <- gsub(".shp$", ".prj", shp)
  maskprj <- gsub(".shp$", ".prj", maskshp)
  file.copy(prj, maskprj)

  ## read shape template from ddsj1.shp, and write shapefile
  cat("[2] Reading template shp...\n")
  shpdf <- readShapePoly(shp)
  cat(sprintf("[3] Writing %s\n", maskshp))
  writePolyShape(shpdf[IdRemain, ], fn = paste0(outdir, fn))#don't contain .shp
  cat(sprintf("Successfully write %s!\n", maskshp))
}

#' @title schemeshp
#' @description according to 6.3 ztu ddsjn.shp, ymlsn.shp, ymssn.shp to write flood scheme shp files.
#' SHOULD BE ATTENTION ZTU directory can't contain shpfiles except ddsjn.shp, ymlsn.shp, ymssn.shp, and
#' the three shp must in ztu directory. Finally generate scheme shapefiles as MIKE II model
#' export, to floodmapping system.
#' @export
#' @return maskshp.shp at outdir directory

get_schemeshp <- function(ztu, outdir = "FloodScheme/"){
  # create outdir if don't have
  if (!dir.exists(outdir)) dir.create(outdir)

  x <- reaDBF_List2xlsx(ztu, save = F)
  N <- length(x)/3#scheme nums
  NGRID <- unique(sapply(x, nrow)) #check if grid num is same
  if (length(NGRID) > 1){
    stop("SCHEME GRIDS NUM IS NOT SAME. PLEASE CHECK!\n")
  }else{
    cat("SCHEME GRIDS NUM IS SAME...\n")
  }
  #generate scheme shpname according to ddsjn.shp, in order to keep same order of value
  scheme <- gsub("ddsj", "FloodScheme_", names(x)[1:N])

  GRID <- x[[1]][, c("GRIDCODE", "GRIDAREA")]#pls check grid value carefully, can't be NULL value
  GRID$GRIDCODE <- as.numeric(GRID$GRIDCODE)
  VALUE <- lapply(x, function(x) x$VALUE)

  mikeResult <- list()
  for (i in 1:N){
    mikeResult[[i]] <- data.frame(GRID, DDSJ = VALUE[[i]], YMLS = VALUE[[i + N]],
                                  YMSS = VALUE[[i + 2*N]])
  }
  names(mikeResult) <- scheme

  ## write mikeResult into floodScheme_n.shp files
  shpTemplate <- dir(ztu, pattern = "ddsj1", full.names = T)
  for (i in 1:N) {
    cat(sprintf("Writing %02dth: %s.shp\n", i, scheme[i]))
    floodshp <- paste0(outdir, gsub("ddsj1", scheme[i],basename(shpTemplate)))
    file.copy(shpTemplate, floodshp)
    # floodshp[1] is dbf file, and use mikeResult we generate overwrite it.
    writeDBF(mikeResult[[i]], floodshp[1], precisionIn = c(10, 13, 13, 13, 13), scaleIn = c(0, 2, 4, 4, 4))
  }
}
