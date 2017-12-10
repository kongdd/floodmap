#' @title readDBF_List2xlsx
#' @description read shapefile dbf table and write into excel *.xlsx
#' @param indir directory which shapefiles dbf file store
#' @param save if save is true, save into file, else import to R variable environment
#' @param fname file name which dbf data saved
#' @examples
#' inputdir <- system.file("data", package = "floodmap")
#' reaDBF_List2xlsx(inputdir, "test01.xlsx")
#' @import openxlsx
#' @importFrom plyr llply
#' @export
readDBF_List2xlsx <- function(indir, fname, save = TRUE, as.is = FALSE){
  filepaths <- dir(indir, pattern = "*.dbf$", full.names = T)
  cat("[01] dbf table is reading ...\n")
  x <- plyr::llply(filepaths, foreign::read.dbf, .progress = "text", as.is = as.is)
  names(x) <- gsub(".dbf", "", basename(filepaths))

  if (save){
    #if (!dir.exists(outputDir)) dir.create(outputDir)
    cat("[02] dbf table is writing in ...\n")
    #shp_write.xlsx2(paste0(outputDir, "/dbf属性表.xlsx"), x)
    writelist_ToXlsx(fname, x)
    cat("Successfully saved!\n")
  }else{
    x#quickly return
  }
}
