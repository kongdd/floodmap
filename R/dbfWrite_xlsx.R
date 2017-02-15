#' @title writelist.xlsx
#' @description base function, write list x into fileName with each list in workbook,
#' write shapefile dbf table into excel *.xlsx
#' @details should be cautious that x attribute name can't be NULL should be
#' @examples
#' data(hydrodata)
#' writelist_ToXlsx("hydrodata.xlsx", hydrodata, .progress = "text")
#' @import openxlsx
#' @export
#'
writelist_ToXlsx <- function (x, fname, .progress = "text", rowNames = FALSE, ...)
{
  sheetNames <- names(x)
  if (is.null(sheetNames))
    sheetNames <- paste0("sheet", seq_along(x))
  wb <- createWorkbook()
  options(openxlsx.borderStyle = "none")
  hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER",
                     textDecoration = "Italic", border = "Bottom")
  writeIn <- function(i) {
    addWorksheet(wb, sheetNames[i])
    writeData(wb, sheetNames[i], x[[i]], colNames = TRUE, rowNames = rowNames, borders = "rows", headerStyle = hs1, ...)
  }
  if (.progress != "none")
    cat(sprintf("[---- Writing into Workbook ----]\n"))
  tmp <- llply(seq_along(x), writeIn, .progress = .progress)
  if (.progress != "none")
    cat(sprintf("[---- Writing into xlsx file: %s ----]\n",
                fname))
  saveWorkbook(wb, fname, overwrite = TRUE)
}

#' @title readxlsx_ToList
#' @description base function, if one excel file hava many sheets, this function will be work
#' @details should be cautious that x attribute name can't be NULL should be
#' @param fname file name included path going to be read.
#' @examples
#' fname <- system.file("data", "hydrodata.xlsx", package = "floodmap")
#' x <- readxlsx_ToList(fname)
#' @import openxlsx readxl
#' @importFrom plyr llply
#' @export
readxlsx_ToList <- function(fname, show = F, ...){
  cat(sprintf("[---- Reading File: %s ----]\n", fname))
  ## judge whether it's xls or xlsx
  #  if file is *.xls use readxl::read_excel
  if (length(grep("xls", basename(fname))) != 0){
    sheetNames <- excel_sheets(fname)
    x <- llply(sheetNames, function(sheet) as.data.frame(read_excel(fname, sheet, ...)),
               .progress = "text")
  }else{
    sheetNames <- getSheetNames(fname)
    x <- llply(sheetNames, function(sheet) read.xlsx(fname, sheet),
               .progress = "text")
  }
  names(x) <- sheetNames
  if (show) print(x)
  x#quickly return
}


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
