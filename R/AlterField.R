#' @title readDBF_fields
#' @details read dbf table fields Name, TYPE, precision and scale. The result is same as ArcGIS
#' @param dbf.name dbf file name
#' @examples
#' fnames <- dir(system.file("data", package = "floodmap"), pattern = "*.dbf$", full.names = T)
#' for (i in seq_along(fnames)){
#'    cat(sprintf("[%02d]----%s\n", i, fnames[i]))
#'    print(readDBF_fields(fnames[i]))
#' }
#' @export
readDBF_fields <- function(dbf.name){
  endian = "little"
  infile<-file(dbf.name,"rb")
  #Header
  file.version <- readBin(infile,integer(), 1, size=1, endian=endian)
  file.year <- readBin(infile,integer(), 1, size=1, endian=endian)
  file.month <- readBin(infile,integer(), 1, size=1, endian=endian)
  file.day <- readBin(infile,integer(), 1, size=1, endian=endian)
  num.records <- readBin(infile,integer(), 1, size=4, endian=endian)
  header.length <- readBin(infile,integer(), 1, size=2, endian=endian)
  record.length <- readBin(infile,integer(), 1, size=2, endian=endian)
  file.temp <- readBin(infile,integer(), 20, size=1, endian=endian)
  header <- list(file.version,file.year, file.month, file.day, num.records, header.length, record.length)
  names(header) <- c("file.version","file.year","file.month","file.day","num.records","header.length","record.length")
  rm(file.version,file.year, file.month, file.day, num.records, header.length, record.length)

  #Calculate the number of fields
  num.fields <- (header$header.length-32-1)/32
  field.name <- NULL
  field.type <- NULL
  field.length <- NULL
  field.decimal <- NULL

  #Field Descriptions (32 bytes each)
  for (i in 1:num.fields) {
    # field.name.test <- readBin(infile, character(), 1, size = 10, endian="big")
    field.name.test <- readBin(infile, character(), 1, size = 10, endian=endian)
    field.name <- c(field.name,field.name.test)
    if (nchar(field.name.test)!=10) {
      file.temp <- readBin(infile,integer(), 10-(nchar(field.name.test)), 1, endian=endian)
    }
    #warning in readChar:In readChar(con, 5) : can only read in bytes in a non-UTF-8 MBCS locale
    # field.type <- c(field.type,readChar(infile, 1))
    field.type <- c(field.type, rawToChar(readBin(infile, "raw", 1, size = 1, endian = endian)))
    file.temp <- readBin(infile,integer(), 4, 1, endian=endian)
    field.length <- c(field.length, readBin(infile,integer(), 1, 1, endian=endian))
    field.decimal <- c(field.decimal, readBin(infile,integer(), 1, 1, endian=endian))
    file.temp <- readBin(infile,integer(), 14, 1, endian=endian)
  }
  # digital point also include in original dbf field header information, in order to consistent with
  # arcgis result, fixed it
  Id_dicimal <- which(field.decimal > 0)
  if (length(Id_dicimal)) field.length[Id_dicimal] <- field.length[Id_dicimal] - 1

  #Create a table of the field info
  fields <- data.frame(NAME=field.name,TYPE=field.type,LENGTH=field.length, DECIMAL=field.decimal,
  	dbf = strsplit(basename(dbf.name), "\\.")[[1]][1], stringsAsFactors = FALSE)

  #Set all fields with length < 0 equal to correct number of characters
  Id <- which(fields$LENGTH < 0)
  if (length(Id) > 0) fields$LENGTH[Id] <- (256 + fields$LENGTH[Id])

  #Read in end of attribute descriptions terminator - should be integer value 13
  file.temp <- readBin(infile,integer(), 1, 1, endian=endian)
  close(infile)
  fields
}

#' @title readDBFs_fields
#' @details read dbf table fields Name, TYPE, precision and scale. The result is same as ArcGIS
#' if fnames input, indir will be ignore
#' @param indir a directory under which have dbf files
#' @examples
#' inputdir <- system.file("data", package = "floodmap")
#' fields <- readDBFs_fields(inputdir)
#' @export
readDBFs_fields <- function(indir, fnames = NULL, checkField = FALSE, show = TRUE){
  if (is.null(fnames)) fnames <- dir.sort(indir, pattern = "*.dbf$", full.names = T)

  fields <- list()
  for (i in seq_along(fnames)){
    fields[[i]] <- readDBF_fields(fnames[i])
    if (show) {
      cat(sprintf("[%02d]----------------------------------\n", i))
      print(fields[[i]])
    }
  }
  names(fields) <- basename(fnames)

  if (checkField){
  	Id <- which(sapply(fields, function(x) length(which(toupper(x$NAME) %in% c("GRIDCODE", "GRIDAREA", "VALUE")))) != 3)
	if (length(Id) > 0) {
	  warning(sprintf("%s don't contain all need fields!\n", paste(basename(fnames[Id]), collapse = ", ", sep = "")))
	  fnames <- fnames[-Id]
	}else{
	  cat("==================================================\n")
      cat("All dbfs contain GRIDCODE, GRIDAREA, VALUE field!\n")
	}
  }
  fields#quickly return
}

