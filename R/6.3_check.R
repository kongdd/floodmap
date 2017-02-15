#' @name readDBFs.ztu
#' @title readDBFs.ztu
#' @export
readDBFs.ztu <- function(indir, IsSplit = TRUE, checkField = FALSE, show = FALSE){
  #get file names in ztu directory
  if (!is.null(indir)) {
    #reorder fnames according scheme number
    fnames <- dir.sort(indir, pattern = "*.dbf$", full.names = T)
  }else {
    indir <- dirname(fnames[1])
  }

  #check dbf fields
  if (checkField) {
    fields <- readDBFs_fields(indir, show = show)
    # find which dbf's field don't have the three need field
    Id <- which(sapply(fields, function(x) length(which(toupper(x$NAME) %in% c("GRIDCODE", "GRIDAREA", "VALUE")))) != 3)
    if (length(Id) > 0) {
      warning(sprintf("%s don't contain all need fields!\n", paste(basename(fnames[Id]), collapse = ", ", sep = "")))
      fnames <- fnames[-Id]
    }else{
      cat("All dbfs contain GRIDCODE, GRIDAREA, VALUE field!\n")
    }
  }

  type <- substr(basename(fnames), 1, 4)#ddsj, ymls, ymss

  cat("Reading dbf files ...\n")
  x <- llply(fnames, function(fname) readDBF(fname) %>% set_names(toupper(colnames(.))), .progress = "text")
  names(x) <- gsub(".dbf", "", basename(fnames))

  cat("Success!\n")
  if (IsSplit){
    split(x, type)#quickly return
  }else{
    x#quickly return
  }
}

#' @name check.ztu_merge
#' @title check.ztu_merge
#' @export
check.ztu_merge <- function(x_list, show = FALSE, save = TRUE, file = "dedectInfo.xlsx"){
  #if (is.null(names(x_list)))
  info.gridcode <- info.gridarea <- info.value <- as.list(numeric(length(x_list))) %>% set_names(names(x_list))
  for (i in seq_along(x_list)) {
    cat(sprintf("[%d]: -----------Testing %s-----------\n", i, names(x_list)[i]))
    # info <- sapply(x, summary) %>% t
    info <- list()
    # get gridcod information detail
    cat("[01] GRIDCODE SUMMARY\n")
    info.gridcode[[i]] <- sapply(x_list[[i]],
                            function(x) c(summary.default(x$GRIDCODE, digits = 5) %>% unclass(),
                                          sapply(c(0, 1), function(code) length(which(x$GRIDCODE == code))) %>%
                                            set_names(c("value0", "value1")),
                                          len = nrow(x), uniquelen = length(unique(x$GRIDCODE)),
                                          unique = nrow(x) == length(unique(x$GRIDCODE)))) %>% t

    cat("[02] GRIDAREA SUMMARY\n")
    info.gridarea[[i]] <- sapply(x_list[[i]], function(x) summary(x$GRIDAREA)) %>% t
    cat("[03] VALUE SUMMARY\n")
    info.value[[i]] <- sapply(x_list[[i]], function(x) summary(x$VALUE)) %>% t
    # cat("=======================================\n")
    # detectResult[[i]] <- info
    # print(info)
    # writelist_ToXlsx(fname = paste0(names(fnames_list)[i], ".xlsx"), info)
  }
  result <- list(do.call(rbind, info.gridcode), do.call(rbind, info.gridarea), do.call(rbind, info.value))
  names(result) <- c("GRIDCODE", "GRIDAREA", "VALUE")

  if (show) print(result)
  if (save) writelist_ToXlsx(result, file, rowNames = TRUE)
  result#quickly return, detect info list
}

#' @name check.ztu_simply
#' @title check.ztu_simply
#' @export
check.ztu_simply <- function(ztu, show = TURE, save = FALSE, file = "dedectInfo.xlsx"){
	x_list <- readDBFs.ztu(ztu, IsSplit = TRUE, checkField = TRUE)
	check.ztu_merge(x_list, show, save, file)
}
#' @name check.ztu
#' @title check floodmap 6.3 data. Don't suggest, will be removed in future.
#' @description before use this function, should be check weather there have value field. Otherwise, will
#' generate fatal error
#' @import magrittr
#' @param inputdir 6.3/ztu path
#' @param fnames if inputdir is null, fnames will be used.
#' @examples
#' TMP <- ztu.check(indir)
#' @export
# read all dbf values in ztu
check.ztu <- function(indir = NULL, fnames){
  if (!is.null(indir)){
    fnames <- dir.sort(indir, pattern = "*.dbf$", full.names = T)
  }else{
    indir <- dirname(fnames[1])
  }
  ## check whether VALUE field exist
  fields <- readDBFs_fields(indir)
  Id <- sapply(fields, function(x) !("VALUE" %in% toupper(x$NAME))) %>% which
  if (length(Id) > 0) {
    warning(sprintf("%s don't contain 'VALUE' field!",
                    paste(fnames[Id], collapse = ", ", sep = "")))
    fnames <- fnames[-Id]
  }
  ## end field check
  ## split data into group according to filename
  type <- substr(basename(fnames), 1, 4)
  fnames_list <- split(fnames, type)
  type <- names(fnames_list)
  valuedf <- list()

  for (i in seq_along(fnames_list)){
    cat(sprintf("[%d]: -----------Testing %s-----------\n", i, type[i]))
    x <- lapply(fnames_list[[i]], function(fname) readDBF(fname) %>% set_names(toupper(colnames(.))) %>% .[, "VALUE"])

    names(x) <- gsub(".dbf", "", basename(fnames_list[[i]]))
    valuedf[[i]] <- x
    print(sapply(x, summary))
    cat("=======================================\n")
  }
  names(valuedf) <- type
  valuedf#quickly return
}
#' @name check.ztu_formatDBF
#' @title format 6.3 ztu dbf field type, precision, scale
#' @description only used for ztu shapefile data. Out of date, don't suggest
#' @import magrittr
#' @param indir 6.3/ztu path
#' @examples
#' TMP <- check.ztu_formatDBF(indir)
#' @export
check.ztu_formatDBF <- function(indir){
  # indir <- "H:/ztu/"
  fields <- readDBFs_fields(indir)
  fnames <- dir.sort(indir, full.names = T, pattern = "*.dbf$")

  for (i in seq_along(fnames)){
    cat(sprintf("[%2d]: %s\n", i, fnames[i]))
    x <- readDBF(fnames[i])
    ## convert2double: gridcode, gridarea, value
    x <- lapply(x, function(x) as.double(x)) %>% set_names(toupper(names(.))) %>%
      do.call(cbind.data.frame, .) %>% .[, c("GRIDCODE", "GRIDAREA", "VALUE")]
    x$VALUE <- round(x$VALUE, 2)
    # writeDBF(x, fnames_old[i], precisionIn = c(10, 11, 11), scaleIn = c(0, 2, 2), copyfile = T)
    # write.dbf(x, fnames_old[i])
    writeDBF(x, fnames[i], precisionIn = c(10, 11, 11), scaleIn = c(0, 2, 2), copyfile = T)
  }
}
