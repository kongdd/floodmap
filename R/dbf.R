#' @name readDBF
#' @aliases readDBF
#' @title read dbf file
#' @description The function reads a DBF file into a data frame, converting character
#' fields to factors, and trying to respect NULL fields.
#' The DBF format is documented but not much adhered to.  There is is no
#' guarantee this will read all DBF files.
#' @export
#' @usage read.dbf(file, as.is = FALSE)
#' @param file name of input file
#' @param as.is should character vectors not be converted to factors?
#' @references url{http://shapelib.maptools.org/}.
#'  The Borland file specification \emph{via} \url{http://www.wotsit.org},
#'  currently at \url{http://www.wotsit.org/list.asp?fc=6}.
#' @seealso \code{\link{write.dbf}}
#'
#' @examples
#'  x <- read.dbf(system.file("files/sids.dbf", package="foreign")[1])
#'  str(x)
#'  summary(x)

readDBF <- function(file, as.is = FALSE)
{
  df <- .Call(Rdbfread, as.character(path.expand(file)))
  onames <- names(df)
  inames <- make.names(onames, unique = TRUE)
  names(df) <- inames
  if (!(identical(onames, inames))) {
    for (i in seq_along(onames))
      if (!(identical(onames[i], inames[i])))
        message(gettextf("Field name: %s changed to: %s",
                         sQuote(onames[i]), sQuote(inames[i])),
                domain = NA)
  }
  data_types <- attr(df, "data_types")
  for(i in seq_along(onames))
    if(data_types[i] == "D") df[[i]] <- as.Date(df[[i]], format="%Y%m%d")
  if(!as.is) {
    df <- data.frame(lapply(df, function(x) if(is.character(x)) factor(x) else x))
    attr(df, "data_types") <-  data_types
  }
  df
}

#' @title writeDBF
#' @description modified write.dbf from foreign package, in order to control field precision
#'  and scale properties
#' @param precisionIn filed length, should be caution that: number of digits including minus sign and decimal sign
#'  scaleIn: digits after the decimal sign
#' @param scaleIn numbers after digital point.
#' @param cofyfile if TRUE, original dbf file would be rename to "*.dbfold" to present dbf data missing
#' @export
writeDBF <- function (dataframe, file, factor2char = TRUE, max_nchar = 254,
                      precisionIn = NULL, scaleIn = NULL, copyfile = F)
{
  allowed_classes <- c("logical", "integer", "numeric", "character",
                       "factor", "Date")
  if (!is.data.frame(dataframe))
    dataframe <- as.data.frame(dataframe)
  if (any(sapply(dataframe, function(x) !is.null(dim(x)))))
    stop("cannot handle matrix/array columns")
  cl <- sapply(dataframe, function(x) class(x[1L]))
  asis <- cl == "AsIs"
  cl[asis & sapply(dataframe, mode) == "character"] <- "character"
  if (length(cl0 <- setdiff(cl, allowed_classes)))
    stop(sprintf(ngettext(length(cl0), "data frame contains columns of unsupported class %s",
                          "data frame contains columns of unsupported classes %s"),
                 paste(dQuote(cl0), collapse = ",")), domain = NA)
  m <- ncol(dataframe)
  DataTypes <- c(logical = "L", integer = "N", numeric = "F",
                 character = "C", factor = if (factor2char) "C" else "N",
                 Date = "D")[cl]
  for (i in seq_len(m)) {
    x <- dataframe[[i]]
    if (is.factor(x))
      dataframe[[i]] <- if (factor2char)
        as.character(x)
    else as.integer(x)
    else if (inherits(x, "Date"))
      dataframe[[i]] <- format(x, "%Y%m%d")
  }
  precision <- integer(m)
  scale <- integer(m)
  dfnames <- names(dataframe)
  for (i in seq_len(m)) {
    nlen <- nchar(dfnames[i], "b")
    x <- dataframe[, i]
    if (is.logical(x)) {
      precision[i] <- 1L
      scale[i] <- 0L
    }
    else if (is.integer(x)) {
      rx <- range(x, na.rm = TRUE)
      rx[!is.finite(rx)] <- 0
      if (any(rx == 0))
        rx <- rx + 1
      mrx <- as.integer(max(ceiling(log10(abs(rx)))) +
                          3L)
      precision[i] <- min(max(nlen, mrx), 19L)
      scale[i] <- 0L
    }
    else if (is.double(x)) {
      precision[i] <- 19L
      rx <- range(x, na.rm = TRUE)
      rx[!is.finite(rx)] <- 0
      mrx <- max(ceiling(log10(abs(rx))))
      scale[i] <- min(precision[i] - ifelse(mrx > 0L, mrx +
                                              3L, 3L), 15L)
    }
    else if (is.character(x)) {
      mf <- max(nchar(x[!is.na(x)], "b"))
      p <- max(nlen, mf)
      if (p > max_nchar)
        warning(gettextf("character column %d will be truncated to %d bytes",
                         i, max_nchar), domain = NA)
      precision[i] <- min(p, max_nchar)
      scale[i] <- 0L
    }
    else stop("unknown column type in data frame")
  }
  ## Modified By Dongdong KONG, 2016-07-17
  if (!is.null(precisionIn)) precision <- precisionIn
  if (!is.null(scaleIn)) scale <- scaleIn
  if (copyfile) file.rename(file, paste0(file, "old"))

  if (any(is.na(precision)))
    stop("NA in precision")
  if (any(is.na(scale)))
    stop("NA in scale")
  invisible(.Call(DoWritedbf, as.character(file), dataframe,
                  as.integer(precision), as.integer(scale), as.character(DataTypes)))
}

environment(writeDBF)<- environment(foreign::write.dbf)
environment(readDBF)<- environment(foreign::read.dbf)
