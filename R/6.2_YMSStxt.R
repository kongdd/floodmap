#' @title check.6.2_ymssTXT
#' @description check floodmap 6.2 part ymss txt file
#' @importFrom readr read_lines
#' @importFrom stringr str_trim str_split
#' @import data.table
#' @export
##  6.2 check
check.6.2_ymssTXT <- function(fname, hourly = T){
  # setTxtProgressBar(pb, i)
  # tmp <- gsub("(^\\s*)|(\\s*$)", "", x[i], perl = T)
  # xi[[i]] <- strsplit(tmp, "\\s+|\t|\\s+\t")[[1]] %>% as.numeric()
  NAME <- basename(fname)
  x <- read_lines(fname)
  t_trim <- system.time(x_trim <- str_trim(x, side = "b"))
  cat(sprintf("[1] %s: str_trim elapsed %.3fs\n", NAME, t_trim[["elapsed"]]))#print(t_trim)
  t_split <- system.time(x_list <- str_split(x_trim, regex("\\s+|\t|\\s+\t")) )
  cat(sprintf("[2] %s: str_split elapsed %.3fs\n", NAME, t_split[["elapsed"]]))#print(t_split)

  #time header lines
  # Id_head <- laply(x_list, function(x) length(x) != 2, .progress = "text")
  Id_isone <- listlen(x_list) == 1
  Id <- which(Id_isone)
  runs <- rle(cumsum(Id_isone))
  runs$values <- as.numeric(unlist(x_list[Id]))#time
  TIME <- inverse.rle(runs)#recode original series timeId, 2016-08-24
  # TIME <- cumsum(Id_isone) + as.numeric(x_list[[1]]) - 1

  cat(sprintf("[3] %s: Fill list blank, and change list to data.frame...\n", NAME))
  df_tmp <- list_change(x_list, Id)#format(object.size(df_tmp), "MB")
  cat(sprintf("[4] %s: Construct data.table, and convert data type... ", NAME))

  t_dt <- system.time({
    x_dt <- data.table(TIME, GRIDCODE = df_tmp$GRIDCODE, VALUE = df_tmp$VALUE)
    # x_dt0 <- data.table(TIME, GRIDCODE = df_tmp$GRIDCODE, VALUE = df_tmp$VALUE)
    # setkey(x_dt, TIME, GRIDCODE)
    x_dt[, c("GRIDCODE","VALUE") := list(as.integer(GRIDCODE), round(as.numeric(VALUE), 2))]
  })
  cat(sprintf("data.table elapsed %.3fs\n", t_dt[["elapsed"]]))

  ## delete zero values
  Id_del <- x_dt[, which(VALUE == 0)]
  if(length(Id_del) > 0){
    x_new <- x_dt[-Id_del, ]
  }else{
    x_new <- x_dt
  }#quickly return

  ## delete time only contain one value
  Id <- x_new[, .N, by = TIME]
  timeId <- which(Id[, N] != 1)

  if (hourly) {
    dt <- x_new[TIME %in% Id[timeId, TIME]]
  }else {
    dt <- x_new[TIME %in% Id[intersect(seq(1, nrow(Id), 10), timeId), TIME]]
    dt[is.na(VALUE), `:=`("GRIDCODE", as.integer(ceiling(GRIDCODE/10)))]
  }
  #dt <- x_dt[TIME %in% intersect(seq(1, nrow(Id), 6), timeId)]
  # setkey(dt, TIME, GRIDCODE)

  rm(x, x_trim, x_list, df_tmp)#save memory
  gc(); gc()
  dt#quickly return
}

#' @title get_ymssTXTinfo_summary
#' @export
get_ymssTXTinfo_summary <- function(dt){
  x <- dt[, list(TIME, GRIDCODE, VALUE)] %>% unclass()
  infot <- lapply(x, summary)#info temp
  info <- data.frame(matrix(NA, nrow = 3, ncol = 7))
  for (i in seq_along(infot)) info[i, seq_along(infot[[i]])] <- infot[[i]]
  dimnames(info) <- list(names(x), c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.", "NA's" ))
  info$var <- names(x)
  # info$continue <- info[1, 6] - info[1, 1] + 1 == info[3, 7]
  TIME <- dt[, unique(TIME)]
  missTime <- setdiff(1:TIME[length(TIME)], TIME) %>% paste(., collapse = ",")
  NotContinue <- dt[!is.na(VALUE), length(unique(GRIDCODE)) == .N, by = TIME][which(!V1), TIME] %>% paste(., collapse = ",")
  info["TIME", "missTime"] <- missTime
  info["GRIDCODE", "NotUniqueTime"] <- NotContinue
  info[, c(8, 1:7, 9:10)]#quickly return
}

#' @title get_ymssTXTinfo
#' @export
get_ymssTXTinfo <- function(x_newDT, save = FALSE, file = "YMSStxt_information.xlsx", ...){
  cat(sprintf("Getting YMSSn.txt summary information...\n"))

  info <- llply(x_newDT, get_ymssTXTinfo_summary, .progress = "text") %>%
  set_names(sapply(strsplit(names(x_newDT), "\\."), `[`, 1))
  vars <- colnames(info[[1]])
  result <- data.table::melt(info, id.vars = vars) %>%
    set_colnames(c(vars, "file")) %>%
    .[, c("file", vars)] %>% as.data.frame()

  # infocbind$var <- paste(infocbind$L1, infocbind$var, sep = "_")
  # result <- infocbind %>% .[, 1:(ncol(.) - 1)]
  if (save) {
    options(openxlsx.borderStyle = "none")
    hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER",
                       textDecoration = "Italic", border = "Bottom")
    write.xlsx(result, file, ..., borders = "rows", headerStyle = hs1)
  }
  result#quickly return
}

#' @title readYMSStxts
#' @description read all ymssn.txt files in indir6.2
#' @import snow
#' @export
readYMSStxts <- function(indir6.2, fnames_txt = NULL, parallel = FALSE, ncluster = 4){
  if (is.null(fnames_txt)) fnames_txt <- dir.sort(indir6.2, pattern = "*.txt", full.names = T)

  if (parallel){
    cl <- makeCluster(ncluster, type = "SOCK")
    tmp <- clusterEvalQ(cl,
                        {
                          library(floodmap)
                          # library(data.table)
                          NULL
                        })
    # clusterExport(cl, "reWriteYMSS")
    tm <- snow.time(x_newDT <- parLapply(cl, fnames_txt, check.6.2_ymssTXT, hourly = T))
    # clusterEvalQ(cl, {rm(list = ls()); gc()})
    stopCluster(cl)
    names(x_newDT) <- basename(fnames_txt)
  }else{
    x_newDT <- llply(fnames_txt, check.6.2_ymssTXT, .progress = "text") %>% set_names(basename(fnames_txt))
  }
  x_newDT#quickly return
}
#' @title writeYMSS
#' @description write data.table variable into ymsstxt file. Time must be continued
#' @import data.table
#' @export
writeYMSS <- function(x_new, fname){
  time <- c(x_new[, which(is.na(VALUE))], nrow(x_new) + 1)#update Id_head, much faster
  # microbenchmark::microbenchmark(time <- c(which(Id_isone[-Id_del]), nrow(x_new) + 1),
  #                                time <- x_dt[, which(is.na(VALUE))], times = 2)
  tryCatch({
    cat(sprintf("%s: Writing into file...\n", basename(fname)))
    # outfile <- paste(outdir, basename(fname), sep = "/")
    sink(fname)
    # pb <- txtProgressBar(max = length(Id_head), style = 3)
    # setTxtProgressBar(pb, i)
    for (i in 1:(length(time) - 1)){
      tmp <- x_new[(time[i] + 1): (time[i + 1] - 1), ]
      cat(sprintf("%d\n", x_new[time[i], GRIDCODE]))
      cat(sprintf("%d\t%.2f\n", tmp[, GRIDCODE], tmp[, VALUE]), sep = "")
    }
    sink(NULL)
  }, error = function(e) {warning("Save failed"); sink(NULL)})
  # file.show(fname)
}

#' @title writeYMSStxts
#' @export
writeYMSStxts <- function(x_newDT, ntime = NULL){
  ## rewrite ymssn.txt files:
  if (!is.null(ntime)) x_newDT <- lapply(x_newDT, function(x) x[TIME %in% seq(ntime), ])
  ## 重写淹没水深
  for (i in seq_along(x_newDT)){
    cat(sprintf("Writing %02dth...\n", i))
    dt <- x_newDT[[i]]
    # setkey(dt, TIME, GRIDCODE)
    # ## if in a time YMSS all is zero
    # timeLen <- which(dt[, .N, by = TIME][, N] == 1)
    # if (length(timeLen) > 0) dt <- dt[-(TIME %in% timeLen)]
    writeYMSS(dt, names(x_newDT)[i])
  }
}

#' @title zipNum
#' @export
encodeNUM <- function(x){
  N <- length(x)
  if (N > 1){
    x[c(1, N)] %>% paste(collapse = ":")
  }else{
    as.character(x[1])
  }
}

#' @title zipNum
#' @export
zipNUM <- function(x){
  if (length(x) > 0){
    dt <- data.table(x, tag = c(0, cumsum(diff(x) != 1)) + 1)
    dt[, list(zip = encodeNUM(x)), by = tag]
  }
}

#' @title check_gridcodeMatch
#' @export
check_gridcodeMatch <- function(x_list.62, x_newDT){
  cat("Matching gridcode, and calculate water volume...\n")
  # x_areaDT <- as.list(seq(length(x_newDT))) %>% set_names(names(x_newDT))
  info <- as.list(seq(length(x_newDT))) %>% set_names(names(x_newDT))
  for (i in seq_along(x_newDT)){
    # cat(sprintf("Running %02dth\n", i))
    shp <- x_list.62[[1]][[i]]
    dt <- x_newDT[[i]]
    # x_newDT[, `:=`(GRIDAREA = match())]
    dt[, `:=`(GRIDAREA = shp$GRIDAREA[match(GRIDCODE, shp$GRIDCODE)])]
    dt[, `:=`(waterV = GRIDAREA*VALUE/10000)]#waterV unit: 10^4 m^3

    code <- dt[!is.na(VALUE), GRIDCODE[which(is.na(GRIDAREA))]]#not matched gridcode
    if (length(code) > 0){
      cat(sprintf("[%02d] %s GRIDCODE can't completely match. GRIDCODEs :\n",
                  i, names(x_newDT)[i]))
      info[[i]] <- zipNUM(sort(unique(code)))
      print(info[[i]])
    }else{
      cat(sprintf("[%02d] %s GRIDCODE completely matched.\n", i, names(x_newDT)[i]))
    }
    x_newDT[[i]] <- dt
  }
  x_newDT
  # x_areaDT#quickly return Dt with GRIDAREA and water Volume
}

## plot to show YMSS progress
windowsFonts(HT = windowsFont("SimHei"), St = windowsFont("SimSun"),
             Times = windowsFont("Times New Roman"), Arial = windowsFont("Arial"))
f_melt <- function(x) reshape2::melt(x, id.vars = c("TIME", "VALUE", "waterV"),
                                     variable.name = "ymss") %>%
  set_colnames(c("TIME", "VALUE", "waterV", "ymss"))

f_v <- function(data, river) ggplot(data, aes(x = TIME, y = VALUE, colour = ymss, shape = ymss)) +
  geom_line(size = 1) + geom_point() +
  ggtitle(paste0(river, "YMSStxt")) +
  ylab(label = expression("Water Volume"~"("~10^4~m^3~")")) +
  theme(title = element_text(size = 14, family = "Times"),
        axis.title = element_text(size = 14, family = "Arial"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

f_value <- function(data, river) ggplot(data, aes(x = TIME, y = waterV, colour = ymss, shape = ymss)) +
  geom_line(size = 1) + geom_point() +
  ggtitle(paste0(river, "YMSStxt")) +
  ylab(label = expression("Water Volume"~"("~10^4~m^3~")")) +
  theme(title = element_text(size = 14, family = "HT"),
        axis.title = element_text(size = 14, family = "Times"),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

#' @title writeYMSStxts
#' @export
YMSStxt.plot <- function(x_areaDT, river){
  df <- lapply(x_areaDT, function(x) x[, .(VALUE = sum(VALUE, na.rm = T),
                                           waterV = sum(waterV, na.rm = T)), by = TIME])
  df_list <- split(df, findInterval(seq_along(df), 7))
  df_plot <- lapply(df_list, f_melt);
  handles <- lapply(df_plot, function(x) print(f_v(x, river)))
  df#quickly return
}
