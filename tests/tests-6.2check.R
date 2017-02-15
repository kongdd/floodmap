library(readr)
library(magrittr)
library(stringr)
library(data.table)
library(snow)
library(magrittr)
# indir_template <- "E:/洪湖东分块/6风险图应用业务相关数据/6.2淹没过程动态展示支撑数据"
# tmp <- readDBFs_fields(indir_template)

indir6.2 <- "G:/地图审核/乌伦古河/6风险图应用业务相关数据/6.2淹没过程动态展示支撑数据"
indir6.3 <- "G:/地图审核/乌伦古河/6风险图应用业务相关数据/6.3影响分析支撑数据-乌伦古河/"

dbf_files <- dir(indir, pattern = "*.dbf$", full.names = T)[-1]
tmp <- check.ztu(fnames = dbf_files)
# tmp <- readDBFs_fields(indir)
tmp <- check.ztu("G:/地图审核/乌伦古河/6风险图应用业务相关数据/6.3影响分析支撑数据-乌伦古河/")

indir <- "G:/地图审核/乌伦古河/6风险图应用业务相关数据/6.2淹没过程动态展示支撑数据"
fnames <- dir(indir, pattern = "*.txt$", full.names = T)
outdir <- "."
i = 1
tmp <- check.6.2_ymssTXT(fnames[1])
# fnames <- dir("E:/洪湖东分块/6风险图应用业务相关数据/6.2淹没过程动态展示支撑数据",
#               pattern = "*.txt$", full.names = T)
# # x <- check.6.2_ymssTXT(fnames[1])
cl <- makeCluster(5, type = "SOCK", outfile = "log.txt")
tmp <- clusterEvalQ(cl,
                    {
                      library(floodmap)
                      # library(readr)
                      # library(stringr)
                      # library(data.table)
                      NULL
                    })
# clusterExport(cl, c("check.6.2_ymssTXT"), envir = environment())#设置环境
tm <- snow.time(x_newDT <- parLapply(cl, fnames, check.6.2_ymssTXT))
stopCluster(cl)

# x_newDT <- list()
# for (i in seq_along(fnames)){
#   cat(sprintf("====fname:%s====\n", fnames[i]))
#   x_newDT[[i]] <- check.6.2_ymssTXT(fnames[i])
# }

ymss <- lapply(x_newDT, function(x) x[, mean(VALUE, na.rm = T), by = c("TIME")])
ymss_df <- do.call(cbind.data.frame, lapply(ymss, function(x) x[, V1])) %>%
  set_colnames(basename(fnames)) %>% cbind(TIME = ymss[[1]][,TIME], .)
X <- data.table::melt(ymss_df, id.vars = "TIME", variable.name = "ymss")
YMSS.plot(X)

## 可以考虑采用并行运算
## 测试洪湖东数据
template <- "E:/洪湖东分块/6风险图应用业务相关数据/6.2淹没过程动态展示支撑数据/"
fnames <- dir(indir,
              pattern = "*.txt$", full.names = T)
x_hd <- check.6.2_ymssTXT(fnames[1])
ymss <- x_hd[, mean(VALUE, na.rm = T), by = c("TIME")]
## 示例数据
plot(ymss$V1, xlab = "TIME", ylab = "YMSS", type = "l")

writeYMSS(x_hd, "ymss1.txt")
