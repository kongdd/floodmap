library(floodmap)

indirs <- dir("G:/地图审核/", full.names = T)[3:5]
rivers <- basename(indirs)
# indir <- indirs[1]

file_log <- "南疆审核.txt"
sink(file_log)
for (i in 1:3){
  cat(sprintf("==================================%s=============================\n", rivers[i]))
  indir <- indirs[i]
  indir6.2 <- paste0(indir, "/6风险图应用业务相关数据/6.2淹没过程动态展示支撑数据")
  indir6.3 <- paste0(indir, "/6风险图应用业务相关数据/6.3影响分析支撑数据")

  tmp <- check.ztu(indir6.2)
  prj.check(indir6.2)

  tmp <- check.ztu(indir6.3)
  tmp <- check.ztu(paste0(indir6.3, "/ztu"))
}
sink(NULL)

# file.show(file_log)
