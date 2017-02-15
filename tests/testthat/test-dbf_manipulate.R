## readDBF_List2xlsx
inputdir <- system.file("data", package = "floodmap")
reaDBF_List2xlsx(inputdir, "test01.xlsx")

## test readDBFs_fields
inputdir <- system.file("data", package = "floodmap")
fields <- readDBFs_fields(inputdir)

## test readDBF_fields
fnames <- dir(system.file("data", package = "floodmap"), pattern = "*.dbf$", full.names = T)
for (i in seq_along(fnames)){
  cat(sprintf("[%02d]----%s\n", i, fnames[i]))
  print(readDBF_fields(fnames[i]))
}

## test readxlsx_ToList
fname <- system.file("data", "hydrodata.xlsx", package = "floodmap")
x <- readxlsx_ToList(fname, .progress = "text")

## test writelist_ToXlsx
data(hydrodata)
writelist_ToXlsx("hydrodata.xlsx", hydrodata, .progress = "text")
