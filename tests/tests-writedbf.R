# require(foreign)

x <- data.frame(matrix(rnorm(16), 4))
writeDBF(x, "log.dbf")

readDBF_fields("log.dbf")

x <- readDBF("log.dbf")
