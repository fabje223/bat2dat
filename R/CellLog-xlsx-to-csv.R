library(readxl)
library(dplyr)

#select directory
file.dir <- file.choose()
dir.name <- dirname(file.dir)

#read CellLog
cell.log <- read_excel(file.dir, 
                      sheet = 1, range="A16:Z56")

#size depends on number of samples
cell.log <- cell.log[,1:17]

#select parameters for R analysis
col <- c("Identifier", "sample name", "instrument", "cell config", "AM loading")
meta <- cell.log %>%
          filter(metadata %in% col)

#modify transposed data.frame before convertion into .csv
meta.t <- t(meta)
meta.t <- meta.t[5:nrow(meta.t), 1:ncol(meta.t)]
row.names(meta.t) <- 1:nrow(meta.t)
colnames(meta.t) <- c(col)
meta.t[,5] <- as.numeric(meta.t[,5])

#write .csv file
setwd(dir.name)
write.table(meta.t, file="meta.csv", dec=",", sep="\t", row.names=FALSE)
