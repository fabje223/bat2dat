## code to prepare `DATASET` dataset goes here

exampleData <- read.table("data-raw/DATASET.txt", header=T, dec = ",", sep = "\t", fill=TRUE)
usethis::use_data(exampleData, overwrite = TRUE)

