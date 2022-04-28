#! /usr/bin/Rscript --vanilla
#args <- commandArgs(trailingOnly = TRUE)

######################################################################################################################
# Single File import:
# 1.1) Analysis1_single.R
#
# Multiple File import:
# 1.2) Analysis1_multi.R (this script)
#
# Script to extract data from EC-Lab files (.txt format) and Arbin files (both .res and .xlsx)
# 2) Analysis2.R
# 3) Biologic_Analysis.R
# 4) Arbin_Analysis.R
#
# additionally for plotting:
# in Origin:  SaveDat.R
# as html Report:   - generateReport.R
#                   - RMD_plot.R
#                   - report.Rmd
#####################################################################################################################
# execute by marking lines 13-43 and press STRG+ENTER
#####################################################################################################################

#set working directory for Rfiles:
#Rdir = 'C:/Users/gm5225/Documents/kadi4mat'
#setwd(Rdir)

#load packages, plot settings & Rscripts
source("Library.R")
source("Analysis2.R")
source("Biologic_Analysis.R")
source("Arbin_Analysis.R")
source("SaveDat.R")
source("RMD_plot.R")
source("generateReport.R")

# Choose directory of meta data and import
# dir --> location of meta data file and raw data
file.dir <- file.choose()
dir <- dirname(file.dir)
meta <- read.csv(paste0(dir, "/", 'meta.csv'), header=TRUE, sep=',', quote="\"", dec=".", fill=TRUE)

outdir = paste0(dir, "/Rprocessed")

#Select (optional)
#for voltage profiles: which cycles shall be extracted?
cycles <- c(0,1,2,4,9,14,19,24,49,74,99) #c(0, seq(1,100,5))
#cycles <- c(0,1,2,4,9,12,17,20,25,27) # Rate Capability Test
#cycles <- c(0:27)

#start analysis
#NOTE: integration of meta data still missing
#dat: list that contains
#       1)  1 data.frame for capacity per cycle
#       2)  1 data.frame for e a c h cycle that was defined in cycles vector
#       contents of lists are adressed with dat[[x]]$capacity & dat[[x]]$VoltageProfiles; x = experiment
dat <- analysis(dir, cycles, meta)

#export data as data report and txt files
for(i in 1:length(dat)){

  exp <- dat[[i]]

  if (!requireNamespace("viridis", quietly = TRUE) ||
      !requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "Package \"viridis\" must be installed to use this function.",
      call. = FALSE
    )
  }

  report(exp, Rdir, outdir)
  #SaveToXlxs(outdir, meta, capacity, VPprofiles)

  #save data for further processing
  SaveToOrigin.Stats(outdir, exp)
  #SaveToOrigin.CCCV(outdir, exp)
  #SaveToOrigin.VP(outdir, exp, cycles)

  }


