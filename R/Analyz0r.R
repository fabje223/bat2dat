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
#source("Library.R")
#source("Analysis2.R")
#source("Biologic_Analysis.R")
#source("Arbin_Analysis.R")
#source("SaveDat.R")
#source("RMD_plot.R")
#source("generateReport.R")

# initiate data processing with
# analyz0r <- process0r(htmlReport = TRUE, exportCap = TRUE)

process0r <- function(reportHTML = FALSE, exportCap = TRUE) {

                  if(reportHTML == FALSE && exportCap == FALSE) {
                    stop(
                      "Select an export format",
                      call. = FALSE
                    )
                  }

                  processedData <- function() {

                                  #Select (optional)
                                  #for voltage profiles: which cycles shall be extracted?
                                  cycles <- c(0,1,2,4,9,14,19,24,49,74,99) #c(0, seq(1,100,5))
                                  #cycles <- c(0,1,2,4,9,12,17,20,25,27) # Rate Capability Test
                                  #cycles <- c(0:27)

                                  #locate experimental data
                                  dir.info <- meta.dir()
                                  dir <- dir.info$dir
                                  meta <- dir.info$meta

                                  #start analysis
                                  eval.raw <- analysis(dir, cycles, meta)

                                  return(eval.raw)
                  }

                  #export data as data report and/or txt files
                  for(i in 1:length(processedData)){

                    exp <- processedData[[i]]

                    if (!requireNamespace("viridis", quietly = TRUE) ||
                        !requireNamespace("rmarkdown", quietly = TRUE)) {
                      stop(
                        "Package \"viridis\" must be installed to use this function.",
                        call. = FALSE
                      )
                    }

                    #Generate html report using RMarkdown
                    if(htmlReport == TRUE){
                        report(exp, Rdir, outdir)
                    }

                    #Export data as .txt (Origin compatible)
                    if(exportCap == TRUE){
                        SaveToOrigin.Stats(outdir, exp)
                    }
                      #SaveToOrigin.CCCV(outdir, exp)
                      #SaveToOrigin.VP(outdir, exp, cycles)

                  }
              }
