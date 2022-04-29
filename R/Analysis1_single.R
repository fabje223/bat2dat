blub <- function() {#Single File Import for EC-Lab files (.txt)
library(dplyr)
library(gridExtra)
library(ggplot2)

#load scripts for analysis and export
source("Biologic_Analysis.R")
source("SaveDat.R")
source("RMD_plot.R")
source("generateReport.R")

#State active material mass (in mg)
AM.mass <- 1.00
#State type of cell ('anode', 'cathode', 'full')
type <- 'LiS'
#State which cycles should be extracted to plot voltage profiles
#Note: Biologic starts counting from 0
cycles <- c(0,1,4,9,24,49,74,99)
#In which directory should R put the result files?
outdir <- c('Q:/DataEvaluation/2_R-CyclingData/output')

#choose directory:
file.dir <- file.choose()

#save file name and directory
filename <- basename(file.dir)
dir.name <- dirname(file.dir)

#generate a meta file from the cell's metadata
meta <- data.frame('filename' = filename,
                   'AM.mass' = AM.mass,
                   'type' = type)

#Import .txt file and rename columns
tmp <- read.table(paste0(dir.name, "/", filename), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt

#rearrange data.frame
raw <- tmp %>%
  select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
  mutate(time.s = time.s - min(time.s))
colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

#calculate capacities for each cycle
capacity <- Biologic.CAP(raw, AM.mass, type)
#splits CC & CV contributions
CCCV <- Biologic.CCCV(raw, AM.mass, type)
#extract voltage profiles for selected cycles
VPprofiles <- Biologic.VP(raw, AM.mass, cycles, type)

#all data is collected in a list
cyc.dat <- list('cell.data' = meta,
                'capacity' = capacity,
                'VoltageProfiles' = VPprofiles,
                'raw' = raw)
#for a single file this step would actually not be necessary
#However, this additional layer is needed to make the script compatible with the multi .txt import
dat <- list(cyc.dat)

#generate a data report
#set working directory for Rfiles (RMarkdown needs to know where R scripts are located)
Rdir = c('C:/Users/gm5225/Documents/Rprojects/bat2dat/R')
setwd(Rdir)

report(cyc.dat, Rdir, outdir)

#save data for further processing
SaveToOrigin.Stats(outdir, dat, meta)
SaveToOrigin.VP(outdir, dat, meta, cycles)
}
