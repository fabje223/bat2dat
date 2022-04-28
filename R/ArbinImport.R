#RODBC package requires R version 4.0 or higher
library(RODBC)
library(dplyr)
library(ggplot2)

file.dir <- file.choose()
dat.name <- basename(file.dir)
dir.name <- dirname(file.dir)

setwd(dir.name)
files = list.files(pattern = ".*.res")

if(length(files)!=0){
      #rename file extension from .res to .accdb
      newfiles <- gsub(".res$", ".accdb", files)
      file.rename(files, newfiles)
  }

accdb = list.files(pattern = ".*.accdb")

#Access & R Versions need to match. 64-bit R --> install AccessDatabaseEngine_x64.exe
#myConn <-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/[P A T H]")

#AM.masses --> manual
mass <- c(3.29, 3.34, 3.81, 3.94, 4.10, 4.12)

#export .accdb data in directory into a R list()
ldat <- list()
for(i in 1:length(accdb)){
      
      fname <- paste0(dir.name,"/",accdb[i])
      
      con <- odbcConnectAccess2007(fname)
      #sqlTables(con, tableType="TABLE")$TABLE_NAME
      cyc <- sqlFetch(con, "Channel_Normal_Table")
      odbcCloseAll()
      
      cyc <- cyc %>%       
                select('Cycle_Index', 'Test_Time', 'Step_Index', 'Voltage', 'Current', 'Charge_Capacity', 'Discharge_Capacity')
     
      colnames(cyc) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qch.mAh', 'Qdc.mAh')
      
      meta <- data.frame('experiment'=accdb[i],
                         'battery.cycler'='Arbin',
                         'AM.mass' = mass[i],
                         'electrolyte'='750mMLPF6, EC:DEC (v/v=1:1)'
                        )
      
      ldat[[i]] <- list('meta'=meta,
                        'raw'=cyc)
}
rm(cyc)
rm(meta)


analysis <- function(ldat) {
  
        #loop through selected data
        output <- lapply(1:length(ldat), function(i) {
      
                    tmp <- ldat[[i]]$raw
                    AM.mass <- ldat[[i]]$meta[[3]]
                    cycler <- ldat[[i]]$meta[[2]]
                    
                    cycles <- c(0,4,9,24,49,74)
                    
                    #calculate capacities for each cycle
                    capacity <- Arbin.CAP(tmp, AM.mass)
                    #extract voltage profiles for selected cycles
                    VPprofiles <- Arbin.VP(tmp, cycles)
                    
                    cyc.dat <- list('cell.data' = ldat[[i]]$meta,
                                    'capacity' = capacity, 
                                    'VoltageProfiles' = VPprofiles)
                    return(cyc.dat)
        })
        
        return(output)
      }

arbin.dat <- analysis(ldat)

SaveToOrigin(dir.name, arbin.dat)

  cyc1 <- cyc %>%
            filter(cyc.nr == 5) %>%
            mutate(time.s = time.s-min(time.s))
  
  p <- ggplot(cyc1) +
        geom_line(aes(x=time.s, y=Ewe.V), color='blue', size=2)
  p