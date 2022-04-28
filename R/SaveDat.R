######################################################################################################################
# Saving results
######################################################################################################################
#save indiviual files:
#change saving directory if desired
#directory must exist
#setwd(outdir)

#save capacity vs. cycle data
SaveToOrigin.Stats <- function(outdir, exp){
          
          capacity <- exp$capacity
          meta <- exp$cell.data
          filename <- meta$filename

          write.table(capacity, paste0(outdir, "//Stat_", filename, ".txt"), sep="\t", row.names=FALSE)
                
          print(paste0('Cycling data of cell ', filename, ' exported to ', outdir))
}

#save CCCV table
SaveToOrigin.CCCV <- function(outdir, exp){
          
          CCCV <- exp$CCCV
          meta <- exp$cell.data
          filename <- meta$sample.name

          write.table(CCCV, paste0(outdir, "//CCCV_", filename, ".txt"), sep="\t", row.names=FALSE)
          
          print(paste0('Cycling data of cell ', filename, ' exported to ', outdir))
}

#save capacity vs. cycle data
SaveToOrigin.VP <- function(outdir, exp, cycles){
    
    VPprofiles <- exp$VoltageProfiles
    meta <- exp$cell.data
    filename <- meta$sample.name
    
    for(i in 1:length(VPprofiles)){
      
      VP <- VPprofiles[[i]]

          write.table(VP, paste0(outdir, "//VP_", filename, "cycle#", cycles[i], ".txt"), sep="\t", row.names=FALSE)
    }
    
    print(paste0('Voltage profiles of cell ', filename, ' exported to ', outdir))

}

SaveToXlxs <- function(outdir, meta, capacity, VPprofiles){
  
  setwd(outdir)
  write.xlsx(capacity, file = paste0(meta[1,2], ".xlsx"), sheetName = "Stats", append = FALSE)
  
}