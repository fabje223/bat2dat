#Script generates a summary of the generated data
#RMarkdown required

report <- function(exp){

          meta <- exp$cell.data
          capacity <- exp$capacity
          VPprofiles <- exp$VoltageProfiles

          if(nrow(capacity)>=1){

            cyc <- cycles[cycles %in% c(0:max(capacity$CycNr))]

            #Plot charge/discharge capacities and CE
            pCap <- plotCapReport(capacity)
            pIRdrop <- plotIRdrop(capacity)

            #Plot voltage profiles for selected cycles
            #uses colour palett 'viridis'
            vp.dat <- data.frame()
            for(j in 1:(length(cyc))){

              tmp <- VPprofiles[[j]]
              vp.dat <- rbind(vp.dat, tmp)

            }
            pVPloop <- plotVPloop(vp.dat)
            pVPlin  <- plotVPlin(vp.dat)

            cell <- meta[1,4]
            pVPsplit  <- plotVPsplit(vp.dat, cell)

            filename <- meta[1,2]
            report.name <- c(paste0("DataReport_", filename, ".html"))

            #Generate Report in Rmarkdown
            rmarkdown::render(
                            "report.RMD",
                            params = list(metavar = meta,
                                          cap = capacity,
                                          plotCap = pCap,
                                          plotIR = pIRdrop,
                                          plotVPloop = pVPloop,
                                          plotVPlin = pVPlin,
                                          plotVPsplit = pVPsplit),
                            output_file = report.name,
                            output_dir = outdir
                          )

          } else if(nrow(cap.dat)==0){
               next
             }

}
