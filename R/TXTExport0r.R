#' @title TXTExport0r
#'
#' @description exports analysed data from list 'processedData', i.e. exp = processedData[[sampleID]]
#'
#' @param exp list containing all sample information
#'
#' @return no return; writes a .txt file with respective data into outdir folder
#' @export
#'
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' l <- process0r()
#' exp <- l[[1]]
#' SaveStatsToOrigin(exp)
#' }
SaveStatsToOrigin <- function(exp){

          capacity <- exp$capacity
          meta <- exp$metadata
          filename <- meta$sample.name
          outdir <- meta$outdir

          write.table(capacity, paste0(outdir, "//Stat_", filename, ".txt"), sep="\t", row.names=FALSE)

          print(paste0('Cycling data of cell ', filename, ' exported to ', outdir))
}

#' @describeIn SaveStatsToOrigin save results from CCCV step analysis
SaveCCCVToOrigin <- function(exp){

          CCCV <- exp$CCCV
          meta <- exp$metadata
          filename <- meta$sample.name
          outdir <- meta$outdir

          write.table(CCCV, paste0(outdir, "//CCCV_", filename, ".txt"), sep="\t", row.names=FALSE)

          print(paste0('Cycling data of cell ', filename, ' exported to ', outdir))
}

#' @describeIn SaveStatsToOrigin save results from extracted voltage profiles
SaveVPToOrigin <- function(exp){

    VPprofiles <- exp$VoltageProfiles
    meta <- exp$metadata
    filename <- meta$sample.name
    outdir <- meta$outdir

    for(i in 1:length(VPprofiles)){

      VP <- VPprofiles[[i]]
      cycNr <- unique(VP$CycNr)

          write.table(VP, paste0(outdir, "//VP_", filename, "cycle#", cycNr, ".txt"), sep="\t", row.names=FALSE)
    }

    print(paste0('Voltage profiles of cell ', filename, ' exported to ', outdir))

}

#' @describeIn SaveStatsToOrigin produces .xlsx file from generated data
#SaveToXlxs <- function(outdir, meta, capacity, VPprofiles){

 # write.xlsx(capacity, file = paste0(meta[1,2], ".xlsx"), sheetName = "Stats", append = FALSE)

#}
