#' Starts evaluation of expeirmental data
#'
#' Depending on instrument different analysis scripts are needed
#'
#' @param raw raw data
#' @param AMmass active material mass in mg
#' @param cellType cell configuration (halfcell, fullcell, etc.)
#' @param cycles cycles to be included in voltage profile analysis
#' @param cccv perform CCCV step analysis? TRUE/FALSE
#' @param warningsLOG a log book for warning messages
#'
#' @return returns a list cycDat with analysed data
#' @export
#'
#' @include Read0r2.R Process0r.R Report0r.R
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @examples
BiologicEvaluat0r <- function(raw, AMmass, cellType, cycles, cccv, warningsLOG){

              cycDat <- list('capacity' = NULL,
                             'VoltageProfiles' = NULL,
                             'CCCV' = NULL)

              #calculate capacities for each cycle
              cycDat$capacity <- Biologic.CAP(raw, AMmass, cellType)

              #extract voltage profiles for selected cycles
              if(length(cycles) != 0){
                cycDat$VoltageProfiles <- Biologic.VP(raw, AMmass, cycles, cellType)
              }

              #CC-CV step analysis is included if TRUE
              if(cccv == TRUE){
                cycDat$CCCV <- Biologic.CCCV(raw, AMmass, cellType, warningsLOG)
              }

              return(cycDat)

              }

#' @describeIn BiologicEvaluat0r Evaluat0r for Arbin Instruments
ArbinEvaluat0r <- function(raw, AMmass, cellType, cycles){

              cycDat <- list('capacity' = NULL,
                             'VoltageProfiles' = NULL,
                             'CCCV' = NULL)

              #calculate capacities for each cycle
              cycDat$capacity <- Arbin.CAP(raw, AMmass, cellType)

              #extract voltage profiles for selected cycles
              cycDat$VPprofiles <- Arbin.VP(raw, AMmass, cycles, cellType)

              return(cycDat)

              }
