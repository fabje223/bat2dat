#' Title
#'
#' @param raw
#' @param AMmass
#' @param cellType
#' @param cycles
#' @param cccv
#' @param warningsLOG
#'
#' @return
#' @export
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

#' Title
#'
#' @param raw
#' @param AMmass
#' @param cellType
#' @param cycles
#'
#' @return
#' @export
#'
#' @examples
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
