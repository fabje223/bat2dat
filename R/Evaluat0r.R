#' @title Evaluat0r
#'
#' @description Starts evaluation of experimental data.
#' Depending on instrument different analysis scripts are needed
#'
#' @param raw raw data
#' @param AMmass active material mass in mg
#' @param cellType cell configuration (halfcell, fullcell, etc.)
#' @param cycles cycles to be included in voltage profile analysis
#' @param warningsLOG a log book for warning messages
#'
#' @return returns a list cycDat with analysed data
#'
#' @include Read0r2.R Process0r.R Report0r.R Biologic_Analysis.R Arbin_Analysis.R
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' BiologicEvaluat0r()
#' }

#' @export
#' @rdname Evaluat0r
#' @details Evaluat0r for Biologic Instruments
BiologicEvaluat0r <- function(raw, AMmass, cellType, cycles, warningsLOG){

              cycDat <- list('capacity' = NULL,
                             'VoltageProfiles' = NULL
                             )

              #calculate capacities for each cycle
              cycDat$capacity <- Biologic.CAPA(raw, AMmass, cellType)

              #extract voltage profiles for selected cycles
              if(length(cycles) != 0){
                cycDat$VoltageProfiles <- Biologic.VP(raw, AMmass, cycles, cellType)
              }

              return(cycDat)

            }

#' @export
#' @rdname Evaluat0r
#' @details Evaluat0r for Arbin Instruments
ArbinEvaluat0r <- function(raw, AMmass, cellType, cycles){

              cycDat <- list('capacity' = NULL,
                             'VoltageProfiles' = NULL,
                             'CCCV' = NULL)

              #calculate capacities for each cycle
              cycDat$capacity <- Arbin.CAPA(raw, AMmass, cellType)

              #extract voltage profiles for selected cycles
              cycDat$VoltageProfiles <- Arbin.VP(raw, AMmass, cellType, cycles)

              return(cycDat)

              }
