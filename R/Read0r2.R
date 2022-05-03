#' @title Read0r2.R
#'
#' @description Imports experimental data from data folders, depending on data format/instrument.
#' Experimental data is converted into a uniform format for further processing
#'
#' @param dir directory to raw data folder
#' @param filename sample name
#'
#' @return returns data.frame with raw data
#' @export
#'
#' @include Process0r.R Report0r.R
#' @importFrom utils read.table
#' @importFrom magrittr %>%
#' @name %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @import readxl RODBC
#'
#' @examples
BCSraw <- function(dir, filename){

  #binding variables locally to function BCSraw()
  time.s <- NULL

  tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
  raw <- tmp %>%
    select('cycle.number', 'time.s', 'Ns', 'Ecell.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
    mutate(time.s = time.s - min(time.s))
  colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

  return(raw)

}

#' @describeIn BCSraw read .txt files from raw data directory
VMPraw <- function(dir, filename){

  #binding variables locally to function VMPraw()
  time.s <- NULL

  tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
  raw <- tmp %>%
    select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
    mutate(time.s = time.s - min(time.s))
  colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

  return(raw)

  }

#' @describeIn BCSraw read .txt files from raw data directory
ARBINrawXLSX <- function(dir, filename){

    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package \"readxl\" must be installed to use this function.",
        call. = FALSE
      )
    }

    f.dir <- paste0(dir, "/", filename, ".xlsx")

    # @param filename must end in .xls or xlsx
    l <- lapply(grep("Channel*", excel_sheets(f.dir), value=TRUE),
                read_excel, path = f.dir)
    l <- do.call(rbind,l)

    raw <- l %>%
      select('Cycle_Index', 'Test_Time(s)', 'Step_Index', 'Voltage(V)', 'Current(A)', 'Charge_Capacity(Ah)', 'Discharge_Capacity(Ah)')
    #mutate(Test_Time(s) = Test_Time(s) - min(Test_Time(s)))
    colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qdc.Ah', 'Qch.Ah')

    return(raw)
  }

#' @describeIn BCSraw read .txt files from raw data directory
ARBINrawACCDB <- function(dir, filename){

    if (!requireNamespace("RODBC", quietly = TRUE)) {
      stop(
        "Package \"RODBC\" must be installed to use this function.",
        call. = FALSE
      )
    }

    #system.file() required???
    errorRODBC <- tryCatch(
                          expr = {
                                  con <- RODBC::odbcConnectAccess2007(filename)
                          },
                          error = function(e) e
                  )

    if(inherits(errorRODBC, "simpeError")){

        print(paste0("Error: could not access odbcConnectAccess2007. Use .xlsx file instead."))
        raw <- NULL

    }else if(!inherits(errorRODBC, "simpeError")){
        #sqlTables(con, tableType="TABLE")$TABLE_NAME
        raw <- RODBC::sqlFetch(con, "Channel_Normal_Table")
        RODBC::odbcCloseAll()

        raw <- raw %>%
          select('Cycle_Index', 'Test_Time', 'Step_Index', 'Voltage', 'Current', 'Charge_Capacity', 'Discharge_Capacity')

        colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qch.Ah', 'Qdc.Ah')
    }

    return(raw)
}
