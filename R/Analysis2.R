
#Analysis function: Selectes cycles for Voltage Profiles, Capacities, C.E., etc.
analysis <- function(dir, cycles, meta) {

  #loop through selected data
  output <- lapply(1:nrow(meta), function(i) {

    filename <- as.character(meta[i,2])
    AM.mass <- as.numeric(meta[i,5])/1000
    cycler <- as.character(meta[i,3])
    type <- meta[i,4]

    #check if .res files are in directory; if so, rename them to .accdb
    files = list.files(pattern = ".*.res")
    if(length(files) != 0 && cycler == 'Arbin'){
          res <- paste0(filename, ".res")
          newfiles <- gsub(".res$", ".accdb", res)
          file.rename(res, newfiles)
    }

    if(cycler == "Biologic BCS"){

        tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
        raw <- tmp %>%
                select('cycle.number', 'time.s', 'Ns', 'Ecell.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
                mutate(time.s = time.s - min(time.s))
        colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

        #calculate capacities for each cycle
        capacity <- Biologic.CAP(raw, AM.mass, type)
        #extract voltage profiles for selected cycles
        VPprofiles <- Biologic.VP(raw, AM.mass, cycles, type)
        #detailed analysis of CC-CV steps
        CCCV <- Biologic.CCCV(raw, AM.mass, type)

        cyc.dat <- list('cell.data' = meta[i,],
                        'capacity' = capacity,
                        'VoltageProfiles' = VPprofiles,
                        'CCCV' = CCCV,
                        'raw' = raw)

        print("calculate results from BCS data")

      }else if(cycler == "Biologic VMP"){

        tmp <- read.table(paste0(dir, "/", filename, ".txt"), header=T, dec = ",", sep = "\t", fill=TRUE) #.txt
        raw <- tmp %>%
                select('cycle.number', 'time.s', 'Ns', 'Ewe.V', 'X.I..mA', 'Q.discharge.mA.h', 'Q.charge.mA.h') %>%
                mutate(time.s = time.s - min(time.s))
        colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.mA', 'Qdc.mAh', 'Qch.mAh')

        #calculate capacities for each cycle
        capacity <- Biologic.CAP(raw, AM.mass, type)
        #extract voltage profiles for selected cycles
        VPprofiles <- Biologic.VP(raw, AM.mass, cycles, type)
        #detailed analysis of CC-CV steps
        CCCV <- Biologic.CCCV(raw, AM.mass, type)

        cyc.dat <- list('cell.data' = meta[i,],
                        'capacity' = capacity,
                        'VoltageProfiles' = VPprofiles,
                        'CCCV' = CCCV,
                        'raw' = raw)

        print("calculate results from VMP data")

      }else if(cycler == "Arbin") {

        file <- list.files(pattern = filename)

        if(endsWith(file, ".xlsx")){

          if (!requireNamespace("readxl", quietly = TRUE)) {
            stop(
              "Package \"readxl\" must be installed to use this function.",
              call. = FALSE
            )
          }

          fname <- paste0(dir, "/", file)

          # @param filename must end in .xls or xlsx
          l <- lapply(grep("Channel*", excel_sheets(fname), value=TRUE),
                    read_excel, path = fname)
          l <- do.call(rbind,l)

          raw <- l %>%
                  select('Cycle_Index', 'Test_Time(s)', 'Step_Index', 'Voltage(V)', 'Current(A)', 'Charge_Capacity(Ah)', 'Discharge_Capacity(Ah)')
                  #mutate(Test_Time(s) = Test_Time(s) - min(Test_Time(s)))
          colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qdc.Ah', 'Qch.Ah')

        }else if(endsWith(file, ".accdb")){

          if (!requireNamespace("RODBC", quietly = TRUE)) {
            stop(
              "Package \"RODBC\" must be installed to use this function.",
              call. = FALSE
            )
          }

          fname <- paste0(dir, "/", file)

          con <- odbcConnectAccess2007(fname)
          #sqlTables(con, tableType="TABLE")$TABLE_NAME
          raw <- sqlFetch(con, "Channel_Normal_Table")
          odbcCloseAll()

          raw <- raw %>%
                  select('Cycle_Index', 'Test_Time', 'Step_Index', 'Voltage', 'Current', 'Charge_Capacity', 'Discharge_Capacity')

          colnames(raw) = c('cyc.nr', 'time.s', 'Ns', 'Ewe.V', 'I.A', 'Qch.Ah', 'Qdc.Ah')
        }

        #calculate capacities for each cycle
        capacity <- Arbin.CAP(raw, AM.mass)
        #extract voltage profiles for selected cycles
        VPprofiles <- Arbin.VP(raw, cycles)

        cyc.dat <- list('cell.data' = meta[i,],
                        'capacity' = capacity,
                        'VoltageProfiles' = VPprofiles,
                        'CCCV' = CCCV,
                        'raw' = raw)

        print("calculate results from Arbin data")

      }else{
        print("cycler not found - check directory")

        cyc.dat <- list('cell.data' = NULL,
                        'capacity' = NULL,
                        'VoltageProfiles' = NULL,
                        'CCCV' = NULL,
                        'raw' = NULL)
      }

    return(cyc.dat)
  })

  return(output)
}
