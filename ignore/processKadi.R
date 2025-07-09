

processKadi <- function(meta, cccv = FALSE, cycles = c(seq(0, 99, 10)) ) {

  #creates log for warning messages
  warningsLOG <- data.frame(
    "script" = character(),
    "section" = character(),
    "message" = character()
  )

  #initializing data.frames() for raw and processed raw data
  raw <- data.frame()
  rawEval <- data.frame()

  #read-in raw data from folder
  sampleSUMMARY <- lapply(1:nrow(meta), function(i) {

    if(meta$instrument[i] == "Biologic BCS"){

      print("Reading BCS raw data file")

      raw <- BCSraw(meta$dir[i], meta$sample.name[i])

      rawEval <- BiologicEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i],
                                   cycles, cccv, warningsLOG)

    }else if(meta$instrument[i] == "Biologic VMP"){

      print("Reading VMP raw data file")
      raw <- VMPraw(meta$dir[i], meta$sample.name[i])

      rawEval <- BiologicEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles, cccv)

    }else if(meta$instrument[i] == "Arbin") {

      print("Reading Arbin raw data file")
      #path/to/file/filename.res --> check is .res file in directory
      res <- paste0(meta$dir[i], "/", meta$sample.name[i], ".res")
      accdb <- paste0(meta$dir[i], "/", meta$sample.name[i], ".accdb")
      xlsx <- paste0(meta$dir[i], "/", meta$sample.name[i], ".xlsx")

      if(file.exists(xlsx)){

        f.path <- paste0(meta$dir[i], "/", meta$sample.name[i], ".xlsx")
        raw <- ARBINrawXLSX(dir, f.path)
        rawEval <- ArbinEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles)

        #check if file has .res ending; if so, rename them to .accdb
      }else if(file.exists(res)){

        print('.res interpreter currently not functional')
        raw <- NULL

        #newfile <- gsub(".res$", ".accdb", res)
        #file.rename(res, newfile)
        #raw <- ARBINrawACCDB(newfile)

      }else if(file.exists(accdb)){

        raw <- NULL
        rawEval <- NULL
        #raw <- ARBINrawACCDB(accdb)
        #rawEval <- ArbinEvaluat0r(raw, meta$AM.loading[i], meta$cell.config[i], cycles)
      }


    }else{

      print("cycler not found - check directory")

      raw <- NULL
      rawEval <- NULL
    }

    l.sample <- list("metadata" = meta[i,],
                     "rawdata" = raw,
                     "capacity" = rawEval$capacity,
                     "VoltageProfiles" = rawEval$VoltageProfiles,
                     "CCCV" = rawEval$CCCV)

    print(paste0('Data analysis of file ', meta$sample.name[i], " finished"))

    return(l.sample)
  })

  return(sampleSUMMARY)
}
