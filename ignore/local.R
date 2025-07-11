devtools::load_all()

cycles <- c(1:10)

samples <- process0r(cycles=cycles)
VPprofiles <- Biologic.VP(raw = samples[[1]]$rawdata,
                          AMmass = as.numeric(samples[[1]]$metadata[5]),
                          cycles = cycles,
                          cellType = samples[[1]]$metadata[4])

vp.dat <- VPprofiles[[1]]
