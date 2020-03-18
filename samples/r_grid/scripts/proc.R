library(jsonlite)
library(dplyr)
library(qmap)

# Read config file
config.file <- "bc.json"
config <- fromJSON(config.file)

source(paste0(config$scriptDir, "/", "helper.R"))
source(paste0(config$scriptDir, "/", "bias_correct.R"))

for (site in config$sites) {
  for (var.name in config$vars) {
    for (gcm in config$gcms) {
      # Get file IDs
      file.ids <- list.files(paste0(GetDir(site, "mod"), "/hist"),
                             paste0(gcm, "_", var.name),
                             full.names = T) %>%
        lapply(function(x) {
          tools::file_path_sans_ext(unlist(strsplit(basename(x), "_"))[3])
        }) %>%
        unlist()
      
      for (file.id in file.ids) {
        # Read observed data
        obs <- ReadObs(site, var.name, file.id)
        
        # Read modelled data
        mod <- ReadMod(site, gcm, var.name, file.id)
        
        # Merge observed and modelled data
        dat <-
          merge(obs,
                mod,
                by = c("year", "month", "day"),
                all.y = T) %>%
          dplyr::select(year:day, exp, obs = val.x, mod = val.y)
        
        # Set data for fitting
        dat$fit <- dat$exp == "hist"
        # dat$fit <- dat$exp=="hist" & dat$year <= 1990
        
        # Set grouping for Li method
        if (config$bcMethod == "Li") {
          dat$grp <- dat$exp
        }
        
        # Logic to use the wet day option
        wet.day <- var.name == "pr"
        
        # Apply bias correction by month
        out.dat <- dat %>%
          dplyr::group_by(month) %>%
          dplyr::do(BiasCorrect(., method = config$bcMethod, wet.day = wet.day)) %>%
          dplyr::arrange(year, month, day) %>%
          dplyr::ungroup()
        
        # Write output
        WriteBC(out.dat, site, gcm, var.name, file.id)
        
      }
    }
  }
}
