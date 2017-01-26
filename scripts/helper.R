library(dplyr)

# Get complete directory path
GetDir <- function(site, type) {
  input.dir <- config$inputDir
  if (type == "obs") {
    path <- config$obsDir
  } else if (type == "mod") {
    path <- config$modDir
  } else if (type == "bc") {
    path <- config$bcDir
  }
  
  return(paste(input.dir, path, site, sep="/"))
}

ReadObs <- function(site, var.name, file.id) {
  obs.dir <- GetDir(site, "obs")
  
  suff <- paste0(var.name,"_", file.id)
  obs <- read.csv(list.files(obs.dir, suff, full.names=T))
  obs$val[obs$val==-99.9] <- NA
  
  return(obs)
}

ReadMod <- function(site, gcm, var.name, file.id) {
  mod.dir <- GetDir(site, "mod")
  file.name <- paste0(gcm, "_", var.name, "_", file.id, ".csv")
  
  mod <- lapply(
    setNames(
      config$experiments, config$experiments),
    function(exp){
      read.csv(paste0(mod.dir, "/", exp, "/", file.name))
    }) %>%
    dplyr::bind_rows(.id="exp")
  
  return(mod)
}

WriteBC <- function(out.dat, site, gcm, var.name, file.id) {
  bc.dir <- GetDir(site, "bc")
  if(!dir.exists(bc.dir)) dir.create(bc.dir, recursive=T)
  
  file.name <- paste0(gcm, "_", var.name, "_", file.id, ".csv")
  print(file.name)
  
  for (e in config$experiments) {
    
    out.dir <- paste0(bc.dir,"/", e)
    if(!file.exists(out.dir)) dir.create(out.dir)
    
    print(paste0(out.dir, "/", file.name))
    
    bc <- out.dat %>%
      dplyr::filter(exp == e) %>%
      dplyr::ungroup() %>%
      dplyr::select(year:day,val=round(bc,4))
    
    write.table(bc, paste0(out.dir, "/", file.name), sep=",", row.names=F, col.names=T, quote=F, na="")
    
  }
}