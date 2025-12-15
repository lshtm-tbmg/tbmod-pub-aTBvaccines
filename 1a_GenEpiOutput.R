#-----------------------------------------------
# Generate Epi Output
# Rebecca Clark
# Updated December 2025
#-----------------------------------------------

# 1. Set-up: 
# 1.1  Load in the required packages
suppressPackageStartupMessages({
  rm(list=ls())
  model = new.env()
  library(XML)
  library(here)
  require(tbmod)
  library(arrow)
  require(tidyverse)
  library(data.table)
  source(here("R", "run_param_set_aTB.R"))
})

tbmod::modelversion()

# 1.2  Set the country code, parameters, scenario characteristics 
cc <- "IND"

#grid_task_int <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))  
grid_task_int <- 1 #commented this out on cluster and uncomment line above

# Replace vx_scenario file to run different scenarios 
vx_scenarios  <- fread("./processing_files/vx_scenario_all.csv")

# Replace parameter file with high/med/low/zero to run differen parameter sets
parameters    <- fread(paste0("./processing_files/param_sets/baseline_final.csv"))
print(paste0("number of parameter sets to run = ", nrow(parameters)))

if (grid_task_int == 1){
  dir.create("./epi_output/")
  dir.create("./epi_output/n_epi_hmer/")
}

# 3. Set-up and generate the output
for (j in 1:nrow(parameters)) {
  
  print(paste0("parameter set = ", j))
  
  params     <- parameters[j, ]
  params_uid <- params[, uid]
  
  if (paste0(params_uid, ".parquet") %in% list.files("epi_output/n_epi_baseline/")){
    print("Done param set!")
  } else {
    
    params     <- params[, !c("uid", "nhits")]
    params     <- unlist(params)
    
    cc_n_epi_param <- list()
    
    for (i in 1:nrow(vx_scenarios)){
      
      vx_chars <- vx_scenarios[i,]
      
      print(paste0("Running scenario number ", i, ": ", vx_chars$runtype))
      
      # run the model with the row of parameters
      vx_scen_output <- run_param_set_scTB(cc, params, params_uid, vx_chars)
      
      cc_n_epi_param[[i]] <- vx_scen_output[["n_epi"]]
      
    }
    
    write_parquet(rbindlist(cc_n_epi_param), paste0("./epi_output/n_epi_baseline/", params_uid, ".parquet"))
    rm(cc_n_epi_param)
    
    print(paste0("End time for parameter set ", j, " = ", Sys.time()))
    
  }
}

# ----end

