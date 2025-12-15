#-----------------------------------------------
# Run no-new-vaccine model and output epi trends
# Rebecca Clark
# Last updated: 16 September 2025
#-----------------------------------------------

rm(list=ls())

# Set-up: Load in the required packages
suppressPackageStartupMessages({
  require(tbmod)
  library(here)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(patchwork)
  #source(here("R", "run_param_set.R"))
  source(here("R", "run_param_set_zero.R"))
  source(here("R", "run_baseline_uncertainty.R"))
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
  
})


######## Set the country code, parameters
cc <- "IND"

# can replace with baseline/high/med/low instead of zero
parameters <- fread("./processing_files/param_sets/zero_plot.csv")


######## Set-up and generate the output ######
total_counts <- list()
total_pops <- list()

for (j in 1:nrow(parameters)) {
  
  print(paste0("parameter set = ", j))
  
  params     <- parameters[j, ]
  params_uid <- params[, uid]
  params     <- params[, !c("uid", "nhits")]
  params     <- unlist(params)
  
  # Run the model with the parameter set
  param_output <- run_param_set_zero(params, params_uid)
  
  total_counts[[j]] <- param_output[["counts"]]
  total_pops[[j]] <- param_output[["pops"]]

}

cc_counts <- rbindlist(total_counts)
cc_pops <- rbindlist(total_pops)


######## Subset the data to get the variables to plot ######## 
n_epi <- run_baseline_uncertainty(cc_counts, cc_pops)

n_epi_long <- melt(n_epi, id.vars = c("Country", "Year", "AgeGrp", "UID", "Runtype"),
                   variable.name = "Indicator")

n_epi_long <- n_epi_long[, `:=`(medval = median(value),
                                lowval = quantile(value, 0.025),
                                highval = quantile(value, 0.975)),
                         by = .(Year, AgeGrp, Indicator, Runtype)]

fwrite(n_epi_long, paste0("./epi_output/no-new-vaccine_output/", cc, "_n_epi_zero.csv"))


######## 



