run_param_set_scTB <- function(cc, params, params_uid, vx_chars) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- set.paths(countrycode  = cc,
                           xml          = vx_chars$xml,
                           parameters   = "input.csv")
  
  if (grepl("baseline", vx_chars$runtype)){
    scen_baseline = NULL
  } else {
    scen_baseline = model$baseline_output 
  } 
  
  # Run the model with the parameter set
  output = run(model_paths, new.parameter.values = params, baseline = scen_baseline, output.flows = F)
  
  #### n_epi
  counts <- output$stocks
  
  # Add the vaccine characteristics
  combined_ipj[["n_epi"]] <- counts[, `:=`(uid     = params_uid,
                                          runtype = vx_chars$runtype)]
  
if (grepl("baseline", vx_chars$runtype)){
  model$baseline_output <- output
}

rm(output)
combined_ipj
}