run_param_set_scTB <- function(cc, params, params_uid, vx_chars) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- set.paths(countrycode  = cc,
                           xml          = vx_chars$xml,
                           parameters   = "input_zero.csv")
  
  if (grepl("baseline", vx_chars$runtype)){
    scen_baseline = NULL
  } else {
    scen_baseline = model$baseline_output 
  } 
  
  # Run the model with the parameter set
  output = run(model_paths, new.parameter.values = params, baseline = scen_baseline, output.flows = F)
  
  #### n_epi
  if (T){
    counts <- output$stocks
    counts <- counts[age_from == 0 & age_thru == 99, ][,AgeGrp := "[0,99]"]
    counts <- counts[, !c("age_from", "age_thru")]
    
    ### Incidence Dn 
    incDn <- counts[TB == "Dncount",]
    incDn <- incDn[, .(N_incDn = sum(value)), by = .(Country = country, Year = year, AgeGrp)]

    ### Incidence Ds 
    incDs <- counts[TB == "Dscount",]
    incDs <- incDs[, .(N_incDs = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    #### Incidence Dc
    incDc <- counts[TB == "Dccount",]
    incDc <- incDc[, .(N_incDc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    #### Mortality
    mort <- counts[TB == "TBdead",]
    mort <- mort[, .(N_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    
    # Get the population size from the population output
    population <- setDT(output$population)
    population <- population[year >= 2023,][year <= 2051]
    population <- population[year %% 1 == 0.5,] # use the average of the year for the stocks
    population <- melt(population, id.vars = c("year", "country"),
                       variable.name = "age", value.name = "Population")
    population$age = as.integer(as.character(population$age))
    
    pop_all <- setDT(population)
    pop_all <- pop_all[, `:=`(N_pop = sum(Population)), by = .(year)]
    pop_all <- pop_all[, .(Year = year, N_pop, Country = "IND")]
    pop_all <- unique(pop_all)
    pop_all <- pop_all[, AgeGrp := "[0,99]"][, Year := floor(Year)]

    
    # Combine everything into one dataset
    n_epi <- incDc[incDs, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[incDn, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[, Year := floor(Year)]
    n_epi <- n_epi[pop_all, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    
    rm(counts)
    rm(incDn,incDs, incDc)
    rm(mort)

        
    # Add the vaccine characteristics
    combined_ipj[["n_epi"]] <- n_epi[, `:=`(uid     = params_uid,
                                            runtype = vx_chars$runtype)]
    
  }
  
  if (grepl("baseline", vx_chars$runtype)){
    model$baseline_output <- output
  }
  
  rm(output)
  combined_ipj
}