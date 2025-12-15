run_param_set <- function(params, params_uid){
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- set.paths(countrycode  = "IND",
                           xml          = "XMLinput_NH_baseline_plot.xml",
                           parameters   = "input.csv")
  
  # Run the model with the parameter set
  output = run(model_paths, new.parameter.values = params, output.flows = F, baseline = NULL)

  counts <- output$stocks
  counts <- counts[age_from == 0 & age_thru == 99, AgeGrp := "[0,99]"]
  counts <- counts[age_from == 0 & age_thru == 14, AgeGrp := "[0,14]"]
  counts <- counts[age_from == 15 & age_thru == 99, AgeGrp := "[15,99]"]
  counts <- counts[, !c("age_from", "age_thru")]
  counts <- counts[!(year %% 0.5 == 0),]
  
  combined_ipj[["counts"]] <- counts[, UID := params_uid][, Runtype := "Baseline"]
  
  population <- setDT(output$population)
  population <- population[year >= 1999,]
  population <- population[year %% 1 == 0.5,] # use the average of the year for the stocks
  population <- melt(population, id.vars = c("year", "country"),
                     variable.name = "age", value.name = "Population")
  population$age = as.integer(as.character(population$age))
  
  pop_child <- setDT(population)
  pop_child <- pop_child[age < 15]
  pop_child <- pop_child[, `:=`(sum_val = sum(Population)), by = .(year)]
  pop_child <- pop_child[, .(year, sum_val)]
  pop_child <- unique(pop_child)
  pop_child <- pop_child[, AgeGrp := "[0,14]"]
  
  pop_adult <- setDT(population)
  pop_adult <- pop_adult[age >= 15]
  pop_adult <- pop_adult[, `:=`(sum_val = sum(Population)), by = .(year)]
  pop_adult <- pop_adult[, .(year, sum_val)]
  pop_adult <- unique(pop_adult)
  pop_adult <- pop_adult[, AgeGrp := "[15,99]"]
  
  pop_all <- setDT(population)
  pop_all <- pop_all[, `:=`(sum_val = sum(Population)), by = .(year)]
  pop_all <- pop_all[, .(year, sum_val)]
  pop_all <- unique(pop_all)
  pop_all <- pop_all[, AgeGrp := "[0,99]"]
  
  pop <- rbind(pop_child, pop_adult)
  pop <- rbind(pop, pop_all)  
  
  combined_ipj[["pops"]] <- pop[, UID := params_uid][, Runtype := "Baseline"]
  
  combined_ipj
}
