run_baseline_uncertainty <- function(cc_counts, cc_pops){
  
  cc_pops <- cc_pops[, Year := floor(year)]
  cc_pops <- cc_pops[, .(Year, UID, Runtype, AgeGrp, Population = sum_val)]
  cc_pops <- cc_pops[Year >= 2000 & Year < 2031]
  
  cc_counts <- cc_counts[VXa != "recvcount"]
  
  #### Incidence
  inc <- cc_counts[TB == "Dccount",]
  inc <- inc[, .(N_inc = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  #### Mortality
  mort <- cc_counts[TB == "TBdead",]
  mort <- mort[, .(N_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  #### Treatment
  treat <- cc_counts[TB == "DcTcount"]
  treat <- treat[, .(N_tx = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  #### Infectious disease prevalence
  prev <- cc_counts[TB == "Ds" | TB == "Dc"]
  prev <- prev[, .(N_prev = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  #### TB infection prevalence
  inf_prev <- cc_counts[TB == "I"]
  inf_prev <- inf_prev[, .(N_infprev = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  #### Symptomatic TB prevalence
  sTB_prev <- cc_counts[TB == "Dc"]
  sTB_prev <- sTB_prev[, .(N_sTBprev = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  #### Asymptomatic TB prevalence
  aTB_prev <- cc_counts[TB == "Ds"]
  aTB_prev <- aTB_prev[, .(N_aTBprev = sum(value)), by = .(Country = country, Year = year, AgeGrp, UID, Runtype)]
  
  n_epi <- inc[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  n_epi <- n_epi[treat, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  n_epi <- n_epi[prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  n_epi <- n_epi[inf_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  n_epi <- n_epi[sTB_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  n_epi <- n_epi[aTB_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  
  n_epi <- n_epi[, Year := floor(Year)][Year < 2031]
  
  n_epi <- n_epi[cc_pops,  on = .(Year = Year, AgeGrp = AgeGrp, UID = UID, Runtype = Runtype)]
  
  n_epi <- n_epi[, inc_rate := (N_inc*0.86)/Population*100000]
  n_epi <- n_epi[, mort_rate := N_mort/Population*100000]
  n_epi <- n_epi[, tx_rate := N_tx/Population*100000]
  n_epi <- n_epi[, prev_rate := (N_prev/0.88)/Population*100000]
  n_epi <- n_epi[, inf_prev_prop := N_infprev/Population]
  n_epi <- n_epi[, aTB_prop := N_aTBprev/N_prev]
  
  n_epi
  
}