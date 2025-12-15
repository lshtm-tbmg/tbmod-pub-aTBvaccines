# ---------------------------
# Process Epi Output
# Rebecca Clark
# Updated December 2025
# ---------------------------

suppressPackageStartupMessages({
  rm(list=ls(all=TRUE))
  library(here)
  library(data.table)
  library(arrow)
  library(dplyr)
})

# Create a folder to save the output
dir.create("./epi_output/grouped_output/")

# Open all the parquet files that you generated together
# Note: replace "n_epi_hmer" with high/low/med/zero for whichever parameter sets
# you want to generate for
n_epi <- open_dataset(sources = paste0("./epi_output/n_epi_baseline/")) %>% dplyr::collect()


#------------------ Total number of cases and deaths 
total <- n_epi
total <- total[Year <=2050]
total <- total[, N_incDsDc := N_incDs + N_incDc]
total <- total[,`:=`(N_incDs = sum(N_incDs),
                     N_incDsDc = sum(N_incDsDc),
                     N_incDc = sum(N_incDc),
                     N_incDn = sum(N_incDn),
                     N_mort = sum(N_mort),
                     N_pop = sum(N_pop)),
               by = .(Year, runtype, uid)]


total <- total[, .(Year, Runtype = runtype, UID = uid, N_incDs, N_incDc, N_incDsDc, N_incDn, N_mort, N_pop)]
total <- unique(total)

total_long <- melt(total, measure.vars = c("N_incDs","N_incDc", "N_incDsDc", "N_incDn", "N_mort", "N_pop"), 
                   id.vars = c("Year", "Runtype", "UID"),
                   value.name = "Value", variable.name = "Indicator")

total_long <- total_long[, .(medval = median(Value),
                             lowval = quantile(Value, 0.025),
                             highval = quantile(Value, 0.975)),
                         by = .(Year, Runtype, Indicator)]

fwrite(total_long, "./epi_output/grouped_output/total_numbers_baseline.csv")

#------------------ Rate reductions
rate_plots <- total[Year >= 2030] 
rate_plots <- rate_plots[, `:=`(incDc_rate = (N_incDc/N_pop)*(100000),
                                incDs_rate = (N_incDs/N_pop)*(100000),
                                incDn_rate = (N_incDn/N_pop)*(100000),
                                mort_rate = (N_mort/N_pop)*(100000)),
                         by = .(Year, Runtype, UID)]

rate_red_long <- melt(rate_plots, measure.vars = c("incDc_rate",
                                                   "incDs_rate",
                                                   "incDn_rate",
                                                   "mort_rate"), 
                      id.vars = c("Year", "Runtype", "UID"),
                      value.name = "Value", variable.name = "Indicator")

# Cast to wide form to calculate the rate reductions
rate_red_wide <- dcast(rate_red_long, Year + UID + Indicator ~ Runtype,
                       value.var = "Value")

# Calculate the rate reductions (scenario compared to baseline)
vx_scenarios <- unique(rate_plots$Runtype)
vx_scenarios <- vx_scenarios[vx_scenarios != "baseline"]

for (scen in vx_scenarios) {
  set(x = rate_red_wide, j = paste0(scen, "_diff"), value = rate_red_wide[["baseline"]] - rate_red_wide[[paste0(scen)]])
  set(x = rate_red_wide, j = paste0("PER_", scen), value = rate_red_wide[[paste0(scen, "_diff")]] / rate_red_wide[["baseline"]])
}

# Melt back to long form with the rate reduction variable
rate_reductions <- melt(data = rate_red_wide, measure.vars = patterns("^PER.*"), 
                        id.vars = c("UID", "Year", "Indicator"),
                        value.name = "Value", variable.name = "Runtype")

rate_reductions <- rate_reductions[Indicator == "incDc_rate", Indicator := "incDc_RR"]
rate_reductions <- rate_reductions[Indicator == "incDs_rate", Indicator := "incDs_RR"]
rate_reductions <- rate_reductions[Indicator == "incDn_rate", Indicator := "incDn_RR"]
rate_reductions <- rate_reductions[Indicator == "mort_rate", Indicator := "mort_RR"]

# Calculate the median, upper, and lower bounds (Uncertainty from the UIDs)
rate_reductions <- rate_reductions[, .(medval = median(Value),
                                       lowval = quantile(Value, 0.025),
                                       highval = quantile(Value, 0.975)),
                                   by = .(Year, Runtype, Indicator)]

rate_reductions <- rate_reductions[, combined := paste0(round(medval*100, 1),
                                                        "% (", round(lowval*100, 1),
                                                        ", ", round(highval*100, 1), ")")]

rate_reductions$Runtype <- gsub("PER_", "", as.character(rate_reductions$Runtype))

fwrite(rate_reductions, "./epi_output/grouped_output/rate_reductions_baseline.csv")


#------------------ Cumulative numbers by scenario
cumulative_sum <- total[Year >= 2030]
cumulative_sum <- cumulative_sum[, .(Year, Runtype, UID, N_incDc, N_incDs, N_incDsDc, N_incDn, N_mort)]
cumulative_sum <- unique(cumulative_sum)

cumulative_sum <- cumulative_sum[, `:=`(sum_incDc = cumsum(N_incDc),
                                        sum_incDs = cumsum(N_incDs),
                                        sum_incDsDc = cumsum(N_incDsDc),
                                        sum_incDn = cumsum(N_incDn),
                                        sum_mort = cumsum(N_mort)),
                                 by = .(Runtype, UID)]

cumulative_sum <- unique(cumulative_sum)

cumulative_epi_long <- melt(cumulative_sum, measure.vars = c("sum_incDc",
                                                             "sum_incDs",
                                                             "sum_incDsDc",
                                                             "sum_incDn",
                                                             "sum_mort"), 
                            id.vars = c("Year", "Runtype", "UID"),
                            value.name = "Value", variable.name = "Indicator")

cumulative_epi_wide <- dcast(cumulative_epi_long, Year + UID + Indicator ~ Runtype,
                             value.var = "Value")

cumulative_sum_total <- melt(data = cumulative_epi_wide, 
                             id.vars = c("Year", "UID", "Indicator"),
                             value.name = "Value", variable.name = "Runtype")

cumulative_sum_total <- cumulative_sum_total[, .(medval = median(Value),
                                                 lowval = quantile(Value, 0.025),
                                                 highval = quantile(Value, 0.975)),
                                             by = .(Year, Runtype, Indicator)]

cumulative_sum_total <- cumulative_sum_total[, combined := paste0(round(medval*1000/1e6, 1),
                                                                  " (", round(lowval*1000/1e6, 1),
                                                                  ", ", round(highval*1000/1e6, 1), ") million")]

cumulative_sum_total <- cumulative_sum_total[Indicator == "sum_incDc",  Indicator := "cumulative_incDc"]
cumulative_sum_total <- cumulative_sum_total[Indicator == "sum_incDs",  Indicator := "cumulative_incDs"]
cumulative_sum_total <- cumulative_sum_total[Indicator == "sum_incDsDc",  Indicator := "cumulative_incDsDc"]
cumulative_sum_total <- cumulative_sum_total[Indicator == "sum_incDn",  Indicator := "cumulative_incDn"]
cumulative_sum_total <- cumulative_sum_total[Indicator == "sum_mort", Indicator := "cumulative_mort"]

fwrite(cumulative_sum_total, "epi_output/grouped_output/cumulative_numbers_baseline.csv")



#------------------ Cumulative proportion difference in sTB and deaths

for (scen in vx_scenarios) {
  set(x = cumulative_epi_wide, j = paste0("Diff_", scen), value = cumulative_epi_wide[["baseline"]] - cumulative_epi_wide[[paste0(scen)]])
  set(x = cumulative_epi_wide, j = paste0("PER_", scen), value = cumulative_epi_wide[[paste0("Diff_", scen)]] / cumulative_epi_wide[["baseline"]])
}

cumulative_diff <- melt(data = cumulative_epi_wide, measure.vars = patterns("^PER.*"), 
                        id.vars = c("Year", "UID", "Indicator"),
                        value.name = "Value", variable.name = "Runtype")

cumulative_diff <- cumulative_diff[Indicator == "sum_incDc",  Indicator := "prop_incDc_diff"]
cumulative_diff <- cumulative_diff[Indicator == "sum_incDs",  Indicator := "prop_incDs_diff"]
cumulative_diff <- cumulative_diff[Indicator == "sum_incDsDc",  Indicator := "prop_incDsDc_diff"]
cumulative_diff <- cumulative_diff[Indicator == "sum_incDn",  Indicator := "prop_incDn_diff"]
cumulative_diff <- cumulative_diff[Indicator == "sum_mort", Indicator := "prop_mort_diff"]

cumulative_diff <- cumulative_diff[, .(medval = median(Value),
                                       lowval = quantile(Value, 0.025),
                                       highval = quantile(Value, 0.975)),
                                   by = .(Year, Runtype, Indicator)]

cumulative_diff <- cumulative_diff[, combined := paste0(round(medval*-100, 1),
                                                        " (", round(highval*-100, 1),
                                                        ", ", round(lowval*-100, 1), ")")]

cumulative_diff$Runtype <- gsub("PER_", "", as.character(cumulative_diff$Runtype))

fwrite(cumulative_diff, "epi_output/grouped_output/cumulative_proportion_difference_baseline.csv")


#------------------ Cumulative number differences in sTB and deaths
cumulative_num_diff <- melt(data = cumulative_epi_wide, measure.vars = patterns("^Diff.*"), 
                            id.vars = c("Year", "UID", "Indicator"),
                            value.name = "Value", variable.name = "Runtype")

cumulative_num_diff <- cumulative_num_diff[Indicator == "sum_incDc",  Indicator := "cumulative_incDc_diff"]
cumulative_num_diff <- cumulative_num_diff[Indicator == "sum_incDs",  Indicator := "cumulative_incDs_diff"]
cumulative_num_diff <- cumulative_num_diff[Indicator == "sum_incDsDc",  Indicator := "cumulative_incDsDc_diff"]
cumulative_num_diff <- cumulative_num_diff[Indicator == "sum_incDn",  Indicator := "cumulative_incDn_diff"]
cumulative_num_diff <- cumulative_num_diff[Indicator == "sum_mort", Indicator := "cumulative_mort_diff"]

cumulative_num_diff <- cumulative_num_diff[, .(medval = median(Value),
                                               lowval = quantile(Value, 0.025),
                                               highval = quantile(Value, 0.975)),
                                           by = .(Year, Runtype, Indicator)]

cumulative_num_diff <- cumulative_num_diff[, combined := paste0(round(medval/1000, 3),
                                                                " (", round(lowval/1000, 3),
                                                                ", ", round(highval/1000, 3), ")")]

cumulative_num_diff$Runtype <- gsub("Diff_", "", as.character(cumulative_num_diff$Runtype))

fwrite(cumulative_num_diff, "epi_output/grouped_output/cumulative_numbers_difference_baseline.csv")

# --- end


