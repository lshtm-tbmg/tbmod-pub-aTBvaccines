# -------------------------
# Results for supplementary material
# Single Age plot showing proportions vaccinated over time in age categories: 15, 25 and 40 year old  
# 18 September 2025
# Rebecca Clark & Hira Tanvir
# -------------------------

suppressPackageStartupMessages({
  rm(list=ls())
  library(here)
  library(data.table)
  library(renv)
  library(arrow)
  library(magrittr)
  library(ggplot2)
  library(tidyverse)
  library(stringr)
  library(viridis)
})

n_epi <- open_dataset(sources = paste0("epi_output/n_epi_singleages/")) %>% dplyr::collect()
n_epi <- setDT(n_epi)

# subset AgeGrp 15, 25 and 40 year olds
n_epi <- n_epi[age_from %in% c(15,25,40)]  


#1. Plot the proportion of the population (in each state) that is protected over time 2030-2050
#b. Protected states indicated by VXa = vac1, vac2 

#b1. All scenarios
n_epi_states <- n_epi[TB %in% c("S","C","I","Dn","Ds","Dc","T","R","Rt")]
n_epi_states <- n_epi_states[VXa != "recvcount"]

n_epi_baseline <- n_epi_states[runtype == "baseline"]

# Reshape from long to wide
nepi_wide <- dcast(n_epi_states, runtype + year + age_from + age_thru + VXa ~ TB, value.var = "value")
nepi_wide <- unique(nepi_wide)

nepi_wide[, `:=`(
  AgeGrp = paste0("[", age_from, ",", age_thru, "]")
)]


# Recode the VXa column and summarize
nepi_wide_modified <- copy(nepi_wide)
nepi_wide_modified[, VXa := factor(
  ifelse(VXa %in% c("prev1", "prev2", "waned"), "prev",
         ifelse(VXa %in% c("vac1", "vac2"), "vac", as.character(VXa))),
  levels = c("never", "prev", "vac"))]


# Melt and then calculate the median, low and high bounds
nepi_long <- melt(nepi_wide_modified, id.vars = c("runtype", "year", "VXa", "AgeGrp"),
                  measure.vars = c("S","C","I","Dn","Ds","Dc","T","R","Rt"),
                  variable.name = "Indicator", value.name = "Value")


nepi_long <- setDT(nepi_long)
nepi_long[runtype == "baseline"]$VXa = "never"

nepi_sum <- nepi_long %>%
  group_by(runtype, year, VXa, AgeGrp) %>%
  summarise(sum_val = sum(Value, na.rm = TRUE))

nepi_sum = as.data.table(nepi_sum)
#Calculate proportions by never, prev and vac
nepi_sum[, Proportion := sum_val / sum(sum_val), by = .(runtype, year, AgeGrp)]

nepi_prop = nepi_sum

custom_labels <- c("[15,15]" = "Age 15",
                   "[25,25]" = "Age 25",
                   "[40,40]" = "Age 40")

#reorder runtype
nepi_prop$runtype <- factor(nepi_prop$runtype, levels = c("baseline", "ProgClinDisease_AI",
                                                          "ProgInfecDisease_AI", "ProgAnyDisease_AI"),
                            labels = c("baseline", 
                                       "Prevents infectious\n symptomatic disease only",
                                       "Prevents any\n infectious disease", 
                                       "Prevents any\n disease"))
nepi_prop$VXa <- factor(nepi_prop$VXa, levels = c("never", "prev", "vac"),
                        labels = c("Never vaccinated", "Vaccinated & not protected",
                                   "Vaccinated & protected"))


prop_plots <- ggplot(nepi_prop[runtype != "baseline"], aes(fill = VXa, y = Proportion, x = year)) +
  geom_bar(position="fill", stat = "identity") +
  scale_fill_viridis_d(direction = 1, option = "viridis")+ ylim(0,1) +
  facet_grid(AgeGrp~runtype, labeller = labeller(AgeGrp = custom_labels)) +
  ylab("Proportion") + 
  xlab("Year") +  # Change x-axis label to 'Year'
  theme(
    axis.title = element_text(size = 14),  # Increase size of axis titles
    axis.text = element_text(size = 14),   # Increase size of axis text (tick labels)
    strip.text = element_text(size = 14),
    legend.position = "bottom", 
    legend.justification = "right",# Ensure the legend is at the bottom
    legend.title = element_blank(),
    legend.text = element_text(size = 14)   # Increase size of legend text
  )


print(prop_plots)
