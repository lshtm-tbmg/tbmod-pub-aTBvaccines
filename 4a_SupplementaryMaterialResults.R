# -------------------------
# Results for supplementary material
# 18 September 2025
# Rebecca Clark
# -------------------------
rm(list=ls())

suppressPackageStartupMessages({
  library(rlang)
  library(fs)
  library(data.table)
  library(ggplot2)
  library(cowplot)
  library(qs)
  library(uuid)
  library(gridExtra)
  library(ggpubr)
  library(digest)
  library(patchwork)
  library(dplyr, warn.conflicts = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
  
})

##### Supplementary Figures and Tables #####

##### ------ Figure S5: No-new-vaccine plots ####

baseline <- fread("./epi_output/no-new-vaccine_output/IND_n_epi_baseline.csv")
baseline <- baseline[, Scenario := "Baseline"]

high <- fread("./epi_output/no-new-vaccine_output/IND_n_epi_high.csv")
high <- high[, Scenario := "High"]

mid <- fread("./epi_output/no-new-vaccine_output/IND_n_epi_mid.csv")
mid <- mid[, Scenario := "Medium"]

low <- fread("./epi_output/no-new-vaccine_output/IND_n_epi_low.csv")
low <- low[, Scenario := "Low"]

comb <- rbind(baseline, high, mid, low)
comb <- comb[AgeGrp == "[0,99]"]


if (T){
  n_epi_long <- comb[Year >= 2005]
  targets <- fread(paste0("./countries/IND/parameters/target_plot.csv"))
  
  plt_theme <- theme(
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = "none",
    axis.title.x = element_text(hjust = 1, size = 14),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(size = 12)
  ) 
  
  custom_labels <- c("inc_rate" = "TB incidence rate",
                     "mort_rate" = "TB mortality rate",
                     "tx_rate" = "TB notification rate",
                     "prev_rate" = "Infectious TB prevalence",
                     "inf_prev_prop" = "Proportion of the population with TB infection",
                     "aTB_prop" = "Proportion of infectious TB that is asymptomatic")
  
  n_epi_long$Scenario <- factor(n_epi_long$Scenario,
                                levels = c("Baseline", "Low", "Medium", "High"),
                                labels = c("Baseline", "Low", "Medium", "High"))
  
  setnames(n_epi_long, "Scenario", "Relative Asymptomatic TB Infectiousness")
  
}

if (T){
  plt <- list()
  
  plt[[1]] <- ggplot(data = n_epi_long[Indicator == "inc_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_inci_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) + 
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[2]] <- ggplot(data = n_epi_long[Indicator == "tx_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_notif_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[3]] <- ggplot(data = n_epi_long[Indicator == "mort_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_deaths_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[4]] <- ggplot(data = n_epi_long[Indicator == "prev_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_prev_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    geom_pointrange(data = targets[name == "TB_prevwider_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[5]] <- ggplot(data = n_epi_long[Indicator == "aTB_prop"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, 
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    ylim(c(0,1)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Proportion") +
    plt_theme
  
  plt[[6]] <- ggplot(data = n_epi_long[Indicator == "inf_prev_prop"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    ylim(c(0,0.1)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = -1) +
    scale_fill_viridis_d(direction = -1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Proportion") +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(size = 12)
    ) 
  
  
  epi_plot <- wrap_plots(plt, ncol = 3)
  
}

print(epi_plot)



##### ------ Figure S6: No-new-vaccine plots (zero infectiousness) #####
comb <- fread("./epi_output/no-new-vaccine_output/IND_n_epi_zero.csv")
comb <- comb[AgeGrp == "[0,99]"][, Scenario := "Zero"]

if (T){
  n_epi_long <- comb[Year >= 2005]
  targets <- fread(paste0("./countries/IND/parameters/target_plot.csv"))
  
  plt_theme <- theme(
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = "none",
    axis.title.x = element_text(hjust = 1, size = 14),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(size = 12)
  ) 
  
  custom_labels <- c("inc_rate" = "TB incidence rate",
                     "mort_rate" = "TB mortality rate",
                     "tx_rate" = "TB notification rate",
                     "prev_rate" = "Infectious TB prevalence",
                     "inf_prev_prop" = "Proportion of the population with TB infection",
                     "aTB_prop" = "Proportion of infectious TB that is asymptomatic")
  
  setnames(n_epi_long, "Scenario", "Relative Asymptomatic TB Infectiousness")
  
}

if (T){
  plt <- list()
  
  plt[[1]] <- ggplot(data = n_epi_long[Indicator == "inc_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_inci_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    scale_color_viridis_d(direction = 1) +
    scale_fill_viridis_d(direction = 1) + 
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[2]] <- ggplot(data = n_epi_long[Indicator == "tx_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_notif_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = 1) +
    scale_fill_viridis_d(direction = 1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[3]] <- ggplot(data = n_epi_long[Indicator == "mort_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_deaths_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = 1) +
    scale_fill_viridis_d(direction = 1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[4]] <- ggplot(data = n_epi_long[Indicator == "prev_rate"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    geom_pointrange(data = targets[name == "TB_prev_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    geom_pointrange(data = targets[name == "TB_prevwider_0_99"],
                    aes(x = year-0.5, y = value, ymin = lo, ymax = hi)) +
    ylim(c(0,NA)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = 1) +
    scale_fill_viridis_d(direction = 1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Rate (per 100,000)") +
    plt_theme
  
  plt[[5]] <- ggplot(data = n_epi_long[Indicator == "aTB_prop"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval, 
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    ylim(c(0,1)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = 1) +
    scale_fill_viridis_d(direction = 1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Proportion") +
    plt_theme
  
  plt[[6]] <- ggplot(data = n_epi_long[Indicator == "inf_prev_prop"]) +
    geom_ribbon(aes(x = Year, ymin = lowval, ymax = highval,
                    fill = `Relative Asymptomatic TB Infectiousness`), alpha = 0.2) +
    geom_line(aes(x = Year, y = medval, col = `Relative Asymptomatic TB Infectiousness`)) +
    ylim(c(0,0.1)) + xlim(c(2005,2030)) +
    scale_color_viridis_d(direction = 1) +
    scale_fill_viridis_d(direction = 1) + 
    facet_wrap(~ Indicator, scales = "free_y", labeller = labeller(Indicator = custom_labels)) +
    xlab("Year") + ylab("Proportion") +
    theme(
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.justification = "right",
      legend.position = "bottom",
      axis.title.x = element_text(hjust = 1, size = 14),
      axis.title.y = element_text(hjust = 1, size = 14),
      axis.text = element_text(size = 12)
    ) 
  
  
  epi_plot <- wrap_plots(plt, ncol = 3)
  
}

print(epi_plot)



##### ------ Figure S7: Vx proportions over time (Separate script)  #####
##### ------ Table S7: No-new-vaccine values (baseline/zero infectiousness) ####
baseline <- fread("./epi_output/grouped_output/cumulative_numbers_baseline.csv")
baseline <- baseline[(Year == 2050 | Year == 2032) & Runtype == "baseline"]

baseline <- baseline[, .(Year, Indicator, combined)]
baseline <- dcast(baseline, Year ~ Indicator, value.var = "combined")


zero <- fread("./epi_output/grouped_output/cumulative_numbers_zero.csv")
zero <- zero[(Year == 2050 | Year == 2032) & Runtype == "baseline"]

zero <- zero[, .(Year, Indicator, combined)]
zero <- dcast(zero, Year ~ Indicator, value.var = "combined")



##### ------ Figure S8: Current infection plots (2032 and 2050)  ####

original_CI <- cumulative_prop[Runtype == "ProgClinDisease_CI" |
                                 Runtype == "ProgInfecDisease_CI" |
                                 Runtype == "ProgAnyDisease_CI"]

original_CI$Runtype <- factor(original_CI$Runtype,
                              levels = c("ProgClinDisease_CI", "ProgInfecDisease_CI","ProgAnyDisease_CI"),
                              labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                         "Prevents\n any\n infectious\n disease",
                                         "Prevents\n any\n disease"))

plt <- list()

plt[[1]] <- ggplot(data = original_CI[Year == "2032"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_wrap(Indicator ~ ., ncol = 1) + ylim(c(-1, 7)) +
  labs(title = "Shorter-term (2030–2032)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme


plt[[2]] <- ggplot(data = original_CI[Year == "2050"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_wrap(Indicator ~ ., ncol = 1) + ylim(c(-6, 42)) +
  labs(title = "Longer-term (2030–2050)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme

# Combine the plots together
wrap_plots(plt, ncol = 2)


##### ------ Table S8: Current infection values (2032 and 2050)  ####

cumulative_prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))
cumulative_prop <- cumulative_prop[Year == 2050 | Year == 2032]
cumulative_prop <- cumulative_prop[, combined := paste0(round(medval*100, 2),
                                                        " (", round(lowval*100, 2),
                                                        ", ", round(highval*100, 2), ")")]

cumulative_prop <- cumulative_prop[Runtype == "ProgClinDisease_CI" |
                                     Runtype == "ProgInfecDisease_CI" |
                                     Runtype == "ProgAnyDisease_CI"]

cumulative_prop <- cumulative_prop[, .(Year, Runtype, Indicator, combined)]
cumulative_prop <- dcast(cumulative_prop, Year + Indicator ~ Runtype, value.var = "combined")
cumulative_prop <- cumulative_prop[, .(Year, Indicator, ProgClinDisease_CI, ProgInfecDisease_CI, ProgAnyDisease_CI)]



cumulative_num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_baseline.csv"))
cumulative_num <- cumulative_num[Year == 2050 | Year == 2032]
cumulative_num <- cumulative_num[, combined := paste0(round(medval/1000, 2),
                                                      " (", round(lowval/1000, 2),
                                                      ", ", round(highval/1000, 2), ")")]

cumulative_num <- cumulative_num[Runtype == "ProgClinDisease_CI" |
                                   Runtype == "ProgInfecDisease_CI" |
                                   Runtype == "ProgAnyDisease_CI"]

cumulative_num <- cumulative_num[, .(Year, Runtype, Indicator, combined)]
cumulative_num <- dcast(cumulative_num, Year + Indicator ~ Runtype, value.var = "combined")
cumulative_num <- cumulative_num[, .(Year, Indicator, ProgClinDisease_CI, ProgInfecDisease_CI, ProgAnyDisease_CI)]

comb <- rbind(cumulative_num, cumulative_prop)

comb$Indicator <- factor(comb$Indicator,
                         levels = c("cumulative_incDc_diff", "cumulative_incDs_diff",
                                    "cumulative_incDsDc_diff",
                                    "cumulative_incDn_diff", "cumulative_mort_diff",
                                    "prop_incDc_diff", "prop_incDs_diff",
                                    "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("cumulative_incDc_diff", "cumulative_incDs_diff",
                                    "cumulative_incDsDc_diff",
                                    "cumulative_incDn_diff", "cumulative_mort_diff",
                                    "prop_incDc_diff", "prop_incDs_diff",
                                    "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"))



##### ------ Figure S9: Current infection plots (time trends)  #####
total <- fread(paste0("./epi_output/grouped_output/total_numbers_baseline.csv"))
total <- total[Indicator != "sum_incDsDc"]
total <- total[Year >= 2028]

total$Runtype <- factor(total$Runtype,
                        levels = c("baseline", "ProgClinDisease_CI", "ProgInfecDisease_CI",
                                   "ProgAnyDisease_CI"),
                        labels = c("No-new-vaccine",
                                   "Prevents infectious symptomatic disease only",
                                   "Prevents any infectious disease",
                                   "Prevents any disease"))

total <- total[!(is.na(Runtype))]
total <- total[Indicator != "N_pop"]
total$Indicator <- factor(total$Indicator,
                          levels = c("N_incDc", "N_incDs",
                                     "N_incDn", "N_mort"),
                          labels = c("sTB", "aTB", "nTB", "TB deaths"))

viridis_colors <- viridis::viridis(n = 4, option = "viridis", direction = -1)

plt_total <- list()

plt_total[[1]] <- ggplot(data = total[Indicator == "sTB"]) +
  geom_ribbon(aes(x = Year, ymin = lowval/1000, ymax = highval/1000, fill = Runtype), alpha = 0.25) +
  geom_line(aes(x = Year, y = medval/1000, col = Runtype)) +
  facet_wrap(Indicator ~ ., scales = "free_y") + ylim(0, NA) + xlim(2028, 2050) +
  scale_fill_manual(values = viridis_colors) +
  scale_color_manual(values = viridis_colors) +
  ylab("Numbers (millions)") + guides(fill=guide_legend(ncol=2)) + plt_theme 

plt_total[[2]] <- ggplot(data = total[Indicator == "aTB"]) +
  geom_ribbon(aes(x = Year, ymin = lowval/1000, ymax = highval/1000, fill = Runtype), alpha = 0.25) +
  geom_line(aes(x = Year, y = medval/1000, col = Runtype)) +
  facet_wrap(Indicator ~ ., scales = "free_y") + ylim(0, NA) + xlim(2028, 2050) +
  scale_fill_manual(values = viridis_colors) +
  scale_color_manual(values = viridis_colors) +
  ylab("Numbers (millions)") + guides(fill=guide_legend(ncol=2)) + plt_theme 

plt_total[[3]] <- ggplot(data = total[Indicator == "nTB"]) +
  geom_ribbon(aes(x = Year, ymin = lowval/1000, ymax = highval/1000, fill = Runtype), alpha = 0.25) +
  geom_line(aes(x = Year, y = medval/1000, col = Runtype)) +
  facet_wrap(Indicator ~ ., scales = "free_y") + ylim(0, NA) + xlim(2028, 2050) +
  scale_fill_manual(values = viridis_colors) +
  scale_color_manual(values = viridis_colors) +
  ylab("Numbers (millions)") + guides(fill=guide_legend(ncol=2)) + plt_theme 

plt_total[[4]] <- ggplot(data = total[Indicator == "TB deaths"]) +
  geom_ribbon(aes(x = Year, ymin = lowval/1000, ymax = highval/1000, fill = Runtype), alpha = 0.25) +
  geom_line(aes(x = Year, y = medval/1000, col = Runtype)) +
  facet_wrap(Indicator ~ ., scales = "free_y") + ylim(0, NA) + xlim(2028, 2050) +
  scale_fill_manual(values = viridis_colors) +
  scale_color_manual(values = viridis_colors) +
  ylab("Numbers (millions)") + guides(fill=guide_legend(ncol=2)) + 
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.justification = "right",
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 1, size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) 

# Combine the plots together
wrap_plots(plt_total, ncol = 2)



##### ------ Table S9/S10: Short and long term varying infectiousness values ####

### Low infectiousness
prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_low.csv"))
prop <- prop[Year == 2050 | Year == 2032]
prop <- prop[, combined := paste0(round(medval*100, 1), " (", round(lowval*100, 1),
                                  ", ", round(highval*100, 1), ")")]

prop <- prop[, .(Year, Runtype, Indicator, combined)]
prop <- dcast(prop, Year + Indicator ~ Runtype, value.var = "combined")
prop <- prop[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]

prop$Indicator <- factor(prop$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"))


num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_low.csv"))
num <- num[Year == 2050 | Year == 2032]
num <- num[, combined := paste0(round(medval/1000, 2), " (", round(lowval/1000, 2),
                                ", ", round(highval/1000, 2), ")")]

num <- num[, .(Year, Runtype, Indicator, combined)]
num <- dcast(num, Year + Indicator ~ Runtype, value.var = "combined")
num <- num[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]

num$Indicator <- factor(num$Indicator,
                        levels = c("cumulative_incDc_diff", "cumulative_incDs_diff", "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"),
                        labels = c("cumulative_incDc_diff", "cumulative_incDs_diff", "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"))

# Medium infectiousness
prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_mid.csv"))
prop <- prop[Year == 2050 | Year == 2032]
prop <- prop[, combined := paste0(round(medval*100, 1), " (", round(lowval*100, 1),
                                  ", ", round(highval*100, 1), ")")]

prop <- prop[, .(Year, Runtype, Indicator, combined)]
prop <- dcast(prop, Year + Indicator ~ Runtype, value.var = "combined")
prop <- prop[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]
prop$Indicator <- factor(prop$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"))


num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_mid.csv"))
num <- num[Year == 2050 | Year == 2032]
num <- num[, combined := paste0(round(medval/1000, 2), " (", round(lowval/1000, 2),
                                ", ", round(highval/1000, 2), ")")]

num <- num[, .(Year, Runtype, Indicator, combined)]
num <- dcast(num, Year + Indicator ~ Runtype, value.var = "combined")
num <- num[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]
num$Indicator <- factor(num$Indicator,
                        levels = c("cumulative_incDc_diff", "cumulative_incDs_diff", "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"),
                        labels = c("cumulative_incDc_diff", "cumulative_incDs_diff", "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"))

# High infectiousness
prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_high.csv"))
prop <- prop[Year == 2050 | Year == 2032]
prop <- prop[, combined := paste0(round(medval*100, 1), " (", round(lowval*100, 1),
                                  ", ", round(highval*100, 1), ")")]

prop <- prop[, .(Year, Runtype, Indicator, combined)]
prop <- dcast(prop, Year + Indicator ~ Runtype, value.var = "combined")
prop <- prop[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]
prop$Indicator <- factor(prop$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"))


num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_high.csv"))
num <- num[Year == 2050 | Year == 2032]
num <- num[, combined := paste0(round(medval/1000, 2), " (", round(lowval/1000, 2),
                                ", ", round(highval/1000, 2), ")")]

num <- num[, .(Year, Runtype, Indicator, combined)]
num <- dcast(num, Year + Indicator ~ Runtype, value.var = "combined")
num <- num[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]
num$Indicator <- factor(num$Indicator,
                        levels = c("cumulative_incDc_diff", "cumulative_incDs_diff",
                                   "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"),
                        labels = c("cumulative_incDc_diff", "cumulative_incDs_diff",
                                   "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"))


# Zero infectiousness
prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_zero.csv"))
prop <- prop[Year == 2050 | Year == 2032]
prop <- prop[, combined := paste0(round(medval*100, 1), " (", round(lowval*100, 1),
                                  ", ", round(highval*100, 1), ")")]

prop <- prop[, .(Year, Runtype, Indicator, combined)]
prop <- dcast(prop, Year + Indicator ~ Runtype, value.var = "combined")
prop <- prop[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]
prop$Indicator <- factor(prop$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"))


num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_zero.csv"))
num <- num[Year == 2050 | Year == 2032]
num <- num[, combined := paste0(round(medval/1000, 2), " (", round(lowval/1000, 2),
                                ", ", round(highval/1000, 2), ")")]

num <- num[, .(Year, Runtype, Indicator, combined)]
num <- dcast(num, Year + Indicator ~ Runtype, value.var = "combined")
num <- num[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]
num$Indicator <- factor(num$Indicator,
                        levels = c("cumulative_incDc_diff", "cumulative_incDs_diff",
                                   "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"),
                        labels = c("cumulative_incDc_diff", "cumulative_incDs_diff",
                                   "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"))


##### ------ Figure S10 and S11: Short and longer term varying infectiousness plots ####

baseline <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))
baseline <- baseline[Runtype == "ProgClinDisease_AI" | Runtype == "ProgInfecDisease_AI" | Runtype == "ProgAnyDisease_AI"]
baseline <- baseline[, Scenario := "Baseline: 0.62—1"]

low <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_low.csv"))
low <- low[Runtype == "ProgClinDisease_AI" | Runtype == "ProgInfecDisease_AI" | Runtype == "ProgAnyDisease_AI"]
low <- low[, Scenario := "Low: 0.62—0.74"]

mid <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_mid.csv"))
mid <- mid[Runtype == "ProgClinDisease_AI" | Runtype == "ProgInfecDisease_AI" | Runtype == "ProgAnyDisease_AI"]
mid <- mid[, Scenario := "Medium: 0.74—0.87"]

high <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_high.csv"))
high <- high[Runtype == "ProgClinDisease_AI" | Runtype == "ProgInfecDisease_AI" | Runtype == "ProgAnyDisease_AI"]
high <- high[, Scenario := "High: 0.87—1"]

zero <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_zero.csv"))
zero <- zero[Runtype == "ProgClinDisease_AI" | Runtype == "ProgInfecDisease_AI" | Runtype == "ProgAnyDisease_AI"]
zero <- zero[, Scenario := "Zero: 0"]

comb <- rbind(baseline, low, mid, high, zero)

comb$Runtype <- factor(comb$Runtype,
                       levels = c("ProgClinDisease_AI", "ProgInfecDisease_AI","ProgAnyDisease_AI"),
                       labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                  "Prevents\n any\n infectious\n disease",
                                  "Prevents\n any\n disease"))

comb <- comb[Indicator != "prop_incDsDc_diff"]
comb$Indicator <- factor(comb$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("sTB", "aTB", "nTB", "TB deaths"))

comb$Scenario <- factor(comb$Scenario,
                        levels = c("Baseline: 0.62—1", "Low: 0.62—0.74",
                                   "Medium: 0.74—0.87", "High: 0.87—1",
                                   "Zero: 0"),
                        labels = c("Baseline: 0.62—1", "Low: 0.62—0.74",
                                   "Medium: 0.74—0.87", "High: 0.87—1",
                                   "Zero: 0"))

viridis_colors <- viridis::viridis(n = 4, option = "viridis", direction = -1)
viridis_colors <- viridis_colors[-1]

plt_theme <- theme(
  legend.text = element_text(size = 14),
  legend.title = element_blank(),
  legend.justification = "right",
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_text(hjust = 1, size = 14),
  axis.text = element_text(size = 10),
  strip.text = element_text(size = 14),
  panel.border = element_rect(color = "black", fill = NA, size = 1)
) 


ggplot(data = comb[Year == "2032"],
       aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-2, 10) +
  labs(title = "Shorter-term (2030–2032)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme


ggplot(data = comb[Year == "2050"],
       aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-8, 40) +
  labs(title = "Longer-term (2030–2050)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme

##### ------ Table S11: AI/CI-ID Short and longer term values ####

prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))
prop <- prop[Year == 2050 | Year == 2032]
prop <- prop[, combined := paste0(round(medval*100, 1), " (", round(lowval*100, 1),
                                  ", ", round(highval*100, 1), ")")]

prop <- prop[Runtype == "ProgClinDisease_AI_ID" | Runtype == "ProgInfecDisease_AI_ID" |
               Runtype == "ProgAnyDisease_AI" | Runtype == "ProgClinDisease_CI_ID" |
               Runtype == "ProgInfecDisease_CI_ID" | Runtype == "ProgAnyDisease_CI"]

prop <- prop[, .(Year, Runtype, Indicator, combined)]
prop <- dcast(prop, Year + Indicator ~ Runtype, value.var = "combined")
prop <- prop[, .(Year, Indicator, ProgClinDisease_AI_ID,
                 ProgInfecDisease_AI_ID, ProgAnyDisease_AI,
                 ProgClinDisease_CI_ID, ProgInfecDisease_CI_ID, ProgAnyDisease_CI)]

prop$Indicator <- factor(prop$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("prop_incDc_diff", "prop_incDs_diff", "prop_incDsDc_diff",
                                    "prop_incDn_diff", "prop_mort_diff"))


num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_baseline.csv"))
num <- num[Year == 2050 | Year == 2032]
num <- num[, combined := paste0(round(medval/1000, 2), " (", round(lowval/1000, 2),
                                ", ", round(highval/1000, 2), ")")]

num <- num[, .(Year, Runtype, Indicator, combined)]
num <- dcast(num, Year + Indicator ~ Runtype, value.var = "combined")
num <- num[, .(Year, Indicator, ProgClinDisease_AI_ID,
               ProgInfecDisease_AI_ID, ProgAnyDisease_AI,
               ProgClinDisease_CI_ID, ProgInfecDisease_CI_ID, ProgAnyDisease_CI)]

num$Indicator <- factor(num$Indicator,
                        levels = c("cumulative_incDc_diff", "cumulative_incDs_diff", "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"),
                        labels = c("cumulative_incDc_diff", "cumulative_incDs_diff", "cumulative_incDsDc_diff",
                                   "cumulative_incDn_diff", "cumulative_mort_diff"))

##### ------ Figure S12: Varying AI/CI ####

cumulative_prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))

AI <- cumulative_prop[Runtype == "ProgClinDisease_AI_ID" |
                        Runtype == "ProgInfecDisease_AI_ID" |
                        Runtype == "ProgAnyDisease_AI"]

AI$Runtype <- factor(AI$Runtype,
                     levels = c("ProgClinDisease_AI_ID", "ProgInfecDisease_AI_ID","ProgAnyDisease_AI"),
                     labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                "Prevents\n any\n infectious\n disease",
                                "Prevents\n any\n disease"))

AI <- AI[, Scenario := "AI"]

CI <- cumulative_prop[Runtype == "ProgClinDisease_CI_ID" |
                        Runtype == "ProgInfecDisease_CI_ID" |
                        Runtype == "ProgAnyDisease_CI"]

CI$Runtype <- factor(CI$Runtype,
                     levels = c("ProgClinDisease_CI_ID", "ProgInfecDisease_CI_ID","ProgAnyDisease_CI"),
                     labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                "Prevents\n any\n infectious\n disease",
                                "Prevents\n any\n disease"))

CI <- CI[, Scenario := "CI"]

comb <- rbind(AI, CI)
comb <- comb[Indicator != "prop_incDsDc_diff"]
comb$Indicator <- factor(comb$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("sTB", "aTB", "nTB", "TB deaths"))
plt <- list()


plt[[1]] <- ggplot(data = comb[Year == "2032"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-2, 10) +
  labs(title = "Shorter-term (2030–2032)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme


plt[[2]] <- ggplot(data = comb[Year == "2050"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-8, 40) +
  labs(title = "Longer-term (2030–2050)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme

# Combine the plots together
wrap_plots(plt, ncol = 2)



##### ------ Figure S13: Varying efficacy ####

cumulative_prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))

decreasing_eff <- cumulative_prop[Runtype == "ProgClinDisease_AI_25" |
                                    Runtype == "ProgInfecDisease_AI" |
                                    Runtype == "ProgAnyDisease_AI_75"]

decreasing_eff$Runtype <- factor(decreasing_eff$Runtype,
                                 levels = c("ProgClinDisease_AI_25", "ProgInfecDisease_AI","ProgAnyDisease_AI_75"),
                                 labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                            "Prevents\n any\n infectious\n disease",
                                            "Prevents\n any\n disease"))

decreasing_eff <- decreasing_eff[, Scenario := "Decreasing"]

increasing_eff <- cumulative_prop[Runtype == "ProgClinDisease_AI_75" |
                                    Runtype == "ProgInfecDisease_AI" |
                                    Runtype == "ProgAnyDisease_AI_25"]

increasing_eff$Runtype <- factor(increasing_eff$Runtype,
                                 levels = c("ProgClinDisease_AI_75", "ProgInfecDisease_AI","ProgAnyDisease_AI_25"),
                                 labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                            "Prevents\n any\n infectious\n disease",
                                            "Prevents\n any\n disease"))

increasing_eff <- increasing_eff[, Scenario := "Increasing"]

comb <- rbind(decreasing_eff, increasing_eff)
comb <- comb[Indicator != "prop_incDsDc_diff"]
comb$Indicator <- factor(comb$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("sTB", "aTB", "nTB", "TB deaths"))
plt <- list()


plt[[1]] <- ggplot(data = comb[Year == "2032"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-2, 10) +
  labs(title = "Shorter-term (2030–2032)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme


plt[[2]] <- ggplot(data = comb[Year == "2050"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-8, 40) +
  labs(title = "Longer-term (2030–2050)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme

# Combine the plots together
wrap_plots(plt, ncol = 2)



##### ------ Figure S14: Varying duration #####

decreasing_dur <- cumulative_prop[Runtype == "ProgClinDisease_AI_5y" |
                                    Runtype == "ProgInfecDisease_AI" |
                                    Runtype == "ProgAnyDisease_AI_20y"]

decreasing_dur$Runtype <- factor(decreasing_dur$Runtype,
                                 levels = c("ProgClinDisease_AI_5y", "ProgInfecDisease_AI","ProgAnyDisease_AI_20y"),
                                 labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                            "Prevents\n any\n infectious\n disease",
                                            "Prevents\n any\n disease"))

decreasing_dur <- decreasing_dur[,Scenario := "Decreasing"]

increasing_dur <- cumulative_prop[Runtype == "ProgClinDisease_AI_20y" |
                                    Runtype == "ProgInfecDisease_AI" |
                                    Runtype == "ProgAnyDisease_AI_5y"]

increasing_dur$Runtype <- factor(increasing_dur$Runtype,
                                 levels = c("ProgClinDisease_AI_20y", "ProgInfecDisease_AI","ProgAnyDisease_AI_5y"),
                                 labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                            "Prevents\n any\n infectious\n disease",
                                            "Prevents\n any\n disease"))

increasing_dur <- increasing_dur[,Scenario := "Increasing"]

comb <- rbind(decreasing_dur, increasing_dur)
comb <- comb[Indicator != "prop_incDsDc_diff"]
comb$Indicator <- factor(comb$Indicator,
                         levels = c("prop_incDc_diff", "prop_incDs_diff",
                                    "prop_incDn_diff", "prop_mort_diff"),
                         labels = c("sTB", "aTB", "nTB", "TB deaths"))
plt <- list()

plt[[1]] <- ggplot(data = comb[Year == "2032"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-2, 10) +
  labs(title = "Shorter-term (2030–2032)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme


plt[[2]] <- ggplot(data = comb[Year == "2050"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_grid(Indicator ~ Scenario) + ylim(-8, 40) +
  labs(title = "Longer-term (2030–2050)") +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme

# Combine the plots together
wrap_plots(plt, ncol = 2)







####### end #####
