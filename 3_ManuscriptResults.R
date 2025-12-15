# -------------------------
# Results for manuscript - main text
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

##### Abstract results ####

abstract <- fread("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv")
abstract <- abstract[(Year == 2032 | Year == 2050)]
abstract <- abstract[Runtype == "ProgClinDisease_AI" | Runtype == "ProgInfecDisease_AI" |
                       Runtype == "ProgAnyDisease_AI"]

abstract <- abstract[, .(Year, Indicator, Runtype, combined)]
abstract <- dcast(abstract, Year + Indicator ~ Runtype, value.var = "combined")
#& Indicator == "prop_incDc_diff"

abstract <- abstract[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]



##### No-new-vaccine cumulative numbers ####

baseline <- fread("./epi_output/grouped_output/cumulative_numbers_baseline.csv")
baseline <- baseline[Year == 2050 & Runtype == "baseline"]

baseline <- baseline[, .(Year, Indicator, combined)]
baseline <- dcast(baseline, Year ~ Indicator, value.var = "combined")


##### Figure 2 ####

cumulative_prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))
cumulative_prop <- cumulative_prop[Year == 2050 | Year == 2032]
cumulative_prop <- cumulative_prop[Indicator != "prop_incDsDc_diff"]

cumulative_prop$Indicator <- factor(cumulative_prop$Indicator,
                                    levels = c("prop_incDc_diff", "prop_incDs_diff",
                                               "prop_incDn_diff", "prop_mort_diff"),
                                    labels = c("sTB", "aTB", "nTB", "TB deaths"))

original <- cumulative_prop[Runtype == "ProgClinDisease_AI" |
                              Runtype == "ProgInfecDisease_AI" |
                              Runtype == "ProgAnyDisease_AI"]

original$Runtype <- factor(original$Runtype,
                           levels = c("ProgClinDisease_AI", "ProgInfecDisease_AI","ProgAnyDisease_AI"),
                           labels = c("Prevents\n infectious\n symptomatic\n disease\n only",
                                      "Prevents\n any\n infectious\n disease",
                                      "Prevents\n any\n disease"))

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


plt <- list()

plt[[1]] <- ggplot(data = original[Year == "2032"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_wrap(Indicator ~ ., ncol = 1) +
  labs(title = "Shorter-term (2030–2032)") + ylim(c(-1, 7)) +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme


plt[[2]] <- ggplot(data = original[Year == "2050"],
                   aes(x = Runtype, y = medval*100, fill = Runtype)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.25,
           width = 1, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lowval*100, ymax = highval*100), width = 0.2) +
  facet_wrap(Indicator ~ ., ncol = 1) +
  labs(title = "Longer-term (2030–2050)") + ylim(c(-6, 42)) +
  scale_fill_manual(values = viridis_colors) +
  ylab("Percent reduction (%)")  + plt_theme

wrap_plots(plt, ncol = 2)




##### Table 2 ####

cumulative_prop <- fread(paste0("./epi_output/grouped_output/cumulative_proportion_difference_baseline.csv"))
cumulative_prop <- cumulative_prop[Year == 2050 | Year == 2032]
cumulative_prop <- cumulative_prop[, combined := paste0(round(medval*100, 1),
                                                        " (", round(lowval*100, 1),
                                                        ", ", round(highval*100, 1), ")")]

cumulative_prop <- cumulative_prop[Runtype == "ProgClinDisease_AI" |
                                     Runtype == "ProgInfecDisease_AI" |
                                     Runtype == "ProgAnyDisease_AI"]

cumulative_prop <- cumulative_prop[, .(Year, Runtype, Indicator, combined)]
cumulative_prop <- dcast(cumulative_prop, Year + Indicator ~ Runtype, value.var = "combined")
cumulative_prop <- cumulative_prop[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]



cumulative_num <- fread(paste0("./epi_output/grouped_output/cumulative_numbers_difference_baseline.csv"))
cumulative_num <- cumulative_num[Year == 2050 | Year == 2032]
cumulative_num <- cumulative_num[, combined := paste0(round(medval/1000, 2),
                                                      " (", round(lowval/1000, 2),
                                                      ", ", round(highval/1000, 2), ")")]

cumulative_num <- cumulative_num[Runtype == "ProgClinDisease_AI" |
                                   Runtype == "ProgInfecDisease_AI" |
                                   Runtype == "ProgAnyDisease_AI"]

cumulative_num <- cumulative_num[, .(Year, Runtype, Indicator, combined)]
cumulative_num <- dcast(cumulative_num, Year + Indicator ~ Runtype, value.var = "combined")
cumulative_num <- cumulative_num[, .(Year, Indicator, ProgClinDisease_AI, ProgInfecDisease_AI, ProgAnyDisease_AI)]



##### Figure 3 #####
total <- fread(paste0("./epi_output/grouped_output/total_numbers_baseline.csv"))
total <- total[Indicator != "sum_incDsDc"]
total <- total[Year >= 2028]

total$Runtype <- factor(total$Runtype,
                        levels = c("baseline", "ProgClinDisease_AI", "ProgInfecDisease_AI",
                                   "ProgAnyDisease_AI"),
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




##### -----

