rm(list=ls())

library(fixest)
library(dplyr)
library(marginaleffects)
library(ggplot2)

setwd('/Users/psp2nq/Documents/KnowledgeSovereignty')

##################################
# Load data
##################################

name <- "bootstrap_noselfauthor"  # or "noselfauthor"
sig=FALSE


dfself <- read.csv("data/clean/bootstrap_noselfauthor_R_disruption_03172026.csv")


if (sig==TRUE) {
  dfself <- subset(dfself, sig_direction == 1)
  filename_suffix=paste0(name,'_sig')
} else {
  filename_suffix=name
}


dfself$income_group <- factor(dfself$income_group, 
                              levels = c("LM-L", "UM", 'H'))

levels(dfself$income_group)

dfself$NResearchers = log10(dfself$NResearchers)


dfself$Country <- as.factor(dfself$Country)
dfself$Year <- as.factor(dfself$Year)
dfself$is_democratic <- as.factor(dfself$is_democratic)

dfself=dfself[complete.cases(dfself[, c('logzscore','FracInternationalAuthors')]), ]
length(unique(dfself$Country))

library(dplyr)

n_years <- length(unique(dfself$Year))
n_countries <- length(unique(dfself$Country))

cat("Panel:", n_countries, "countries ×", n_years, "years =", nrow(dfself), "obs\n")


df_normalized <- dfself %>%
  mutate(across(c(logNumPub, GDP,GDP_PCAP, RND_per,normalized_frac_top, FracInternationalAuthors,novel_pct10_rate,novel_pct10_rate_norm, disrupt_top5_rate,hit_rate,hit_rate_scinet,
                  logzscore, NResearchers,FracInternationalAuthors,logzscore, polity2, gov_sum_index ), scale))  # Normalize only columns x, y, z

length(unique(df_normalized$Country))

##################################
# Table 3 (main paper):normalized_frac_top as dependent variable
##################################


mt1 <- feols(normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors   | Country+Year, data = df_normalized, cluster = ~Country)
mt2 <- feols(normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers  + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
mt3 <- feols(normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
mt4 <- feols(normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore  | Country+Year, data = df_normalized, cluster = ~Country)


cor(df_normalized[, c('normalized_frac_top','hit_rate','novel_pct10_rate','disrupt_top5_rate','GDP',  "GDP_PCAP", "NResearchers", 
                      'FracInternationalAuthors','logzscore','RND_per', 'polity2', 'gov_sum_index')], use = "complete.obs")


etable(mt1,mt2,mt3,mt4)

fitstat_register("n_countries", function(x){
  if(!is.null(x$fixef_sizes) && "Country" %in% names(x$fixef_sizes)){
    x$fixef_sizes["Country"]   # number of unique countries in FE
  } else if("Country" %in% names(x$model_frame)){
    length(unique(x$model_frame$Country))  # fallback
  } else {
    NA
  }
})



etable(mt1,mt2,mt3,mt4, tex = TRUE, file = paste0("output/tables/H2_TopJournal_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, replace = TRUE, digits = 3,  
       caption = 'Two-way fixed effects panel regression estimates of top journal share on international
collaboration and citation self-preference.',
       dict = c('normalized_frac_top'='Top journal rate',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "logNumPub" = "# of publications",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                "FracInternationalAuthors" = "International collaboration",
                'polity2' = 'Polity2',
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7, label = 'tab:H2_topjournal',fontsize = "scriptsize"
       
)

lm_fit <- lm(normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers+ FracInternationalAuthors*logzscore, data = df_normalized)
a=car::vif(lm_fit, type = 'predictor')
#vif_df <- data.frame(  Variable = rownames(a), VIF      = a[, 1]  # first column is GVIF)




##################################
# Table S3 (SI): hit rate as dependent variable
##################################



mh1 <- feols(hit_rate ~  GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors   | Country+Year, data = df_normalized, cluster = ~Country)
mh2 <- feols(hit_rate ~  GDP_PCAP + RND_per + NResearchers  + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
mh3 <- feols(hit_rate ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
mh4 <- feols(hit_rate ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore  | Country+Year, data = df_normalized, cluster = ~Country)





etable(mh1,mh2,mh3,mh4)



etable(mh1,mh2,mh3,mh4, tex = TRUE, file = paste0("output/tables/H2_HitRate_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, replace = TRUE, digits = 3,  
       caption = 'Two-way fixed effects panel regression estimates of hit rate on international
collaboration and citation self-preference.',
       dict = c('hit_rate'='Hit rate',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "logNumPub" = "# of publications",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                "FracInternationalAuthors" = "International collaboration",
                'polity2' = 'Democaracy score',
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7, label = 'tab:H2_hitrate',fontsize = "scriptsize"
       
)


##################################
# Table S4 (SI): novel_pct10_rate as dependent variable 
##################################

mn1 <- feols(novel_pct10_rate_norm ~  GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors   | Country+Year, data = df_normalized, cluster = ~Country)
mn2 <- feols(novel_pct10_rate_norm ~  GDP_PCAP + RND_per + NResearchers  + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
mn3 <- feols(novel_pct10_rate_norm ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
mn4 <- feols(novel_pct10_rate_norm ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore  | Country+Year, data = df_normalized, cluster = ~Country)


etable(mn1,mn2,mn3,mn4)


etable(mn1,mn2,mn3,mn4, tex = TRUE, file = paste0("output/tables/H2_Novelty_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, replace = TRUE, digits = 3,  
       caption = 'Two-way fixed effects panel regression estimates of novelty rate on international
collaboration and citation self-preference (bootstrap).',
       dict = c('novel_pct10_rate_norm'='Novelty rate',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "logNumPub" = "# of publications",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                "FracInternationalAuthors" = "International collaboration",
                'polity2' = 'Polity2',
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7, label = 'tab:H2_novelty',fontsize = "scriptsize"
       
)

##################################
# Table S5 (SI):  disrupt_top5_rate as dependent variable
##################################

md1 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors   | Country+Year, data = df_normalized, cluster = ~Country)
md2 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers  + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
md3 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
md4 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore  | Country+Year, data = df_normalized, cluster = ~Country)


etable(md1,md2,md3,md4)


etable(md1,md2,md3,md4, tex = TRUE, file = paste0("output/tables/H2_Disruption_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, replace = TRUE, digits = 3,  
       caption = 'Two-way fixed effects panel regression estimates of disruption rate on international
collaboration and citation self-preference (bootstrap).',
       dict = c('disrupt_top5_rate'='Disruption rate',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "logNumPub" = "# of publications",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                "FracInternationalAuthors" = "International collaboration",
                'polity2' = 'Polity2',
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7, label = 'tab:H2_disruption',fontsize = "scriptsize"
       
)

##################################
#Table S6 (SI): compare dependent variables without Polity2
##################################

mt4 <- feols(normalized_frac_top ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
mh4 <- feols(hit_rate ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
mn4 <- feols(novel_pct10_rate_norm ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
md4 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)


etable(mt4,mh4,mn4,md4)


etable(mt4,mh4,mn4,md4, tex = TRUE, file = paste0("output/tables/SI_H2_MultipleDep_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, replace = TRUE, digits = 3,  
       caption = 'Two-way fixed effects panel regression estimates of different dependent variables on international
collaboration and citation self-preference (bootstrap).',
       dict = c('disrupt_top5_rate'='Disruption rate',
                'novel_pct10_rate'='Novelty rate',
                'hit_rate'='Hit rate',
                'normalized_frac_top'='Top journal share',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "logNumPub" = "# of publications",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                "FracInternationalAuthors" = "International collaboration",
                'polity2' = 'Polity2',
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7, label = 'tab:quality_polity2',fontsize = "scriptsize"
       
)


##################################
#Table S7 (SI): compare dependent variables with Polity2
##################################

mt4 <- feols(normalized_frac_top ~  GDP_PCAP + RND_per + NResearchers +polity2 + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
mh4 <- feols(hit_rate ~ GDP_PCAP + RND_per + NResearchers +polity2 + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
mn4 <- feols(novel_pct10_rate_norm ~ GDP_PCAP + RND_per + NResearchers +polity2 + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
md4 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers +polity2 + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)


etable(mt4,mh4,mn4,md4)

etable(mt4,mh4,mn4,md4, tex = TRUE, file = paste0("output/tables/SI_H2_MultipleDep_polity2_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, replace = TRUE, digits = 3,  
       caption = 'Two-way fixed effects panel regression estimates of different dependent variables on international
collaboration and citation self-preference (bootstrap).',
       dict = c('disrupt_top5_rate'='Disruption rate',
                'novel_pct10_rate_norm'='Novelty rate',
                'hit_rate'='Hit rate',
                'normalized_frac_top'='Top journal share',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "logNumPub" = "# of publications",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                "FracInternationalAuthors" = "International collaboration",
                'polity2' = 'Democracy score',
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7, label = 'tab:quality_polity2',fontsize = "scriptsize"
       
)


##################################
# Figure 3 (main paper): coefficient plot
##################################

library(fixest)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggtext)

# define term order
term_order <- c(
  "GDP_PCAP",
  "RND_per",
  "NResearchers",
  'polity2',
  "FracInternationalAuthors",
  "logzscore",
  "FracInternationalAuthors:logzscore"
)


extract_coefs <- function(model, model_name) {
  coef_df        <- as.data.frame(coeftable(model))
  ci             <- as.data.frame(confint(model, se = "cluster", cluster = ~Country))
  coef_df$term   <- rownames(coef_df)
  coef_df$model  <- model_name
  coef_df$estimate <- coef_df$Estimate
  coef_df$ci_low   <- ci[, 1]
  coef_df$ci_high  <- ci[, 2]
  coef_df$p.value  <- coef_df[, 'Pr(>|t|)']
  coef_df[, c('term', 'model', 'estimate', 'ci_low', 'ci_high', 'p.value')]
}

dict = c('disrupt_top5_rate'='Disruption rate',
         'novel_pct10_rate'='Novelty rate',
         'hit_rate'='Hit rate',
         'normalized_frac_top'='Top journal share',
         "logzscore" = "Citation self-preference",
         "GDP_PCAP" = "GDP per capita",
         "logNumPub" = "# of publications",
         "RND_per" = "R&D as % of GDP",
         "NResearchers" = "# of researchers per million",
         "FracInternationalAuthors" = "International collaboration",
         'FracInternationalAuthors:logzscore'='Interaction term',
         'polity2' = 'Polity2'
         )

mt4 <- feols(normalized_frac_top ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
mh4 <- feols(hit_rate ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
mn4 <- feols(novel_pct10_rate_norm ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)
md4 <- feols(disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore | Country+Year, data = df_normalized, cluster = ~Country)


plot_coef_bolocks_sig <- function(coef_df, color, model, show_labels = TRUE) {
  
  coef_df$term <- as.character(coef_df$term)
  coef_df <- coef_df[coef_df$term %in% term_order, ]
  
  r2 <- round(fitstat(model, 'r2')$r2, 3)
  
  model_name <- unique(coef_df$model)
  letter_part <- sub("^(\\S+)\\s+(.*)", "\\1", model_name)
  name_part   <- sub("^(\\S+)\\s+(.*)", "\\2", model_name)
  
  title_html <- paste0(
    "<b>", letter_part, "</b>",
    " <span style='font-size:10pt;'>", name_part, "</span>"
  )
  
  coef_df <- coef_df[, c('term', 'estimate', 'ci_low', 'ci_high', 'p.value')]
  
  # significance flag
  coef_df$significant <- coef_df$p.value < 0.05
  
  coef_df <- rbind(
    coef_df,
    data.frame(term = 'R²', estimate = NA, ci_low = NA, ci_high = NA, p.value = NA, significant = NA)
  )
  
  term_levels <- c('R²', rev(term_order[term_order %in% coef_df$term]))
  coef_df$term <- factor(coef_df$term, levels = term_levels)
  coef_df <- coef_df[!is.na(coef_df$term), ]
  coef_df$term <- droplevels(coef_df$term)
  
  n_rows <- nrow(coef_df)
  bands <- data.frame(
    ymin = seq(0.5, n_rows - 0.5, by = 1),
    ymax = seq(1.5, n_rows + 0.5, by = 1),
    fill = rep(c('grey95', 'white'), length.out = n_rows)
  )
  
  x_mid <- mean(c(-0.5, 2))
  
  ggplot(coef_df, aes(x = estimate, y = term)) +
    geom_rect(
      data = bands,
      aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, fill = fill),
      inherit.aes = FALSE
    ) +
    scale_fill_identity() +
    scale_y_discrete(labels = dict) +
    geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey50') +
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5,
             fill = 'grey95', alpha = 1) +
    
    # error bars
    # error bars
    geom_errorbarh(
      aes(xmin = ci_low, xmax = ci_high),
      color = color, height = 0.2, na.rm = TRUE
    ) +
    # dots
    geom_point(
      shape = 21,
      color = color,
      fill = ifelse(coef_df$significant == TRUE, color, 'white'),
      size = 2.,
      na.rm = TRUE
    )   +
    
    scale_x_continuous(breaks = c(-1,0, 1, 2), labels = c(-1,0, 1, 2)) +
    coord_cartesian(xlim = c(-1, 2.1)) +
    annotate('text', x = x_mid, y = 'R²', label = r2, size = 3., color = 'grey30') +
    geom_hline(yintercept = 1.5, color = 'grey70', linewidth = 0.3) +
    labs(x = 'Coefficient', y = NULL, title = title_html) +
    theme_bw() +
    theme(
      panel.border  = element_blank(),
      axis.line.x   = element_line(color = 'black'),
      axis.ticks.x  = element_line(color = 'black'),
      axis.ticks.y  = element_blank(),
      panel.grid    = element_blank(),
      plot.title    = element_markdown(size = 10, hjust = 0, margin = margin(l = -12)),
      axis.text.y   = if (show_labels) element_text(size = 10) else element_blank(),
      axis.text.x   = element_text(size = 10),
      axis.title.x  = element_text(size = 10)
    )
}

colors <- c('#4c72b0', '#dd8452', '#8172b3', '#937860', '#da8bc3','#ccb974')

p1 <- plot_coef_bolocks_sig(extract_coefs(mt4, 'A Top journal share'), colors[3], mt4, show_labels = TRUE)
p2 <- plot_coef_bolocks_sig(extract_coefs(mh4, 'B Hit rate'), colors[4], mh4, show_labels = FALSE) 
#+scale_x_continuous(breaks = c(0, 0.5, 1))
p3 <- plot_coef_bolocks_sig(extract_coefs(mn4, 'C Novelty rate'), colors[5], mn4, show_labels = FALSE) 
p4 <- plot_coef_bolocks_sig(extract_coefs(md4, 'D Disruption rate'), colors[6], md4, show_labels = FALSE)

(p1 | plot_spacer() | p2 | plot_spacer() | p3 | plot_spacer() | p4)

ggsave(
  'output/figures/H2_compare_coef_plots.pdf',
  plot = (p1 | plot_spacer() | p2 | plot_spacer() | p3 | plot_spacer() | p4) +
    plot_layout(widths = c(1, 0.01, 1, 0.01, 1, 0.01, 1)),
  width  = 190,
  height = 80,
  units  = 'mm',
  device = 'pdf'
)



##################################
# Figure S9 & S10 (SI): Variance inflation factor (VIF) analysis
##################################


library(car)
library(ggplot2)
library(RColorBrewer)

# define clean variable names
var_labels <- c(
  "GDP_PCAP"                           = "GDP per capita",
  "RND_per"                            = "R&D as % of GDP",
  "NResearchers"                       = "# of researchers per million",
  'polity2'                            = 'Democracy score',
  "FracInternationalAuthors"           = "International collaboration",
  "logzscore"                          = "Citation self-preference",
  "FracInternationalAuthors:logzscore" = "International collaboration × Self-preference"
)

# function to compute and plot VIF
plot_vif <- function(formula_str, data, type, title, color,  filename) {
  lm_fit     <- lm(as.formula(formula_str), data = data)
  vif_result <- car::vif(lm_fit, type = type)
  
  # handle matrix output from type='predictor'
  if (type=='predictor') {
    vif_df <- data.frame(
      Variable = rownames(vif_result),
      VIF      = vif_result[, 1]  # first column is GVIF
    )
  } else {
    vif_df <- data.frame(
      Variable = names(vif_results),
      VIF = as.numeric(vif_results)
    )
  }
  
  vif_df$Variable_Clean <- ifelse(
    is.na(var_labels[vif_df$Variable]),
    vif_df$Variable,
    var_labels[vif_df$Variable]
  )
  
  p <- ggplot(vif_df, aes(x = reorder(Variable_Clean, VIF), y = VIF)) +
    geom_col(fill = color, width = 0.7) +
    geom_hline(yintercept = 5, linetype = "dashed", color = '#8c8c8c', linewidth = 0.7) +
    coord_flip() +
    labs(x = "Variables", y = "Variance Inflation Factor (VIF)", title = title) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid   = element_blank(),
      panel.border = element_blank(),
      axis.line.x  = element_line(color = "black", linewidth = 0.3),
      axis.line.y  = element_line(color = "black", linewidth = 0.3),
      axis.ticks.x = element_line(color = "black", linewidth = 0.3),
      axis.ticks.y = element_line(color = "black", linewidth = 0.3),
      axis.title   = element_text(size = 10),
      axis.text    = element_text(size = 9),
      plot.title   = element_text(size = 11, face = "bold")
    )
  
  #ggsave(filename, plot = p, width = 12, height = 6, dpi = 300, device = "pdf")
  return(p)
}


# Figure S9: without polity2
####################################

# run for all four regressions
vif1 <-plot_vif("normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore",
                df_normalized,'predictor', "A  Top journal rate",colors[3], "vif_top_journal.pdf")


vif2 <- plot_vif("hit_rate ~ GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "B  Hit rate",colors[4], "vif_hit_rate.pdf")

vif3 <- plot_vif("novel_pct10_rate_norm ~ GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "C  Novelty rate",colors[5], "vif_novelty.pdf")

vif4 <- plot_vif("disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "D  Disruption rate",colors[6], "vif_disruption.pdf")

(vif1 | vif2) / (vif3 | vif4)

ggsave(
  'output/figures/SI_H2_compare_vif.pdf',
  plot = (vif1 | vif2) / (vif3 | vif4) +
    plot_layout(widths = c(1, 0.01, 1, 0.01, 1, 0.01, 1)),
  width  = 180,
  height = 100,
  units  = 'mm',
  device = 'pdf'
)



# Figure S10: with polity2
##################################


# run for all four regressions
vif1 <-plot_vif("normalized_frac_top ~ GDP_PCAP + RND_per + NResearchers + polity2 + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "A  Top journal rate",colors[3], "vif_top_journal.pdf")


vif2 <- plot_vif("hit_rate ~ GDP_PCAP + RND_per + NResearchers+ polity2 + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "B  Hit rate",colors[4], "vif_hit_rate.pdf")

vif3 <- plot_vif("novel_pct10_rate_norm ~ GDP_PCAP + RND_per + NResearchers+ polity2 + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "C  Novelty rate",colors[5], "vif_novelty.pdf")

vif4 <- plot_vif("disrupt_top5_rate ~ GDP_PCAP + RND_per + NResearchers+ polity2 + FracInternationalAuthors*logzscore",
                 df_normalized,'predictor', "D  Disruption rate",colors[6], "vif_disruption.pdf")

(vif1 | vif2) / (vif3 | vif4)

ggsave(
  'output/figures/SI_H2_compare_vif_polity2.pdf',
  plot = (vif1 | vif2) / (vif3 | vif4) +
    plot_layout(widths = c(1, 0.01, 1, 0.01, 1, 0.01, 1)),
  width  = 180,
  height = 100,
  units  = 'mm',
  device = 'pdf'
)



##################################
# Figure 4 (main paper): Interaction effects between international collaboration and self-preference on research quality. 
##################################

mfx_intl <- slopes(mt4, variables = "FracInternationalAuthors",
                   at = list(logzscore = seq(min(df_normalized$logzscore, na.rm = TRUE), 
                                             max(df_normalized$logzscore, na.rm = TRUE), 
                                             by = 0.5)))

# Plot marginal effects of international collaboration
p1_intl <- ggplot(mfx_intl, aes(x = logzscore, y = estimate)) +
  geom_line(size = 2, color = colors[3]) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = colors[3]) +
  geom_hline(yintercept = 0, linetype = "dashed", color = '#8D95A0', alpha = 1, size=2.5) +
  labs(#title = "Marginal Effect of International Collaboration on Publication Quality",
    subtitle = "A",
    x = "Citation self-preference",
    y = "Marginal effect of \n international collaboration",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    panel.border = element_blank(),         # Remove panel border
    axis.line.x = element_line(color = "black"),  # Add bottom axis line
    axis.line.y = element_line(color = "black"),  # Add left axis line
    axis.ticks.x = element_line(color = "black"), # Add bottom ticks
    axis.ticks.y = element_line(color = "black"), # Add left ticks
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 32),
    plot.title = element_text(size = 32, face = "bold"),
    axis.text.y = element_text(size = 32),
    plot.subtitle = element_text(size = 32))

print(p1_intl)


# Plot marginal effects of self-preference
mfx_self <- slopes(mt4, variables = "logzscore",
                   at = list(FracInternationalAuthors = seq(0, 1, by = 0.1)))

p1_self <- ggplot(mfx_self, aes(x = FracInternationalAuthors, y = estimate)) +
  geom_line(size = 2, color = colors[5]) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = colors[5]) +
  geom_hline(yintercept = 0, linetype = "dashed", color = '#8D95A0', alpha = 1, size = 2.5) +
  labs(#title = "Marginal effect of self-preference",
    subtitle = "B",
    x = "International collaboration",
    y = "Marginal effect of \n citation self-preference",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    panel.border = element_blank(),         # Remove panel border
    axis.line.x = element_line(color = "black"),  # Add bottom axis line
    axis.line.y = element_line(color = "black"),  # Add left axis line
    axis.ticks.x = element_line(color = "black"), # Add bottom ticks
    axis.ticks.y = element_line(color = "black"), # Add left ticks
    axis.title = element_text(size = 32),
    axis.text = element_text(size = 32),
    plot.title = element_text(size = 32, face = "bold"),
    axis.text.y = element_text(size = 32),
    plot.subtitle = element_text(size = 32)
  )

print(p1_self)

# Create a side-by-side comparison
library(patchwork)

combined_plot <-  p1_intl+ plot_spacer() + p1_self  + 
  plot_layout(widths = c(1, 0.1, 1))  # Adjust the middle value to control spacing

combined_plot <- combined_plot + plot_annotation()
print(combined_plot)

filename = paste0("output/figures/H2_margninal_effects_interaction_term_", filename_suffix, ".pdf")

ggsave(filename, width = 18, height = 8, dpi = 300, device = "pdf")




##################################
# Table S8 (SI): Alternative specifications: Two-way fixed-effect panel regression estimates of scientific quality.
##################################


r1 <- feols(normalized_frac_top ~   GDP_PCAP + RND_per   + NResearchers + FracInternationalAuthors*logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
r2 <- feols(normalized_frac_top ~   GDP_PCAP + RND_per  + NResearchers + FracInternationalAuthors + logzscore +I(logzscore^2) | Country+Year, data = df_normalized, cluster = ~Country)
r3 <- feols(normalized_frac_top ~   RND_per  + NResearchers +  FracInternationalAuthors + logzscore*income_group   | Country+Year, data = df_normalized, cluster = ~Country)
r4 <- feols(normalized_frac_top ~   RND_per  + NResearchers + FracInternationalAuthors*income_group + logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
r5 <- feols(normalized_frac_top ~   RND_per  + NResearchers + FracInternationalAuthors*logzscore*income_group  | Country+Year, data = df_normalized, cluster = ~Country)

etable(r1,r2,r3,r4,r5) 

coef(r5)

etable(r1,r2,r3,r4,r5, tex = TRUE, file = "output/tables/SI_H2_topshare_alternative_bootstrap_03112026.tex",
       fitstat = ~ n + n_countries + f + r2 + ar2, 
       replace = TRUE,placement = "H",arraystretch = 0.7, digits = 3, label='tab:alternatives', 
       caption = 'Alternative specifications: Two-way fixed-effect panel regression estimates of scientific quality.',
       order = c("GDP", "GDP_PCAP","RND_per","NResearchers",
                 "FracInternationalAuthors",
                 'logzscore',
                 'FracInternationalAuthors:logzscore',
                 'I(logzscore^2)',
                 'income_groupUM',
                 'income_groupH',
                 'logzscore:income_groupUM',
                 'logzscore:income_groupH',
                 'FracInternationalAuthors:income_groupUM',
                 'FracInternationalAuthors:income_groupH',
                 'FracInternationalAuthors:logzscore:income_groupUM',
                 'FracInternationalAuthors:logzscore:income_groupH'
       ),
       dict = c('normalized_frac_top'='Top journal share',
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                "RND_per" = "R&D as % of GDP",
                "NResearchers" = "# of researchers per million",
                'polity2'= 'Polity2',
                'logzscore'='Self-preference',
                "FracInternationalAuthors" = "International collaboration",
                'FracInternationalAuthors:logzscore'='International collaboration × Self-preference',
                'logzscore:income_groupH'='Self-preference × High income',
                'logzscore:income_groupUM'='Self-preference × Upper middle income',
                'income_groupH'='High income',
                'income_groupUM'='Upper middle income',
                'FracInternationalAuthors:income_groupH'='International collaboration × High income',
                'FracInternationalAuthors:income_groupUM'='International collaboration × Upper middle income',
                'FracInternationalAuthors:logzscore:income_groupH'='International collaboration × High income × Self-preference',
                'FracInternationalAuthors:logzscore:income_groupUM'='International collaboration × Upper middle income × Self-preference',
                "n_countries" = "Countries",
                "I(I(logzscore^2))" = "Self-preference square"
                
       )
)


