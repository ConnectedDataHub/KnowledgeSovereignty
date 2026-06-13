
rm(list=ls())

library(fixest)
library(dplyr)
library(marginaleffects)
library(ggplot2)

colors <- c('#4c72b0', '#dd8452', '#8172b3', '#937860')

setwd('/Users/psp2nq/Documents/KnowledgeSovereignty')

##################################
# Load data
##################################

name <- "bootstrap_noselfauthor"  # or "noselfauthor"
sig=FALSE


dfself <- read.csv("data/clean/bootstrap_noselfauthor_R_12272025.csv")


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


df_normalized <- dfself %>%
  mutate(across(c(logNumPub, GDP,GDP_PCAP, RND_per, FracInternationalAuthors,normalized_frac_top,
                  logzscore, NResearchers,FracInternationalAuthors,logzscore, polity2, gov_sum_index ), scale))  # Normalize only columns x, y, z

length(unique(df_normalized$Country))


#######################################
# Table 2 (main paper): Regression estimates of international collaboration on citation self-preference by stages of economic development.
#######################################

m1 <- feols(FracInternationalAuthors ~ NResearchers + RND_per + logzscore, data = df_normalized)
m2 <- feols(FracInternationalAuthors ~ NResearchers + RND_per +logzscore | Year, data = df_normalized)
m3 <- feols(FracInternationalAuthors ~ NResearchers + RND_per +logzscore | Country+Year, data = df_normalized, cluster = ~Country)
m4 <- feols(FracInternationalAuthors ~ NResearchers + RND_per + logzscore * GDP_PCAP | Country+Year, data = df_normalized, cluster = ~Country)

clean_data <- subset(df_normalized, income_group != "")
clean_data$income_group <- factor(clean_data$income_group)
clean_data$income_group <- relevel(clean_data$income_group, ref = "LM-L")

m5 <- feols(FracInternationalAuthors ~ NResearchers + RND_per + logzscore*income_group | Country+Year, data = clean_data, cluster = ~Country)


fitstat_register("n_countries", function(x){
  if(!is.null(x$fixef_sizes) && "Country" %in% names(x$fixef_sizes)){
    x$fixef_sizes["Country"]   # number of unique countries in FE
  } else if("Country" %in% names(x$model_frame)){
    length(unique(x$model_frame$Country))  # fallback
  } else {
    NA
  }
})




etable(m1,m2,m3,m4,m5)

etable(m1,m2,m3,m4,m5, tex = TRUE, digits = 3,  
       file = paste0("output/tables/H1_", filename_suffix, "_03112026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, 
       replace = TRUE,
       caption = "Regression estimates of international collaboration on citation self-preference by stages of
economic development.",
       order = c('NResearchers',"logzscore", "GDP_PCAP", "income_groupUM", 
                 "income_groupH", "logzscore:income_groupUM", 
                 "logzscore:income_groupH", 
                 "!Constant"),
       dict = c("FracInternationalAuthors" = "International collaboration",
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                'NResearchers' = "# of researchers per million",
                "RND_per" = "R&D as % of GDP",
                "income_groupH" = "High income",
                "income_groupUM" = "Upper-middle income",
                "logzscore:income_groupH" = "Citation self-preference × High income",
                "logzscore:income_groupUM" = "Citation self-preference × Upper-middle income",
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7 )


# Countries in data but not in the model
# Correct
m2$fixef_sizes["Country"]

# To get just the number
as.integer(m3$fixef_sizes["Country"])

# Check dropped countries
countries_in_model <- names(fixef(m3)$Country)
dropped <- setdiff(unique(df_normalized$Country), countries_in_model)
length(dropped)


# Check if they're singletons
df_normalized %>%
  filter(Country %in% dropped) %>%
  count(Country) %>%
  arrange(n)

# For a single model
as.integer(m3$fixef_sizes["Country"])

# Across all models
models <- list(m1, m2, m3, m4, m5)
sapply(models, function(m) {
  if ("Country" %in% names(m$fixef_sizes)) {
    as.integer(m$fixef_sizes["Country"])
  } else {
    NA
  }
})


##################################
# Figure 1 (main paper): Marginal Effects of self-preference on international collaboration by World Bank income groups. 
#################################

library(marginaleffects)
library(ggplot2)

mfx_by_group <- slopes(m5, 
                       variables = "logzscore",
                       by = "income_group")

print(unique(mfx_by_group$income_group))

ggplot(mfx_by_group, aes(x = reorder(income_group, -estimate), y = estimate)) +
  geom_point(aes(color = income_group), size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = income_group), 
                width = 0.2, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", alpha = 1, size=1.5) +
  coord_flip() +
  scale_color_manual(values = c( '#DA6437', '#2171b5','#8D95A0')) +
  scale_x_discrete(labels = c("LM-L" = "Low & lower-middle",
                              "UM" = "Upper-middle",
                              "H" = "High")) +
  labs(y = "Marginal effect",
       x = "Income group") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),           
    panel.border = element_blank(),         
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_line(color = "black"),  
    axis.ticks.x = element_line(color = "black"), 
    axis.ticks.y = element_line(color = "black"), 
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 23),
    legend.position = "none"
  )


filename = paste0("output/figures/H1_marginal_effects_by_income_", filename_suffix, ".pdf")

ggsave(filename, width = 10, height = 6, dpi = 300, device = "pdf")


##################################
# Figure S6 (SI): Marginal effect of citation self-preference on collaboration at different levels of GDP per capita 
##################################


mfx_gdp <- slopes(m4, variables = "logzscore",
                  newdata = datagrid(GDP_PCAP = seq(min(df_normalized$GDP_PCAP, na.rm = TRUE),
                                                    max(df_normalized$GDP_PCAP, na.rm = TRUE),
                                                    length.out = 100)))

p1_gdp <- ggplot(mfx_gdp, aes(x = GDP_PCAP, y = estimate)) +
  geom_line(size = 2, color = '#2171b5') +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = '#2171b5') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", alpha = 1, size=2) +
  labs(#title = "Marginal Effect of International Collaboration on Publication Quality",
    #subtitle = "A",
    x = "GDP per capita",
    y = "Marginal effect of \n self-preference",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    panel.border = element_blank(),         # Remove panel border
    axis.line.x = element_line(color = "black"),  # Add bottom axis line
    axis.line.y = element_line(color = "black"),  # Add left axis line
    axis.ticks.x = element_line(color = "black"), # Add bottom ticks
    axis.ticks.y = element_line(color = "black"), # Add left ticks
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    #plot.title = element_text(size = 30, face = "bold"),
    axis.text.y = element_text(size = 30),
    plot.subtitle = element_text(size = 30))

print(p1_gdp)

filename = paste0("output/figures/SI_H1_marginal_effect_selfVSgdp_", filename_suffix, ".pdf")

ggsave(filename, width = 10, height = 8, dpi = 300, device = "pdf")


##################################
# Figure S7 (SI): VIF for the regression
##################################

var_labels <- c(
  "GDP_PCAP"                           = "GDP per capita",
  "RND_per"                            = "R&D as % of GDP",
  'income_group'                       = 'Income group',
  "NResearchers"                       = "# of researchers per million",
  'polity2'                            = 'Polity2',
  "FracInternationalAuthors"           = "International collaboration",
  "logzscore"                          = "Citation self-preference",
  "FracInternationalAuthors:logzscore" = "International collaboration × Self-preference"
)



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
    labs(x = "Variables", y = "VIF", title = title) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid   = element_blank(),
      panel.border = element_blank(),
      axis.line.x  = element_line(color = "black", linewidth = 0.3),
      axis.line.y  = element_line(color = "black", linewidth = 0.3),
      axis.ticks.x = element_line(color = "black", linewidth = 0.3),
      axis.ticks.y = element_line(color = "black", linewidth = 0.3),
      axis.title   = element_text(size = 10),
      axis.text    = element_text(size = 10),
      plot.title   = element_text(size = 11, face = "bold")
    )
  
  #ggsave(filename, plot = p, width = 12, height = 6, dpi = 300, device = "pdf")
  return(p)
}

vif1 <-plot_vif("FracInternationalAuthors ~ NResearchers + RND_per + logzscore*income_group",
                df_normalized,'predictor', "A",colors[1], "vif_top_journal.pdf")

vif2 <-plot_vif("FracInternationalAuthors ~ NResearchers + RND_per + logzscore*GDP_PCAP",
                df_normalized,'predictor', "B",colors[2], "vif_top_journal.pdf")

library(patchwork)
(vif1 | vif2)

ggsave(
  'output/figures/SI_ColSelf_vif_plots.pdf',
  plot = (vif1 | vif2),
  width  = 180,
  height = 50,
  units  = 'mm',
  device = 'pdf'
)

print(vif2)

