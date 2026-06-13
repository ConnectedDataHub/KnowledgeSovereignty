
rm(list=ls())

library(fixest)
library(dplyr)
library(marginaleffects)
library(ggplot2)

library(data.table)



setwd('/Users/psp2nq/Documents/KnowledgeSovereignty')

colors <- c('#4c72b0', '#dd8452', '#8172b3', '#937860')


##################################
# Load data
##################################

fieldinfo  <- read.csv('data/raw/fields_data/fieldinfo0.csv.gz')
fieldinfo0 <- fieldinfo[fieldinfo$FieldLevel == 1, ]
df_field_names <- fieldinfo[fieldinfo$FieldlevelName == 'fields', c('FieldId', 'FieldName')]

field_hierarchy <- read.csv('data/raw/fields_data/fieldhierarchy0.csv.gz')

domain_id=1
domain_names <- fieldinfo[fieldinfo$FieldLevel == 0, c('FieldId', 'FieldName')]
domain_name <- domain_names$FieldName[domain_names$FieldId == domain_id]
print(domain_name)

field_to_domain <- field_hierarchy[field_hierarchy$ParentFieldId %in% c(1, 2, 3, 4), ]
fields <- field_to_domain[field_to_domain$ParentFieldId == domain_id, ]
field_ids <- fields$ChildFieldId


dfself_fields <- read.csv("data/clean/noselfauthor_fields_R_disruption_03172026.csv.gz")

#######################################
# Tables S14-S17 (SI): Regression estimates of international collaboration on citation self-preference by stages of economic development.
#######################################

name <- paste0(gsub(" ", "", domain_name))

sig=FALSE

model_list <- list()

for (field_id in field_ids) {
  
  print(field_id)
  
  dfself=dfself_fields[dfself_fields$field==field_id, ]
  
  field_name <- df_field_names$FieldName[df_field_names$FieldId == field_id]
  
  if (length(field_name) == 0) field_name <- paste0("field_", field_id)  # fallback
  
  
  
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
  
  
  df_normalized <- dfself %>%
    mutate(across(c(logNumPub, GDP,GDP_PCAP, RND_per, FracInternationalAuthors,normalized_frac_top,
                    logzscore, NResearchers,FracInternationalAuthors,logzscore, polity2, gov_sum_index ), scale))  # Normalize only columns x, y, z
  
  length(unique(df_normalized$Country))
  
  clean_data <- subset(df_normalized, income_group != "")
  clean_data$income_group <- factor(clean_data$income_group)
  clean_data$income_group <- relevel(clean_data$income_group, ref = "LM-L")
  
  m <- feols(FracInternationalAuthors ~ NResearchers + RND_per + logzscore*income_group | Country+Year, data = clean_data, cluster = ~Country)
  
  model_list[[field_name]] <- m
  
}


fitstat_register("n_countries", function(x){
  if(!is.null(x$fixef_sizes) && "Country" %in% names(x$fixef_sizes)){
    x$fixef_sizes["Country"]   # number of unique countries in FE
  } else if("Country" %in% names(x$model_frame)){
    length(unique(x$model_frame$Country))  # fallback
  } else {
    NA
  }
})


names(model_list) <- sapply(as.character(field_ids), function(fid) {
  match_name <- df_field_names$FieldName[df_field_names$FieldId == as.integer(fid)]
  if (length(match_name) == 0) fid else match_name
})

etable(model_list)

etable(model_list, tex = TRUE, digits = 3,  
       file = paste0("output/tables/SI_H1_", filename_suffix, "_04132026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, 
       headers = setNames(names(model_list), names(model_list)),
       replace = TRUE,
       caption = paste0(domain_name,": Regression estimates of international collaboration on citation self-preference by
stages of economic development."),
       label = paste0("tab:H1_", filename_suffix),
       fontsize = "scriptsize",
       order = c('NResearchers',"RND_per","logzscore", "GDP_PCAP", "income_groupUM", 
                 "income_groupH", "logzscore:income_groupUM", 
                 "logzscore:income_groupH", 
                 "!Constant"),
       dict = c('normalized_frac_top' = 'Top journal share',
                'hit_rate'='Hit rate',
                "RND_per" = "R&D as % of GDP",
                "FracInternationalAuthors" = "International collaboration",
                "logzscore" = "Citation self-preference",
                "GDP_PCAP" = "GDP per capita",
                'NResearchers' = "# of researchers per million",
                "income_groupH" = "High income",
                "income_groupUM" = "Upper-middle income",
                "logzscore:income_groupH" = "Citation self-preference × High income",
                "logzscore:income_groupUM" = "Citation self-preference × Upper-middle income",
                "n_countries" = "Countries"),
       placement = "H",arraystretch = 0.7 )

##################################
# Figures S14-S17 (SI): Marginal Effects of self-preference on international collaboration by World Bank income groups.
#################################

library(marginaleffects)
library(ggplot2)

# Compute marginal effects for each model in model_list
mfx_list <- lapply(names(model_list), function(field_name) {
  mfx <- slopes(model_list[[field_name]],
                variables = "logzscore",
                by = "income_group")
  mfx$field <- field_name
  as.data.frame(mfx)
})

mfx_all <- do.call(rbind, mfx_list)

mfx_all$income_group <- factor(mfx_all$income_group, levels = c("H", "UM", "LM-L"))
mfx_all$field <- factor(mfx_all$field, levels = names(model_list))

# Build "A. FieldName" strip labels
field_letter_map <- setNames(
  gsub(" and ", " & ", paste0(LETTERS[seq_along(names(model_list))], ". ", names(model_list))),
  names(model_list)
)
mfx_all$field_labeled <- factor(
  field_letter_map[as.character(mfx_all$field)],
  levels = field_letter_map
)

income_labels <- c("LM-L" = "Low & lower-middle",
                   "UM"   = "Upper-middle",
                   "H"    = "High")

ggplot(mfx_all, aes(x = income_group, y = estimate, color = income_group)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 1.2) +
  coord_flip() +
  scale_color_manual(values = c("LM-L" = '#DA6437', "UM" = '#2171b5', "H" = '#8D95A0')) +
  scale_x_discrete(labels = income_labels) +
  labs(y = "Marginal effect of citation self-preference",
       x = "Income group") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid        = element_blank(),
    panel.border      = element_blank(),
    axis.line.x       = element_line(color = "black"),
    axis.line.y       = element_line(color = "black"),
    axis.ticks.x      = element_line(color = "black"),
    axis.ticks.y      = element_line(color = "black"),
    axis.title        = element_text(size = 20),
    axis.text         = element_text(size = 17),
    strip.text        = element_text(size = 19, face = "bold"),
    panel.spacing.x   = unit(2, "lines"),
    panel.spacing.y   = unit(2, "lines"),
    legend.position   = "none"
  ) +
  facet_wrap(~ field_labeled, ncol = 5, axes = "all", axis.labels = "margins",
             labeller = label_wrap_gen(width = 15))

filename <- paste0("output/figures/SI_H1_marginal_effects_by_income_", filename_suffix, ".pdf")
ggsave(filename, width = 5 * 4, height = 4 * ceiling(length(model_list) / 5), dpi = 300, device = "pdf")

