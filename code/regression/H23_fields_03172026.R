
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

domain_id=4
domain_names <- fieldinfo[fieldinfo$FieldLevel == 0, c('FieldId', 'FieldName')]
domain_name <- domain_names$FieldName[domain_names$FieldId == domain_id]
print(domain_name)

field_to_domain <- field_hierarchy[field_hierarchy$ParentFieldId %in% c(1, 2, 3, 4), ]
fields <- field_to_domain[field_to_domain$ParentFieldId == domain_id, ]
field_ids <- fields$ChildFieldId


dfself_fields <- read.csv("data/clean/noselfauthor_fields_R_disruption_03172026.csv.gz")

#######################################
# Tables S18-S21 (SI)
#######################################

name <- paste0("hitrate_", gsub(" ", "", domain_name))

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
  
  m <- feols(hit_rate ~  GDP_PCAP + RND_per + NResearchers  + FracInternationalAuthors*logzscore  | Country+Year, data = df_normalized, cluster = ~Country)
  
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

etable(model_list, tex = TRUE, digits = 3,  
       file = paste0("output/tables/SI_H2_", filename_suffix, "_03172026.tex"),
       fitstat = ~ n + n_countries + f + r2 + ar2, 
       headers = setNames(names(model_list), names(model_list)),
       replace = TRUE,
       caption = paste0(domain_name,": Two-way fixed effects panel regression estimates of hit rate on international
collaboration and citation self-preference."),
       label = paste0("tab:H2_", filename_suffix),
       fontsize = "scriptsize",
       order = c('NResearchers',"logzscore", "GDP_PCAP", "income_groupUM", 
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

