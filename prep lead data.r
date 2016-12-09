library(readr)
library(dplyr)
library(tidyr)

wd <- 'C:/Box Sync/jlittel/lead risk test'
setwd(wd)

# ----------------------------------------------------------------
# ----   read data -----------------------------------------------


# rap_data <- read_csv('rap_data_Q4_15_05.24.16.csv')


# Run script to load and process data
  wd_mod <- paste0("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/",
    "Risk Profile/PD Model/2.Model Selection", sep="")
  setwd(wd_mod)
  source('Preprocess data file.r') # Cleans and creates variables

rap_data_dir <- paste0('C:/Box Sync/Risk Appetite - Provisioning Project/',
	'Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs')
setwd(rap_data_dir)
pd_data  <- read_csv('pds_08.19.16.csv')

df.lead <- pd_data %>%
  select(RC.Opp.Number, pd, pd_one_year = pd_adjusted_pre_tenor) %>%
  right_join(df.rap)


# ----------------------------------------------------------------
# ----   prep data -----------------------------------------------

# pre process

df.lead <- preProcess(train)

qualification_cutoff <- 0.10

df.lead$preq_lgl <- df.lead$pd_one_year < qualification_cutoff
df.lead$preq <- factor(as.numeric(df.lead$preq_lgl),
	levels = 0:1,
	labels = c('not_qualify', 'qualify'))

prop.table(table(df.lead$preq_lgl))