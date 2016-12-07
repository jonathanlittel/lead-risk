library(readr)
library(dplyr)
library(tidyr)

wd <- 'C:/Box Sync/jlittel/lead risk test'
setwd(wd)

# ----------------------------------------------------------------
# ----   read data -----------------------------------------------

rap_data_dir <- paste0('C:/Box Sync/Risk Appetite - Provisioning Project/',
	'Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs')
setwd(rap_data_dir)
rap_data <- read_csv('rap_data_Q4_15_05.24.16.csv')
pd_data  <- read_csv('pds_08.19.16.csv')

df <- pd_data %>%
  select(RC.Opp.Number, pd, pd_one_year = pd_adjusted_pre_tenor) %>%
  right_join(rap_data)

setwd(wd)

# prep data


