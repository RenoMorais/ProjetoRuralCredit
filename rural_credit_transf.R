##################

# Author : Renan Morais
# Date: 16.01.2023
# Email: renanflorias@hotmail.com
# Goal: Some transformation with datas of rural credit


########################### Libraries ######################################
install.packages("pacman")

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               deflateBR,
               httr,
               rjson,
               magrittr, 
               tibble, 
               Matrix,
               data.table,
               pivottabler)

########################### Directories (variable) ########################################
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
wd_workf<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01.DATA/BCB/3.Temp files/")


########################## get data ################################

data_custeio <- readRDS(paste0(wd_workf,"credit.rds"))

data_invest <- readRDS(paste0(wd_workf,"contractbyinvest_clear2.rds"))

#################### transform values and create columns ###################

data_custeio <- data_custeio %>% 
mutate(value_billion = vl_custeio/1000000000) %>%  #important to visualization into tables
dplyr::rename(cd_municipio = cod_cad_mu)


data_invest <- data_invest %>% 
mutate(value_billion = vl_custeio/1000000000)


"Include a variable that will help us to distiguinsh each other"

data_custeio <- data_custeio %>% mutate(contrato = "custeio")

data_invest <- data_invest %>% mutate(contrato = "investimento")


################## join datas ###########

planilha_teste <- rbind.fill(data_custeio, data_invest)

###################### export to files rds ###############

saveRDS(data_invest,paste0(wd_workf,"contractbyinvest_transf.rds"))

saveRDS(data_custeio,paste0(wd_workf,"contractbycusteio_transf.rds"))

saveRDS(planilha_teste,paste0(wd_workf,"mergedata.rds"))
