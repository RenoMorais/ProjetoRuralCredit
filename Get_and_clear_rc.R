##################

# Author : Renan Morais
# Date: 09.01.2023
# Email: renanflorias@hotmail.com
# Goal: get rural credits of bcb


########################### Libraries ######################################
install.packages("pacman")

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               httr,
               rjson,
               magrittr, 
               tibble, 
               Matrix,
               data.table,
               XML,
               xml2,
               jsonlite,
               purrr,
               tidyr,
               dplyr,
               plyr)

########################### Directories (variable) ########################################


root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
wd_workf<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01.DATA/BCB/3.Temp files/")


################# Get data with a+pi OLINDA of BCB ##############

#Downlaod subsources: Contratos de Custeio por Município e Produto
#Date download: 11/01/2023


cred_bacen <- httr::GET("https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/CusteioMunicipioProduto?$top=3000000&$format=json&$select=Municipio,nomeProduto,AnoEmissao,cdPrograma,cdSubPrograma,cdFonteRecurso,cdTipoSeguro,cdEstado,VlCusteio,cdProduto,codCadMu,Atividade,cdModalidade,codIbge,AreaCusteio")

"comentar nomes diferentes nas variáveis"

#Downlaod subsources: Contratos de Investimento por Município e Produto
#Date download: 11/01/2023

cred_bacen2 <- httr::GET("https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/InvestMunicipioProduto?$format=json&$select=Municipio,nomeProduto,AnoEmissao,cdPrograma,cdSubPrograma,cdFonteRecurso,cdTipoSeguro,cdEstado,VlCusteio,cdProduto,cdMunicipio,Atividade,cdModalidade,AreaInvest")


"OBS: Em ambas as bases (custeio e investimento por municip e produto) não está disponível a variável quantidade de contratos,
provavelmente por sigilo bancário."

#Downlaod subsources: Custeio Investimento Comercial Industrial
#essa base será utilizada para consultar valores por finalidade ou municipio em relação ao total do credito rural
#Date download: 13/01/2023

rc_indus_comerc <- httr::GET("https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/CusteioInvestimentoComercialIndustrialSemFiltros?&$format=json&$select=cdEstado,nomeUF,cdMunicipio,Municipio,AnoEmissao,cdPrograma,cdSubPrograma,cdFonteRecurso,Atividade,QtdCusteio,VlCusteio,QtdInvestimento,VlInvestimento,QtdComercializacao,VlComercializacao,QtdIndustrializacao,VlIndustrializacao,codMunicIbge,AreaCusteio,AreaInvestimento")


"OBS: Em ambas as bases não está disponível a variável quantidade,
provavelmente por sigilo bancário."

#this step is necessary to convert a list into matrix.
arq_raw <- httr::content(cred_bacen)

arq_raw2 <- httr::content(cred_bacen2)

arq_raw3 <- httr::content(rc_indus_comerc)


################## reducing a list with goal of obtain a oficial list (without description in elements) - necessary if data is a json###############

data_custeio <- arq_raw[[2]]

data_invest <- arq_raw2[[2]]

data_fact3 <- arq_raw3[[2]]


"teste de conversão para tentar obter as colunas"

data_custeio <-
  data.table::rbindlist(data_custeio) #deu certo!!!

data_invest <-
  data.table::rbindlist(data_invest)

data_fact3 <-
  data.table::rbindlist(data_fact3)
############## get data after process of download from api ##############

data_custeio <- readRDS(paste0(wd_workf,"credit.rds"))

data_invest <- readRDS(paste0(wd_workf,"contractbyinvest_clear2.rds"))

################### clear data #####################

data_custeio <- data_custeio %>% 
  janitor::clean_names(.) %>% 
  mutate(municipio=str_to_lower(municipio))%>%
  mutate(municipio=stri_trans_general(str = municipio, id = "Latin-ASCII")) %>% 
  mutate(nome_produto=str_to_lower(nome_produto))%>%
  mutate(nome_produto=stri_trans_general(str = nome_produto, id = "Latin-ASCII")) %>% 
  mutate(vl_custeio = as.numeric(vl_custeio)) #important to add decimal values

data_custeio$nome_produto <- str_replace(data_custeio$nome_produto, '\\"',"") #nao funciona dentro do encadeamento de pipe

data_invest <- data_invest %>% 
  janitor::clean_names(.) %>% 
  mutate(municipio=str_to_lower(municipio))%>%
  mutate(municipio=stri_trans_general(str = municipio, id = "Latin-ASCII")) %>% 
  mutate(nome_produto=str_to_lower(nome_produto))%>%
  mutate(nome_produto=stri_trans_general(str = nome_produto, id = "Latin-ASCII")) %>% 
  mutate(vl_custeio = as.numeric(vl_custeio))

data_invest_clear$nome_produto <- str_replace(data_invest_clear$nome_produto, '\\"',"")

###################### export to files ###############

saveRDS(data_invest,paste0(wd_workf,"contractbyinvest_clear2.rds"))

saveRDS(data_custeio,paste0(wd_workf,"credit.rds"))




