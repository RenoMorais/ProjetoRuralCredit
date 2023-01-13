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
wd_workf<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01.DATA/BCB/Credito_RuraL_SICOR_RECOR/")


################# Get data with a+pi OLINDA of BCB ##############

#Downlaod subsources: Contratos de Custeio por Município e Produto
#Date download: 11/01/2023
cred_bacen <- httr::GET("https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/CusteioMunicipioProduto?$top=3000000&$format=json&$select=Municipio,nomeProduto,AnoEmissao,cdPrograma,cdSubPrograma,cdFonteRecurso,cdTipoSeguro,cdEstado,VlCusteio,cdProduto,codCadMu,Atividade,cdModalidade,codIbge,AreaCusteio")

"comentar nomes diferentes nas variáveis"

"baixar as finalidades restantes do credito rural"

#Downlaod subsources: Contratos de Investimento por Município e Produto
#Date download: 11/01/2023
cred_bacen2 <- httr::GET("https://olinda.bcb.gov.br/olinda/servico/SICOR/versao/v2/odata/InvestMunicipioProduto?$format=json&$select=Municipio,nomeProduto,AnoEmissao,cdPrograma,cdSubPrograma,cdFonteRecurso,cdTipoSeguro,cdEstado,VlCusteio,cdProduto,cdMunicipio,Atividade,cdModalidade,AreaInvest")


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

data_fact <- arq_raw[[2]]

data_fact2 <- arq_raw2[[2]]

data_fact3 <- arq_raw3[[2]]

length(data_fact)

#df <- data.frame(matrix(unlist(arq_raw), nrow=length(data_fact), byrow=TRUE))

"teste de conversão para tentar obter as colunas"

data_fact <-
  data.table::rbindlist(data_fact) #deu certo!!!

data_fact2 <-
  data.table::rbindlist(data_fact2)

data_fact3 <-
  data.table::rbindlist(data_fact3)
############## get data after process of download from api ##############

data_custeio <- readRDS(paste0(wd_workf,"credit.rds"))

data_invest <- readRDS(paste0(wd_workf,"contractbyinvest_clear2.rds"))

################### clear data #####################

data_custeio_clear <- data_custeio %>% 
  janitor::clean_names(.) %>% 
  mutate(municipio=str_to_lower(municipio))%>%
  mutate(municipio=stri_trans_general(str = municipio, id = "Latin-ASCII")) %>% 
  mutate(nome_produto=str_to_lower(nome_produto))%>%
  mutate(nome_produto=stri_trans_general(str = nome_produto, id = "Latin-ASCII")) %>% 
  mutate(vl_custeio = as.numeric(vl_custeio)) %>% #important to add decimal values
  mutate (value_billion = vl_custeio/1000000000) %>%  #important to visualization into tables
  rename(cd_municipio = )

data_custeio_clear$nome_produto <- str_replace(data_custeio_clear$nome_produto, '\\"',"") #nao funciona dentro do encadeamento de pipe

data_invest_clear <- data_invest %>% 
  janitor::clean_names(.) %>% 
  mutate(municipio=str_to_lower(municipio))%>%
  mutate(municipio=stri_trans_general(str = municipio, id = "Latin-ASCII")) %>% 
  mutate(nome_produto=str_to_lower(nome_produto))%>%
  mutate(nome_produto=stri_trans_general(str = nome_produto, id = "Latin-ASCII")) %>% 
  mutate(vl_custeio = as.numeric(vl_custeio)) %>% 
  mutate (value_billion = vl_custeio/1000000000)

data_invest_clear$nome_produto <- str_replace(data_invest_clear$nome_produto, '\\"',"")

################## compare columns ########

namescolunas_custeio <- as.data.frame(colnames(data_custeio))

namescolunas_invest <- as.data.frame(colnames(data_invest)) 

colnames(data_fact3)

# rename("colnames(arq_clear)" = "colnames(arq_clear2)")


cols_common <- inner_join(namescolunas_invest, namescolunas_custeio)

"comentar sobre diferença de colunas"

############# includes a variable that identify where is from a data########

data_custeio_clear <- data_custeio_clear %>% mutate(contrato = "custeio")

data_invest <- data_invest %>% mutate(contrato = "investimento")

################## join datas ###########

planilha_teste <- rbind.fill(data_custeio_clear, data_invest)

###################### export to files ###############

saveRDS(data_invest,paste0(wd_workf,"contractbyinvest_clear2.rds"))

saveRDS(data_custeio_clear,paste0(wd_workf,"credit.rds"))

saveRDS(data_fact3,paste0(wd_workf,"contract_comerc_indust.rds"))

saveRDS(planilha_teste,paste0(wd_workf,"mergedata.rds"))



