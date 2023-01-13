##################

# Author : Renan Morais
# Date: 11.01.2023
# Email: renanflorias@hotmail.com
# Goal: Some analysis of rural credits


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
wd_workf<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01.DATA/BCB/Credito_RuraL_SICOR_RECOR/")

########################### Directories (variable) ########################################

data_custeio <- readRDS(paste0(wd_workf,"credit.rds"))

data_invest <- readRDS(paste0(wd_workf,"contractbyinvest_clear2.rds"))

data_merge <- readRDS(paste0(wd_workf,"mergedata.rds"))

########## include variable "billions" #########

data_custeio <- data_custeio %>% 
  mutate (value_billion = vl_custeio/1000000000)

data_invest <- data_invest %>% 
  mutate (value_billion = vl_custeio/1000000000)

################# analysis custeio ############

"link para guia de visualização"

"https://www.bcb.gov.br/estabilidadefinanceira/micrrural"

" visualização agregada por ano"

ptc <- PivotTable$new()

ptc$addData(data_custeio)
ptc$addColumnDataGroups("cd_modalidade")
ptc$addRowDataGroups("ano_emissao", totalCaption = "Total")
ptc$defineCalculation(calculationName = "Totais por anos", summariseExpression = "sum(value_billion)",format= "%.2f ")
ptc$renderPivot()

"visualização agregada por nome do produto"

ptc <- PivotTable$new()

ptc$addData(data_custeio)
ptc$addRowDataGroups("nome_produto", totalCaption = "Total")
ptc$defineCalculation(calculationName = "Totais por anos", summariseExpression = "sum(value_billion)",format= "%2.2f ")
ptc$renderPivot()

################# analysis custeio ############

"filter para análise"

data_filter <- data_invest %>% 
  filter(nome_produto == '"outras maquinas"', atividade == 1)%>% 
  group_by(ano_emissao,nome_produto) %>% 
  select(value_billion) %>%
  summarise("Total por ano" = sum(value_billion))


pti <- PivotTable$new()

pti$addData(data_invest)
pti$addColumnDataGroups("nome_produto", fromData = FALSE,
                        explicitListOfValues=list('trator','outras maquinas') ,
                        visualTotals= FALSE)
pti$addColumnDataGroups("atividade", visualTotals = FALSE)
pti$addRowDataGroups("ano_emissao")
pti$defineCalculation(calculationName = "Totais por anos", summariseExpression = "sum(value_billion)",format= "%.2f ")
pti$theme <- "largeplain"
pti$renderPivot(styleNamePrefix="t2")

"comentar qual a visualização eu tomei como referencia para comparar os valores"

                        