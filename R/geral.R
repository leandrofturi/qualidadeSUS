library(data.table)
library(stringr)
library(dplyr)

source("R/analise-agrupada.R")
source("R/temporalidade-agrupada.R")
source("R/unicidade-agrupada.R")
source("R/auxiliares.R")
source("R/string-matching.R")

if(!exists("parametros_database"))
  stop("ERRO! parametros_database")

print("Variáveis")
variaveis <- variaveis_tabela(parametros_database) %>% toupper
#' Remoção de variáveis estranhas que aparecem
variaveis <- variaveis[variaveis %in% variaveis_dict(parametros_database)] %>% .[!is.na(.)]


print("Completude, Conformidade e Acurácia")
dt <- diagnostico_agr(variaveis, parametros_database)
if(exists("referencia_subs")) {
  dt_ANO <- diagnostico_sub_agr(variaveis, parametros_database, referencia_subs, "ANO")
  dt_UF <- diagnostico_sub_agr(variaveis, parametros_database, referencia_subs, "UF")
}


if(exists("v_temp")) {
  print("Temporalidade")
  temporalidade_agr(v_temp, parametros_database)
  if(exists("referencia_subs")) {
    temporalidade_sub_agr(v_temp, parametros_database, referencia_subs, "ANO")
    temporalidade_sub_agr(v_temp, parametros_database, referencia_subs, "UF")
  }
}


if(exists("variavel_id") && exists("variaveis_comp")) {
  print("Unicidade")
  unicidade_agr(variavel_id, variaveis_comp, parametros_database)
  freq_identificadores(variavel_id, parametros_database)
  if(exists("referencia_subs")) {
    unicidade_sub_agr(variavel_id, variaveis_comp, referencia_subs, "ANO", parametros_database)
    unicidade_sub_agr(variavel_id, variaveis_comp, referencia_subs, "UF", parametros_database)
  }
}


print("Registros inconformes/inacurados")
v_tmp <- dt$variavel[dt$conformes != dt$nao.nulo | dt$acurados != dt$conformes] %>% .[!is.na(.)]
inconformes_inacurados_agr(v_tmp, parametros_database)
print("Outliers")
outliers_agr(parametros_database)
print("Ignorados")
ignorados_agr(variaveis, parametros_database)

