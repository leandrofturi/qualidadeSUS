library(data.table)
library(stringr)
library(dplyr)

source("R/qualidade.R")
source("R/dicionario.R")
source("R/leitura-data.R")
source("R/analise-agrupada.R")
source("R/temporalidade-agrupada.R")
source("R/unicidade-agrupada.R")
source("R/auxiliares.R")
source("R/string-matching.R")
source("R/funcoes-filtro.R")

if(!exists("parametros_database"))
  stop("ERRO! parametros_database")

print("Variáveis")
variaveis <- variaveis_tabela(parametros_database) %>% toupper
#' Remoção de variáveis estranhas que aparecem
variaveis <- variaveis[variaveis %in% variaveis_dict(parametros_database)] %>% .[!is.na(.)]

print("Completude, Conformidade e Acurácia")
dt <- diagnostico_filtro_agr(variaveis, parametros_database, 
                             filtro, out=F)
if(exists("referencia_subs")) {
  dt_ANO <- diagnostico_sub_agr_filtro(variaveis, parametros_database, referencia_subs, "ANO", filtro)
  dt_UF <- diagnostico_sub_agr_filtro(variaveis, parametros_database, referencia_subs, "UF",filtro)
}


if(exists("v_temp")) {
  print("Temporalidade")
  temporalidade_agr_filtro(v_temp, parametros_database,filtro)
  if(exists("referencia_subs")) {
    temporalidade_sub_agr_filtro(v_temp, parametros_database, referencia_subs, "ANO",filtro)
    temporalidade_sub_agr_filtro(v_temp, parametros_database, referencia_subs, "UF",filtro)
  }
}

if(exists("variavel_id") && exists("variaveis_comp")) {
  print("Unicidade")
  unicidade_agr_filtro(variavel_id, variaveis_comp,filtro, parametros_database)
  freq_identificadores_filtro(variavel_id, parametros_database,filtro)
  if(exists("referencia_subs")) {
    unicidade_sub_agr_filtro(variavel_id, variaveis_comp, referencia_subs, "ANO", parametros_database,filtro)
    unicidade_sub_agr_filtro(variavel_id, variaveis_comp, referencia_subs, "UF", parametros_database,filtro)
  }
}


print("Registros inconformes/inacurados")
v_tmp <- dt$variavel[dt$conformes != dt$nao.nulo | dt$acurados != dt$conformes] %>% .[!is.na(.)]
inconformes_inacurados_agr_filtro(v_tmp, parametros_database,filtro)
print("Outliers")
outliers_agr_filtro(parametros_database,filtro)
print("Ignorados")
ignorados_agr_filtro(variaveis, parametros_database,filtro)


