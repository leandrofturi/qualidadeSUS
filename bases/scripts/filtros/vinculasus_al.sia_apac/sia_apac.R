rm(list = ls())

database_name <- "vinculasus_al"
table_name <- "sia_apac"

# AE27000001N201908.DTS
referencia_subs <- data.frame("VARIAVEL" = "REMESSA",
                              "INICIO.ANO" = 12, "QNT.ANO" = 4,
                              "INICIO.MES" = 16, "QNT.MES" = 2)

# Temporalidade
v_temp <- list(c("DATA_INICIO", "DATA_FIM"), 
               c("DATA_INICIO", "COMPETENCIA"), 
               c("COMPETENCIA", "DATA_FIM"))

filtro <- list("VARIAVEL"="CNES", "VAL"="2006499")

# Unicidade
variavel_id <- "ID_PACIENTE"

variaveis_comp <- c("DT_NASCIMENTO", "SEXO")

parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)

source("R/relatorio-athena-filtro.R")

# Consistência #################################################################
source("R/funcoes-filtro.R")
cons_anual <- list(
  consistencia_data_subs_filtro("DATA_SOLIC", "DATA_GERACAO",
                         referencia_subs, "ANO", parametros_database,filtro),
  consistencia_data_subs_filtro("DT_NASCIMENTO", "DATA_SOLIC",  
                         referencia_subs, "ANO", parametros_database,filtro),
  consistencia_data_subs_filtro("DT_NASCIMENTO", "DATA_GERACAO",  
                         referencia_subs, "ANO", parametros_database,filtro),
  consistencia_data_subs_filtro("DT_NASCIMENTO", "DATA_INICIO",  
                         referencia_subs, "ANO", parametros_database,filtro),
  consistencia_data_subs_filtro("DT_NASCIMENTO", "DATA_FIM",  
                         referencia_subs, "ANO", parametros_database,filtro),
  consistencia_data_subs_filtro("DT_NASCIMENTO", "DATA_OCORR",  
                         referencia_subs, "ANO", parametros_database,filtro))

for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("T", i), paste0("t", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
fwrite(dt, "consistencia-ano.csv")


testes_cons <- data.frame("teste" = c("T1", "T2", "T3", "T4", "T5","T6"),
                          "desc" = c("DATA_SOLIC > DATA_GERACAO",
                                     "DT_NASCIMENTO > DATA_SOLIC",
                                     "DT_NASCIMENTO > DATA_GERACAO",
                                     "DT_NASCIMENTO > DATA_INICIO",
                                     "DT_NASCIMENTO > DATA_FIM",
                                     "DT_NASCIMENTO > DATA_OCORR"),
                          "desc_completa" = c(
"A data de solicitação não pode ser maior que a data de geração da remessa",
"A data de nascimento não pode ser maior que a data de solicitação",
"A data de nascimento não pode ser maior que a data de geração",
"A data de nascimento não pode ser maior que a data de início",
"A data de nascimento não pode ser maior que a data de fim",
"A data de nascimento não pode ser maior que a data de ocorrência"))

fwrite(testes_cons, "consistencia-descricao.csv")

install.packages("kableExtra")
source("API/relatorio-filtro.R")
