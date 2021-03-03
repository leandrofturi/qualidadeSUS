rm(list = ls())

database_name <- "vinculasus_al"
table_name <- "sia_raas_ad"

# AE27000001N201908.DTS
referencia_subs <- data.frame("VARIAVEL" = "CO_REMESSA",
                              "INICIO.ANO" = 12, "QNT.ANO" = 4,
                              "INICIO.MES" = 16, "QNT.MES" = 2)

# Temporalidade
v_temp <- list(c("DT_INICIO", "DT_FIM"), 
               c("DT_INICIO", "DT_COMPETENCIA"), 
               c("DT_COMPETENCIA", "DT_FIM"))

# Unicidade
variavel_id <- "ID_PACIENTE"
variaveis_comp <- c("DT_NASCIMENTO", "DS_SEXO")

parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)

source("R/relatorio-athena.R")

# Consistência #################################################################
source("R/consistencia-athena.R")

cons_anual <- list(
  consistencia_data_subs("DT_REALIZACAO", "DT_FIM",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("DT_INICIO", "DT_REALIZACAO",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("DT_INICIO", "DT_FIM",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("DT_NASCIMENTO", "DT_INICIO",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("DT_NASCIMENTO", "DT_FIM",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("DT_NASCIMENTO", "DT_REALIZACAO",
                         referencia_subs, "ANO", parametros_database)
)
for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("T", i), paste0("t", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
fwrite(dt, "consistencia-ano.csv")

testes_cons <- data.frame("teste" = c("T1", "T2", "T3", "T4", "T5","T6"),
                          "desc" = c("DT_REALIZACAO > DT_FIM",
                                     "DT_REALIZACAO < DT_INICIO",
                                     "DT_FIM < DT_INICIO",
                                     "DT_NASCIMENTO > DT_INICIO",
                                     "DT_NASCIMENTO > DT_FIM",
                                     "DT_NASCIMENTO > DT_REALIZACAO"),
                          "desc_completa" = c(
"A data de realização não pode ser maior que a data final",
"A data de realização não pode ser menor que a data de inicio",
"A data final não pode ser menor que a data inicial",
"A data de nascimento não pode ser maior que a data de início",
"A data de nascimento não pode ser maior que a data de fim",
"A data de nascimento não pode ser maior que a data de realização"
                          ))

fwrite(testes_cons, "consistencia-descricao.csv")

install.packages("kableExtra")
source("API/relatorio.R")