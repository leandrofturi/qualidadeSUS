rm(list = ls())

database_name <- "vinculasus_al"
table_name <- "sia_bpai"

# AE27000001N201908.DTS
referencia_subs <- data.frame("VARIAVEL" = "REMESSA",
                              "INICIO.ANO" = 12, "QNT.ANO" = 4,
                              "INICIO.MES" = 16, "QNT.MES" = 2)

# Temporalidade
v_temp <- list(c("DT_ATENDIMENTO", "COMPETENCIA"))

# Unicidade
variavel_id <- "ID_PACIENTE"
variaveis_comp <- c("DT_NASCIMENTO", "SEXO")

parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)

source("R/relatorio-athena.R")


# Consistência #################################################################
source("R/consistencia-athena.R")

cons_anual <- list(
  consistencia_data_subs("DT_NASCIMENTO", "DT_ATENDIMENTO",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("DT_NASCIMENTO", "CO_BPI_MVM",
                         referencia_subs, "ANO", parametros_database),
  consistencia_query_subs("CAST(\"CO_BPI_TPFIN\" AS VARCHAR) LIKE '1' AND (NOT \"DS_TP_FINANCIAMENTO\" LIKE 'PAB')",
                          referencia_subs, "ANO", parametros_database),
  consistencia_query_subs("CAST(\"CO_BPI_TPFIN\" AS VARCHAR) LIKE '2' AND (NOT \"DS_TP_FINANCIAMENTO\" LIKE 'MAC')",
                          referencia_subs, "ANO", parametros_database),
  consistencia_query_subs("CAST(\"CO_BPI_TPFIN\" AS VARCHAR) LIKE '3' AND (NOT \"DS_TP_FINANCIAMENTO\" LIKE 'FAEC')",
                          referencia_subs, "ANO", parametros_database)
)
for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("T", i), paste0("t", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
fwrite(dt, "consistencia-ano.csv")

testes_cons <- data.frame("teste" = c("T1", "T2", "T3", "T4", "T5"),
                          "desc" = c("DT_NASCIMENTO > DT_ATENDIMENTO",
                                     "DT_NASCIMENTO > CO_BPI_MVM",
                                     "co_bpi_tpfin like 1 and ds_tp_financiamento != PAB",
                                     "co_bpi_tpfin like 2 and ds_tp_financiamento != MAC",
                                     "co_bpi_tpfin like 3 and ds_tp_financiamento != FAEC"),
                          "desc_completa" = c(
"A data de nascimento não pode ser maior que a data de atendimento",
"A data de nascimento não pode ser maior que a data de movimentação",
"Se o código do tipo de financiamento for 1 (Procedimento PAB) então a Descrição do tipo de financiamento não pode ser diferente de PAB",
"Se o código do tipo de financiamento for 2 (Procedimento MAC) então a Descrição do tipo de financiamento não pode ser diferente de MAC",
"Se o código do tipo de financiamento for 3 (Procedimento FAEC) então a Descrição do tipo de financiamento não pode ser diferente de FAEC"))

fwrite(testes_cons, "consistencia-descricao.csv")

install.packages("kableExtra")
source("API/relatorio.R")
