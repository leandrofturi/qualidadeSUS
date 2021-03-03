rm(list = ls())

database_name <- "vinculasus_al"
table_name <- "aih"

# HM27043001N201507.DTS
referencia_subs <- data.frame("VARIAVEL" = "NO_ARQ_REMESSA",
                              "INICIO.ANO" = 12, "QNT.ANO" = 4,
                              "INICIO.MES" = 16, "QNT.MES" = 2)

parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)
# Temporalidade
v_temp <- list(c("dt_internacao", "dt_saida")
               ,c("dt_emissao", "dt_lote_apres"),
               c("dt_emissao", "dt_cmpt"))
# Unicidade
variavel_id <- "id_paciente"
variaveis_comp <- c("co_paciente_nacionalidade","co_paciente_sexo","dt_paciente_nascimento","co_raca","raca")


source("R/relatorio-athena.R")


# Consistência #################################################################
source("R/consistencia-athena.R")
parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)
cons_anual <- list(
  consistencia_data_subs("dt_emissao","dt_cmpt",  
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_paciente_nascimento", "dt_emissao",  
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_emissao","dt_lote_apres",   
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_saida", "dt_cmpt",  
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_internacao", "dt_cmpt",  
                             referencia_subs,  "ANO",parametros_database),
  consistencia_data_subs("dt_saida","dt_lote_apres",   
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_internacao", "dt_lote_apres",  
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_paciente_nascimento", "dt_saida",  
                             referencia_subs,  "ANO",parametros_database),
  consistencia_data_subs("dt_internacao", "dt_saida",  
                             referencia_subs,  "ANO",parametros_database)
)
for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("T", i), paste0("t", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
fwrite(dt, "consistencia-ano.csv")

testes_cons <- data.frame("teste" = c(paste0("T", seq(cons_anual))),
                          "desc" = c("dt_emissao > dt_cmp",
                                     "dt_paciente_nascimento > dt_emissao",
                                     "dt_emissao > dt_lote_apres",
                                     "dt_saida > dt_cmp",
                                     "dt_internacao > dt_cmp",
                                     "dt_saida > dt_lote_apres",
                                     "dt_internacao > dt_lote_apres",
                                     "dt_paciente_nascimento > dt_saida",
                                     "dt_internacao > dt_saida"),
                          "desc_completa" = c(
"A data de competência não pode ser menor que a data de emissão da AIH",
"A data de nascimento do paciente não pode ser maior que a data de emissão da AIH",
"A data de competência de apresentação da AIH não pode ser menor que a data de emissão da AIH",
"A data de competência não pode ser menor que a data de saída do paciente",
"A data de competência não pode ser menor que a data de internação do paciente",
"A data de competência de apresentação da AIH não pode ser menor que a data de saída do paciente",
"A data de competência de apresentação da AIH não pode ser menor que a data de internação",
"A data de nascimento do paciente não pode ser maior que a data de saída do paciente",
"A data de internação não pode ser maior que a data de saída do paciente"))

fwrite(testes_cons, "consistencia-descricao.csv")

install.packages("kableExtra")
source("API/relatorio.R")

