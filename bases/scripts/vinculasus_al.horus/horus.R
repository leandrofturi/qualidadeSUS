database_name <- "vinculasus_al"
table_name <- "horus"

referencia_subs <- data.frame("VARIAVEL" = "DT_ATENDIMENTO",
                              "INICIO.ANO" = 1, "QNT.ANO" = 4,
                              "INICIO.MES" = 6, "QNT.MES" = 2)
                              #2018-05
# Temporalidade
v_temp <- list(c("dt_receita", "dt_atendimento"))

# Unicidade
variavel_id <- "id_paciente"
variaveis_comp <- c("dt_nascimento", "tp_sexo")


source("R/relatorio-athena.R")


# Consistência #################################################################
source("R/consistencia-athena.R")
parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)

cons_anual <- list(
  consistencia_data_subs("dt_receita", "dt_atendimento",
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_nascimento", "dt_receita", 
                         referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dt_nascimento", "dt_atendimento", 
                         referencia_subs, "ANO", parametros_database),
  consistencia_query_subs(c("ds_tipo_produto", "sg_tipo_produto"),
                          "\"ds_tipo_produto\" LIKE 'MEDICAMENTO' AND (NOT \"sg_tipo_produto\" LIKE 'M')", 
                          referencia_subs, "ANO", parametros_database),
  consistencia_query_subs(c("ds_tipo_produto", "sg_tipo_produto"),
                          "\"ds_tipo_produto\" LIKE 'PRODUTO PARA SAÚDE' AND (NOT \"sg_tipo_produto\" LIKE 'I')", 
                          referencia_subs, "ANO", parametros_database)
)
for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("T", i), paste0("t", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
fwrite(dt, "consistencia-ano.csv")

testes_cons <- data.frame("teste" = c("T1", "T2", "T3", "T4", "T5"),
                          "desc" = c("dt_receita > dt_atendimento",
                                     "dt_nascimento > dt_receita",
                                     "dt_nascimento > dt_atendimento",
                                     "ds_tipo_produto == MEDICAMENTO & sg_tipo_produto != M",
                                     "ds_tipo_produto == PRODUTO PARA SAÚDE & sg_tipo_produto != I"),
                                     "desc_completa" = c("A data de emissão do paciente não pode ser maior que a data de atendimento do paciente",
                                     "A data de nascimento do paciente não pode ser maior que a data de emissão do paciente",
                                     "A data de nascimento do paciente não pode ser maior que a data de atendimento do paciente",
                                     "Se a descrição do tipo de produto for \\textit{MEDICAMENTO}, a sigla do tipo de produto precisa ser \\textit{M}",
                                     "Se a descrição do tipo de produto for \\textit{PRODUTO PARA SAÚDE}, a sigla do tipo de produto precisa ser \\textit{I}"))
fwrite(testes_cons, "consistencia-descricao.csv")

install.packages("kableExtra")
source("API/relatorio.R")