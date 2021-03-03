rm(list = ls())

database_name <- "vinculasus_al"
table_name <- "sim"

#' Construção do dicionário
# source("bases/scripts/inicial.R")

referencia_subs <- data.frame("VARIAVEL" = "DTOBITO",
                              "INICIO.ANO" = 1, "QNT.ANO" = 4)

parametros_database <- list("database_name"=database_name,
                            "table_name"=table_name)
# Temporalidade
v_temp <- list(c("dtobito", "dtcadastro"),
               c("dtobito", "dtinvestig"),
               c("dtobito", "dtrecebim"))

# Unicidade
variavel_id <- "id_paciente"
variaveis_comp <- c("dtnasc","numerodo","horaobito","sexo","lococor","idade","racacor","codpaisres","dtobito")

source("R/relatorio-athena.R")

# Consistência #################################################################
source("R/consistencia-athena.R")


cons_anual <- list(
  consistencia_data_subs("dtobito", "dtrecorig",  
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dtobito", "dtcadastro",  
                             referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("obitopuerp","sexo"),
                          "(\"obitopuerp\" = 1 OR \"obitopuerp\" = 2 OR  \"obitopuerp\" = 3) AND (\"sexo\" = 1)" ,
                          referencia_subs, "ANO",parametros_database),                    
  consistencia_data_subs("dtobito", "dtrecoriga",  
                             referencia_subs,  "ANO",parametros_database),
  consistencia_data_subs("dtobito", "dtregcart", 
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dtobito", "dtatestado",  
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dtobito", "dtrecebim", 
                             referencia_subs, "ANO", parametros_database),
  consistencia_data_subs("dtobito", "dtinvestig", 
                             referencia_subs,  "ANO", parametros_database),
  consistencia_query_subs(c("lococor","codestab"),
                          "(\"lococor\" != 1 AND \"lococor\" != 2) AND ( \"codestab\" IS NOT NULL )",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("obitoparto","dtobito","dtnasc"),
                          "\"obitoparto\" = 2 AND ( \"dtobito\" != \"dtnasc\" )",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("obitoparto","dtobito","dtnasc"),
                          "\"obitoparto\" = 1 AND ( \"dtobito\" > \"dtnasc\")",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("obitograv","sexo"),
                          "\"obitograv\" = 1 AND ( \"sexo\" = 1)",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("atestante","comunsvoim"),
                          "(\"atestante\" != 3 AND \"atestante\" != 4) AND ( \"comunsvoim\" IS NOT NULL)",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("esc","idade"),
                          "(\"esc\" = 2 AND \"idade\" < 401) AND (\"esc\" = 3 AND \"idade\" < 404)  AND (\"esc\" = 4 AND \"idade\" < 408)  AND (\"esc\" = 5 AND \"idade\" < 412)",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("esc","idade"),
                          "(\"esc\" = 2 AND \"idade\" < 401) AND (\"esc\" = 3 AND \"idade\" < 404)  AND (\"esc\" = 4 AND \"idade\" < 408)  AND (\"esc\" = 5 AND \"idade\" < 412)",
                          referencia_subs, "ANO",parametros_database),
  consistencia_data_subs("dtnasc", "dtobito", 
                             referencia_subs,  "ANO", parametros_database),
  consistencia_query_subs(c("escmae","idademae"),
                          "(\"escmae\" = 2 AND \"idademae\" < 1) AND (\"escmae\" = 3 AND \"idademae\" < 4)  AND (\"escmae\" = 4 AND \"idademae\" < 8)  AND (\"escmae\" = 5 AND \"idademae\" < 2)",
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("tipobito","dtobito","dtnasc"),
                          "\"tipobito\" = 1 AND (\"dtobito\" != \"dtnasc\")", 
                          referencia_subs, "ANO",parametros_database),
  consistencia_query_subs(c("tppos","dtinvestig"),
                          "(\"tppos\" LIKE 'N') AND (\"dtinvestig\" IS NOT NULL)",
                          referencia_subs, "ANO",parametros_database)
)
for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("T", i), paste0("t", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
fwrite(dt, "consistencia-ano.csv")


testes_cons <- data.frame("teste" = c("T1", "T2", "T3", "T4", "T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16","T17","T18"),
                          "desc" = c("dtobito > dtrecorig",
                                     "dtobito > dtcadastro",
                                     "obitopuerp == 1|2|3 & sexo == 1",
                                     "dtobito > dtrecoriga",
                                     "dtobito > dtregcart",
                                     "dtobito > dtatestado",
                                     "dtobito > dtrecebim",
                                     "dtobito > dtinvestig",
                                     "codestab !=null & lococor != 1&2",
                                     "obitoparto == 2 & dtnasc != dtobito",
                                     "obitoparto == 1 & dtnasc < dtobito",
                                     "obitograv == 1 & sexo == 1",
                                     "comunsvoim != null & atestante != 3&4",
                                     "esc > idade",
                                     "dtobito < dtnasc",
                                     "escmae > idademae",
                                     "tipobito == 1 & dtnasc != dtobito",
                                     "tppos == ‘N’ & dtinvestig != null"),
                          "desc_completa"=c(
"A data de óbito não pode ser maior que a data do recebimento original",
" A data de óbito não pode ser maior que a data do cadastro",
"Se óbito no puerpério é 1(Sim, até 42 dias após o parto) ou 2(Sim, de 43 dias a 1 ano) ou 3(não), o sexo não ser 1(Masculino)",
"A data de óbito não pode ser maior que a data do recebimento original",
"A data de óbito não pode ser maior que a data do registro no cartório",
"A data de óbito não pode ser maior que a data do atestado",
"A data de óbito não pode ser maior que a data de recebimento",
"A data de óbito não pode ser maior que a data de investigação",
"Se o código do estabelecimento existe, o local de ocorrência do óbito não pode ser diferente de 1 (hospital) ou 2 (outros estabelecimentos de saúde)",
"Se o óbito do paciente for 2 (ocorreu durante o parto), a data de nascimento precisa ser igual a data de óbito",
"Se o óbito do paciente for 1 (ocorreu antes do parto), a data de nascimento não pode ser menor que a data de óbito",
" Se óbito na gravidez  é 1 (sim), o sexo não ser 1 (Masculino)",
"Se código do município do SVO ou IML estiver preenchido, atestante precisa ser 3 (IML) ou 4 (SVO)",
"A escolaridade precisa estar em acordo com a idade, por exemplo se a escolaridade for 2 (1 a 3 anos), a idade precisa ser superior a 3 anos",
"A data de óbito não pode ser menor que a data de nascimento",
"A escolaridade da mãe precisa estar em acordo com a sua idade, por exemplo se a escolaridade for 2 (1 a 3 anos), a idade precisa ser superior a 3 anos",
"Se o tipo do óbito é 1 (fetal), a data de nascimento precisa ser a mesma da data de óbito",
"Se o óbito investigado é N (Não) a data de investigação não pode estar preenchida."))

fwrite(testes_cons, "consistencia-descricao.csv")

install.packages("kableExtra")
source("API/relatorio.R")