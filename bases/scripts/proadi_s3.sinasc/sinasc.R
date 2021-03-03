database_name <- "proadi_s3"
table_name <- "sinasc"

#' Construção do dicionário
# source(p"bases/scripts/inicial.R")

# # SINASC/DNSP1999.parquet
referencia_subs <- data.frame("VARIAVEL" = "FILENAME", 
                              "INICIO.ANO" = 12, "QNT.ANO" = 4,
                              "INICIO.UF" = 10, "QNT.UF" = 2)

v_temp <- list(c("DTNASC","DTCADASTRO"),
               c("DTNASC","DTDECLARAC"),
               c("DTNASC","DTREGCART"))

source("bases/scripts/base.R")


# Consistência #################################################################
source("R/athena.R")

# Anual
cons_anual <- list(
  consistencia_query_subs_sql(
    "CAST(\"QTDFILMORT\" AS integer) + CAST(\"QTDFILVIVO\" AS integer) > CAST(\"QTDGESTANT\" AS integer)", 
    referencia_subs, "ANO", database_name, table_name),

  consistencia_query_subs_sql(
    "CAST(\"QTDPARTCES\" AS integer) + CAST(\"QTDPARTNOR\" AS integer) > CAST(\"QTDGESTANT\" AS integer)", 
    referencia_subs, "ANO", database_name, table_name),

  consistencia_query_subs_sql(
    "CAST(\"IDANOMAL\" as varchar) like '1' AND \"CODANOMAL\" LIKE 'NA'", 
    referencia_subs, "ANO", database_name, table_name),
  
  consistencia_data_subs_sql("DTNASC", "DTCADASTRO", dict, referencia_subs, "ANO", 
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTULTMENST", "DTNASC", dict, referencia_subs, "ANO",
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTNASC", "DTRECORIGA", dict, referencia_subs, "ANO",
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTNASC", "DTRECEBIM", dict, referencia_subs, "ANO",
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTNASCMAE", "DTNASC", dict, referencia_subs, "ANO",
                             database_name, table_name),
  
  consistencia_query_subs_sql(
    "(CAST(\"LOCNASC\" AS varchar) LIKE '1' OR CAST(\"LOCNASC\" AS varchar) like '2') 
AND CAST(\"CODESTAB\" AS varchar) LIKE 'NA' AND CAST(SUBSTRING(\"FILENAME\", 12, 4) AS integer) > 2000", 
    referencia_subs, "ANO", database_name, table_name),
  
  consistencia_query_subs_sql(
    "(CAST(\"STCESPARTO\" AS varchar) LIKE '1' OR CAST(\"STCESPARTO\" AS varchar) LIKE '2') AND CAST(\"PARTO\" AS varchar) NOT LIKE '2'",
    referencia_subs, "ANO", database_name, table_name)
)
for(i in seq(cons_anual))
  colnames(cons_anual[[i]]) <- c("ano", paste0("t", i), paste0("T", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "ano"), cons_anual)
write.csv2(dt, paste0(path_to_data, "out/consistencia-ano-sinasc.csv"), row.names = F)

# Estadual
cons_estadual <- list(
  consistencia_query_subs_sql(
    "CAST(\"QTDFILMORT\" AS integer) + CAST(\"QTDFILVIVO\" AS integer) > CAST(\"QTDGESTANT\" AS integer)", 
    referencia_subs, "UF", database_name, table_name),
  
  consistencia_query_subs_sql(
    "CAST(\"QTDPARTCES\" AS integer) + CAST(\"QTDPARTNOR\" AS integer) > CAST(\"QTDGESTANT\" AS integer)", 
    referencia_subs, "UF", database_name, table_name),
  
  consistencia_query_subs_sql(
    "CAST(\"IDANOMAL\" as varchar) like '1' AND \"CODANOMAL\" LIKE 'NA'", 
    referencia_subs, "UF", database_name, table_name),
  
  consistencia_data_subs_sql("DTNASC", "DTCADASTRO", dict, referencia_subs, "UF", 
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTULTMENST", "DTNASC", dict, referencia_subs, "UF",
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTNASC", "DTRECORIGA", dict, referencia_subs, "UF",
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTNASC", "DTRECEBIM", dict, referencia_subs, "UF",
                             database_name, table_name),
  
  consistencia_data_subs_sql("DTNASCMAE", "DTNASC", dict, referencia_subs, "UF",
                             database_name, table_name),
  
  consistencia_query_subs_sql(
    "(CAST(\"LOCNASC\" AS varchar) LIKE '1' OR CAST(\"LOCNASC\" AS varchar) like '2') 
AND CAST(\"CODESTAB\" AS varchar) LIKE 'NA' AND CAST(SUBSTRING(\"FILENAME\", 12, 4) AS integer) > 2000", 
    referencia_subs, "UF", database_name, table_name),
  
  consistencia_query_subs_sql(
    "(CAST(\"STCESPARTO\" AS varchar) LIKE '1' OR CAST(\"STCESPARTO\" AS varchar) LIKE '2') AND CAST(\"PARTO\" AS varchar) NOT LIKE '2'",
    referencia_subs, "UF", database_name, table_name)
)
for(i in seq(cons_estadual))
  colnames(cons_estadual[[i]]) <- c("UF", paste0("t", i), paste0("T", i))

dt <- Reduce(function(x, y) full_join(x, y, by = "UF"), cons_estadual)
write.csv2(dt, paste0(path_to_data, "out/consistencia-uf-sinasc.csv"), row.names = F)


dt <- data.frame("teste"=paste0("T", 1:10),
                 "desc"=c("QTDFILMORT + QTDFILVIVO > QTDGESTANT",
                          "QTDPARTCES + QTDPARTNOR > QTDGESTANT",
                          "IDANOMAL == 1 & CODANOMAL == NULL",
                          "DTNASC > DTCADASTRO",
                          "DTULTMENST > DTNASC",
                          "DTNASC > DTRECORIGA",
                          "DTNASC > DTRECEBIM",
                          "DTNASC > DTNASCMAE",
                          "(LOCNASC == 1 | LOCNASC == 2) & CODESTAB == NULL",
                          "(STCESPARTO == 1 | STCESPARTO == 2) & PARTO != 2"),
                 "desc_completa" = c(
"A quantidade de filhos mortos somados a quantidade de filhos vivos não podem ser maior que a quantidade de gestações anteriores. Obs: A quantidade de gestações pode ser maior porque pode ocorrer aborto.",
"A quantidade de partos cesáreos somados a quantidade de partos normais não podem ser maior que a quantidade de gestações anteriores. Obs: A quantidade de gestações pode ser maior porque pode ocorrer aborto.",
"Se o indicador de anomalia congênita é sim (1), o código da anomalia no CID10 deverá estar preenchida.",
"A data de nascimento não pode ser maior que a data do cadastro da DN no sistema.",
"A data de nascimento não pode ser menor que a data da última menstruação.",
"A data de nascimento não pode ser maior que a data do 1º recebimento do lote.",
"A data de nascimento não pode ser maior que a data do último recebimento do lote.",
"A data de nascimento não pode ser menor que a data de nascimento da mãe.",
"Se o local de nascimento é hospital (1) ou outro estabelecimento de saúde (2), o código do estabelecimento CNES deverá estar preenchido.",
"Se a cesárea ocorreu antes do parto iniciar sim (1) ou não (2) o parto precisa ser cesárea (1)."))
write.csv2(dt, paste0(path_to_data, "out/consistencia-descricao-sinasc.csv"), row.names = F)
