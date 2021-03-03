library(dplyr)
library(bdacodeR)
library(reticulate)
#use_condaenv("notebook", required=TRUE)


#' Leitura dos dados agrupados pelas quantidades de cada registro
#' @param variavel. Variável de leitura
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @return data.table. Colunas variavel e total
coluna_agregada <- function(variavel, parametros_database) {
  names(parametros_database) <- tolower(names(parametros_database))
  q <- paste0(
"SELECT \"", variavel, "\" AS \"variavel\",",
" COUNT(\"", variavel, "\") AS \"total\"",
" FROM ", parametros_database$database_name, ".", parametros_database$table_name,
" GROUP BY \"", variavel, "\"",
" ORDER BY \"total\" DESC")
  data <- get_athena_data(parametros_database$database_name, q, show_time=T)
  if(is.null(data)) return(NULL)
  
  data[data == "NA" | data == "NULL" | data == "null"] <- NA
  data[, variavel := as.character(variavel)]
  data[, total := as.numeric(total)]
  return(data)
}


#' Leitura dos dados agrupados pelas quantidades de cada registro, 
#'   a partir da comparação de substrings de variavel_ref
#' @param variavel. Variável de leitura
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param variavel_ref character. Variável utilizada como referência. e.g.: "FILENAME"
#' @param inicio_ref numeric. Posição inicial de ref
#' @param ref character. Substring a ser analisada. e.g. "ES"
#' @return data.table. Colunas variavel e total
coluna_ref_agregada <- function(variavel, parametros_database,
                                variavel_ref, inicio_ref, ref) {
  names(parametros_database) <- tolower(names(parametros_database))
  q <- paste0("substring(CAST(\"", variavel_ref, "\" AS varchar), ", inicio_ref,", ", nchar(ref), ") LIKE '", ref, "'", 
              collapse=", ")
  q <- paste0(
"SELECT \"", variavel, "\" AS \"variavel\",",
" COUNT(\"", variavel, "\") AS \"total\"",
" FROM ", parametros_database$database_name, ".", parametros_database$table_name,
" WHERE ", q, 
" GROUP BY \"", variavel, "\"",
" ORDER BY \"total\" DESC")
  data <- get_athena_data(parametros_database$database_name, q, show_time=T)
  if(is.null(data)) return(NULL)
  
  data[data == "NA" | data == "NULL" | data == "null"] <- NA
  data[, variavel := as.character(variavel)]
  data[, total := as.numeric(total)]
  return(data)
}


#' Variáveis de uma tabela de dados
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @return character. Vetor contendo os nomes das colunas
variaveis_tabela <- function(parametros_database) {
  names(parametros_database) <- tolower(names(parametros_database))
  get_athena_data(parametros_database$database_name,
                 paste0("select * from ", parametros_database$database_name, ".",
                        parametros_database$table_name, " limit 1")) %>% 
    colnames %>% toupper
}


#' Leitura dos dados agrupados pelas quantidades de cada registro, 
#'   a partir de valores armazenados referentes a um identificador de paciente
#' @param variavel_id character. Variável contendo o identificador do paciente.
#' e.g. "ID_PACIENTE"
#' @param variaveis_comp character. Variáveis contendo informações do paciente.
#' e.g. c("DT_NASCIMENTO", SEXO") 
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @return data.table. Colunas variavel_id, variaveis_comp e total
coluna_unicidade_agregada <- function(variavel_id, variaveis_comp, parametros_database) {
  names(parametros_database) <- tolower(names(parametros_database))
  database_name <- parametros_database$database_name
  table_name <- parametros_database$table_name
  
  variavel_id <- toupper(variavel_id)
  x <- toupper(variaveis_comp)
  x <- paste0("CAST(", database_name, ".", table_name, ".", x, " AS VARCHAR)", collapse = ", '*', ")
  
  data <- get_athena_data(database_name,
                            paste0(
"SELECT CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR) , '*', ",  x, ") AS \"variavel\", ",
"       COUNT(CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR), '*', ", x, "))  AS \"total\"",
" FROM (",
"       SELECT \"", variavel_id, "\"",
"       FROM ", database_name, ".", table_name,
"       GROUP BY \"", variavel_id, "\"",
"       HAVING COUNT(\"", variavel_id, "\") > 1) tmp",
" INNER JOIN ", database_name, ".", table_name, " ON tmp.", variavel_id, " = ", database_name, ".", table_name, ".", variavel_id,
" GROUP BY CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR), '*', ", x, ")",
" ORDER BY \"total\" DESC"))
  
  nms <- make.unique(c(variavel_id, variaveis_comp), sep="_")
  data[, (nms) := tstrsplit(variavel, "*", fixed=T, fill=NA, keep=c(1, seq(length(variaveis_comp))+1))]
  data$variavel <- NULL
  data[data == "NA" | data == "NULL" | data == "null"] <- NA
  
  options(scipen=999)
  data <- data[, lapply(.SD, function(x) 
    if(all(is.finite(as.numeric(x))))
      as.character(as.numeric(x))
    else 
      as.character(x))]
  data[, total := as.numeric(total)]
  
  setcolorder(data, c(nms,"total"))
  return(data)
}


#' Leitura dos dados agrupados pelas quantidades de cada registro, 
#'   a partir de valores armazenados referentes a um identificador de paciente
#' @param variavel_id character. Variável contendo o identificador do paciente.
#' e.g. "ID_PACIENTE"
#' @param variaveis_comp character. Variáveis contendo informações do paciente.
#' e.g. c("DT_NASCIMENTO", SEXO") 
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param variavel_ref character. Variável utilizada como referência. e.g.: "FILENAME"
#' @param inicio_ref numeric. Posição inicial de ref
#' @param ref character. Substring a ser analisada. e.g. "ES"
#' @return data.table. Colunas variavel_id, variaveis_comp e total
coluna_ref_unicidade_agregada <- function(variavel_id, variaveis_comp, parametros_database,
                                          variavel_ref, inicio_ref, ref) {
  names(parametros_database) <- tolower(names(parametros_database))
  database_name <- parametros_database$database_name
  table_name <- parametros_database$table_name
  
  variavel_id <- toupper(variavel_id)
  x <- toupper(variaveis_comp)
  x <- paste0("CAST(", database_name, ".", table_name, ".", x, " AS VARCHAR)", collapse = ", '*', ")
  
  y <- paste0("substring(CAST(\"", variavel_ref, "\" AS varchar), ", inicio_ref,", ", nchar(ref), ") LIKE '", ref, "'", 
              collapse=", ")
  
  data <- get_athena_data(database_name,
                            paste0(
"SELECT CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR) , '*', ",  x, ") AS \"variavel\", ",
"       COUNT(CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR), '*', ", x, "))  AS \"total\"",
" FROM (",
"       SELECT \"", variavel_id, "\"",
"       FROM ", database_name, ".", table_name,
"       WHERE ", y,
"       GROUP BY \"", variavel_id, "\"",
"       HAVING COUNT(\"", variavel_id, "\") > 1) tmp",
" INNER JOIN ", database_name, ".", table_name, " ON tmp.", variavel_id, " = ", database_name, ".", table_name, ".", variavel_id,
" GROUP BY CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR), '*', ", x, ")",
" ORDER BY \"total\" DESC"))
  
  nms <- make.unique(c(variavel_id, variaveis_comp), sep="_")
  data[, (nms) := tstrsplit(variavel, "*", fixed=T, fill=NA, keep=c(1, seq(length(variaveis_comp))+1))]
  data$variavel <- NULL
  data[data == "NA" | data == "NULL" | data == "null"] <- NA
  
  options(scipen=999)
  data <- data[, lapply(.SD, function(x) 
    if(all(is.finite(as.numeric(x))))
      as.character(as.numeric(x))
    else 
      as.character(x))]
  data[, total := as.numeric(total)]
  
  setcolorder(data, c(nms,"total"))
  return(data)
}


#' Coluna agrupada usada para fins de comparação entre datas, 
#'   agrupado pela quantidade de registros
#' @param x character.
#' @param y character.
#' Variáveis de comparação: y - x
#' @param parametros_database list. Parâmetros para leitura dos dados
#' #' @return data.table. Colunas: day_diff, total
coluna_temp_agregada <- function(x, y, parametros_database) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  database_name <- parametros_database$database_name
  table_name <- parametros_database$table_name
  
  x <- toupper(x)
  y <- toupper(y)
  
  data <- get_athena_data(database_name,
                            paste0(
"SELECT date_diff('day', tmp.", x, ", tmp.", y, ") AS \"day_diff\",",
"       COUNT(date_diff('day', tmp.", x, ", tmp.", y, ")) AS \"total\"",
" FROM (", 
"           SELECT TRY(date_parse(CAST(\"", x, "\" AS VARCHAR), '", D$formatos[[x]], "')) AS \"", x, "\", ",
"                  TRY(date_parse(CAST(\"", y, "\" AS VARCHAR), '", D$formatos[[y]], "')) AS \"", y, "\"",
"           FROM ", database_name, ".", table_name,
") tmp",
" WHERE (tmp.", x, " >= date_parse('", D$referencias[[x]][1], "','", D$formatos[[x]], "') AND ",
"        tmp.", x, " <= date_parse('", D$referencias[[x]][2], "','", D$formatos[[x]], "') AND ",
"        tmp.", y, " >= date_parse('", D$referencias[[y]][1], "','", D$formatos[[y]], "') AND ", 
"        tmp.", y, " <= date_parse('", D$referencias[[y]][2], "','", D$formatos[[y]], "'))", 
" GROUP BY date_diff('day', tmp.", x, ", tmp.", y, ")", 
" ORDER BY \"total\" DESC"
                            ))
  data <- data[, lapply(.SD, as.numeric)]
  if(!grepl("%d", D$formatos[[x]]) || !grepl("%d", D$formatos[[y]]))
    data <- data[data$day_diff >= 31 | data$day_diff <= -31, ]
  
  return(data)
}


#' Coluna agrupada usada para fins de comparação entre datas, 
#'   agrupado pela quantidade de registros
#' @param x character.
#' @param y character.
#' Variáveis de comparação: y - x
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param variavel_ref character. Variável utilizada como referência. e.g.: "FILENAME"
#' @param inicio_ref numeric. Posição inicial de ref
#' @param ref character. Substring a ser analisada. e.g. "ES"
#' #' @return data.table. Colunas: day_diff, total
coluna_ref_temp_agregada <- function(x, y, parametros_database, 
                                     variavel_ref, inicio_ref, ref) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  database_name <- parametros_database$database_name
  table_name <- parametros_database$table_name
  
  x <- toupper(x)
  y <- toupper(y)
  
  data <- get_athena_data(database_name,
                            paste0(
"SELECT date_diff('day', tmp.", x, ", tmp.", y, ") AS \"day_diff\",",
"       COUNT(date_diff('day', tmp.", x, ", tmp.", y, ")) AS \"total\"",
" FROM (", 
"           SELECT TRY(date_parse(CAST(\"", x, "\" AS VARCHAR), '", D$formatos[[x]], "')) AS \"", x, "\", ",
"                  TRY(date_parse(CAST(\"", y, "\" AS VARCHAR), '", D$formatos[[y]], "')) AS \"", y, "\"",
"           FROM ", database_name, ".", table_name,
"           WHERE substring(CAST(\"", variavel_ref, "\" AS varchar), ", inicio_ref,", ", nchar(ref), ") LIKE '", ref, "'",
") tmp",
" WHERE (tmp.", x, " >= date_parse('", D$referencias[[x]][1], "','", D$formatos[[x]], "') AND ",
"        tmp.", x, " <= date_parse('", D$referencias[[x]][2], "','", D$formatos[[x]], "') AND ",
"        tmp.", y, " >= date_parse('", D$referencias[[y]][1], "','", D$formatos[[y]], "') AND ", 
"        tmp.", y, " <= date_parse('", D$referencias[[y]][2], "','", D$formatos[[y]], "'))", 
" GROUP BY date_diff('day', tmp.", x, ", tmp.", y, ")", 
" ORDER BY \"total\" DESC"
                            ))
  data <- data[, lapply(.SD, as.numeric)]
  if(!grepl("%d", D$formatos[[x]]) || !grepl("%d", D$formatos[[y]]))
    data <- data[data$day_diff >= 31 | data$day_diff <= -31, ]
  
  return(data)
}


#' Leitura do dicionário
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param referencia_subs list. Vide "analise-agrupada.R"
#' @param atualizar_datas boolean. Atualizar as datas contidas no dicionário?
#' @return data frame. Dicionário de dados, vide "dicionario.R"
leitura_dict <- function(parametros_database, referencia_subs=NULL, atualizar_datas=F) {
  path <- paste0("bases/scripts/", parametros_database$database_name, ".", parametros_database$table_name, 
                 "/dict-", parametros_database$table_name, ".csv")
  if(!file.exists(path)) {
    path <- paste0(getwd() %>% str_remove(., "/out$"), "/dict-", parametros_database$table_name, ".csv")
    if(!file.exists(path))
      stop("ERRO! Dicionário não encontrado!")
  }
  
  dict <- read.csv2(path, stringsAsFactors = F)
  #' Remoção de acentos 
  colnames(dict) <- stringi::stri_trans_general(str = colnames(dict), 
                                                id = "Latin-ASCII") %>% toupper
  path_cnv <- paste0("bases/dicionarios/cnv/", parametros_database$table_name, "/")
  if(!all(is.na(dict$VARIANTES)))
    dict$VARIANTES[str_detect(dict$VARIANTES, ".CNV$|.cnv$")] <- 
    paste0(path_cnv, dict$VARIANTES[str_detect(dict$VARIANTES, ".CNV$|.cnv$")])
  
  if(atualizar_datas)
    dict <- atualizar_datas_dict(parametros_database, referencia_subs)
  
  return(dict)
}
