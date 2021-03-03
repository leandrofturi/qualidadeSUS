library(dplyr)
library(data.table)
library(bdacodeR)
library(reticulate)
use_condaenv("notebook", required=TRUE)

source("R/dicionario.R")
source("R/leitura-data.R")



#' Consistência anual entre datas (para geral basta somar)
#' @param V1 character
#' @param V2 character
#' Teste: V1 > V2
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2,
#'                               "inicio.uf" = 10, "qnt.uf" = 2)
#' @param sub character. "ANO || UF"
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @return data.table. Colunas total, falhas, sub
consistencia_data_subs <- function(V1, V2, referencia_subs, sub, 
                                   parametros_database) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  names(referencia_subs) <- toupper(names(referencia_subs))
  referencia_subs$VARIAVEL <- toupper(as.character(referencia_subs$VARIAVEL))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  subs <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)$variavel
  subs <- substr(subs, referencia_subs[[s[1]]], referencia_subs[[s[1]]]+referencia_subs[[s[2]]]-1) %>% 
    unique %>% sort
  
  dt <- lapply(subs, function(w) {
    print(paste(toupper(V1), toupper(V2), w))
    data <- coluna_ref_temp_agregada(toupper(V1), toupper(V2), parametros_database, 
                                     referencia_subs$VARIAVEL, referencia_subs[[s[1]]], w)
    if(is.null(data) || nrow(data) == 0)
      return(c(NA, NA))
    
    c <- sum(data$total[data$day_diff < 0])
    t <- sum(data$total)
    return(c(t, c))
  }) %>% do.call(rbind.data.frame, .) %>% `colnames<-`(c("total", "falhas"))
  dt[[sub]] <- subs
  setcolorder(dt, c(sub, "total","falhas"))
  
  return(dt)
}


#' Consistência anual entre uma query (para geral basta somar)
#' @param query character.
#' @param variaveis_query character[]. variáveis envolvidas na query
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2,
#'                               "inicio.uf" = 10, "qnt.uf" = 2)
#' @param sub character. "ANO || UF"
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @return data.table. Colunas total, falhas, sub
consistencia_query_subs <- function(variaveis, query, referencia_subs, sub, 
                                    parametros_database) {
  names(parametros_database) <- tolower(names(parametros_database))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  names(referencia_subs) <- toupper(names(referencia_subs))
  variaveis_query <- str_extract_all(query, "\"[:graph:]+\"", simplify = T) %>% 
    str_replace_all(., "\"|\"", "")
  
  TT <- total_nao_nulos(variaveis_query, referencia_subs, sub, parametros_database)
  
  tt <- get_athena_data(database_name,
                          paste0(
"SELECT COUNT(CASE WHEN (TRY(", query, ")) THEN 1 END) AS \"falhas\",",
"       substring(\"", referencia_subs$VARIAVEL, "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ") AS \"", sub, "\"",
" FROM ", parametros_database$database_name, ".", parametros_database$table_name,
" GROUP BY substring(\"", referencia_subs$VARIAVEL, "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")",
" ORDER BY substring(\"", referencia_subs$VARIAVEL, "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")"
                          ))
  dt <- full_join(tt, TT, by = sub)
  setcolorder(dt, c(sub, "total","falhas"))
  
  return(dt)
}


#' Total de registros em que são ambos não nulos em variaveis
#' Usando para consistências
#' @param variaveis character[].
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2)
#' @param sub character. "ANO || UF"
#' @param parametros_database list. Parâmetros para leitura dos dados
total_nao_nulos <- function(variaveis, referencia_subs, sub, parametros_database) {
  q_comp <- paste0(
    " (NOT (CAST(\"", variaveis, "\" AS VARCHAR) LIKE 'NA')) AND",
    " (NOT (CAST(\"", variaveis, "\" AS VARCHAR) LIKE 'NULL')) AND",
    " (NOT (CAST(\"", variaveis, "\" AS VARCHAR) LIKE 'null')) AND",
    " (NOT \"", variaveis, "\" IS NULL) ", collapse = " AND ")
  
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  names(referencia_subs) <- toupper(names(referencia_subs))
  t <- get_athena_data(database_name,
                         paste0(
"SELECT COUNT(CASE WHEN", q_comp, "THEN 1 END) AS \"total\",", 
"       substring(\"", as.character(referencia_subs$VARIAVEL), "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ") AS \"", sub, "\"",
" FROM ", database_name, ".", table_name,
" GROUP BY substring(\"", as.character(referencia_subs$VARIAVEL), "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")",
" ORDER BY substring(\"", as.character(referencia_subs$VARIAVEL), "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")"
                         ))
  t[["total"]] <- as.numeric(t[["total"]])
  t[[sub]] <- as.character(t[[sub]])
  return(t)
}
