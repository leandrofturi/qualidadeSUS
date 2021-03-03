library(data.table)
library(stringr)
library(dplyr)

source("R/dicionario.R")
source("R/leitura-data.R")


#' Levantamento de domínio de table_name
#' São também calculados tamanhos, classe, porcentagem de registros e completude
#' @param variaveis character. Variáveis a serem analisadas
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção do ano dos registros.
#' @param database_name character.
#' @param table_name character.
#' Saídas escritas em "dominio-unique-athena.csv"
tabela_dominio <- function(variaveis, referencia_subs, database_name, table_name) {
  if(length(variaveis) == 0) return(NULL)
  names(referencia_subs) <- toupper(names(referencia_subs))
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  
  variaveis <- sort(variaveis)
  dts <- lapply(variaveis, function(x) {
    print(x)
    q <- paste0(
      "SELECT CASE WHEN COUNT(DISTINCT \"", x, "\") <= 30 THEN array_join(array_sort(array_agg(DISTINCT CAST(\"", x, "\" AS varchar))), ', ', 'NA') END AS \"dominio\", ",
      " COUNT(DISTINCT \"", x, "\") AS \"distinct\", ",
      " MIN(LENGTH(CAST(\"", x, "\" AS varchar))) AS \"nchar_min\", ",
      " MAX(LENGTH(CAST(\"", x, "\" AS varchar))) AS \"nchar_max\", ",
      " (COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) LIKE 'NA') THEN 1 END) + ",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) LIKE 'NULL') THEN 1 END) +",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) LIKE 'null') THEN 1 END) +",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) IS NULL) THEN 1 END)) = ",
      " (COUNT(\"", x, "\") + ",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) IS NULL) THEN 1 END)) AS \"null\", ",
      " substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ") AS \"ano\"",
      " FROM ", database_name, ".", table_name,
      " GROUP BY substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ")",
      " ORDER BY substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ")")
    
    dt <- get_athena_data(database_name, query = q)
    dt <- dt[, lapply(.SD, as.character)]
    return(split(dt, as.character(dt$ano)))
  }) %>% `names<-`(variaveis)
  
  ano <- get_athena_data(database_name, 
                         paste0(
                           "SELECT substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ") AS \"ano\"",
                           " FROM ", database_name, ".", table_name,
                           " GROUP BY substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ")")) %>% 
    .[["ano"]] %>% sort
  
  dt <- lapply(variaveis, function(x)
    lapply(ano, function(y) c("variavel"=x, dts[[x]][[y]])) %>% rbindlist) %>% rbindlist
  
  dict <- leitura_dict(parametros_database)
  dict$VARIAVEL <- toupper(dict$VARIAVEL)
  
  # TRUE em apenas as variáveis que ocorreram alterações no domínio
  dt[["mud_dom"]] <- lapply(variaveis, function(x) {
    n <- length(dt$dominio[dt$variavel == x])
    if(is.null(dict$VARIANTES))
      return(rep(FALSE, n))
    else if(is.na(dict$VARIANTES[dict$VARIAVEL == x]) || dict$VARIANTES[dict$VARIAVEL == x] == "")
      return(rep(FALSE, n))
    
    s <- str_split(dt$dominio[dt$variavel == x], ", ")
    i <- Reduce(intersect, s[!sapply(s, function(x) any(is.na(x)))])
    
    if(length(i) <= 0) i <- NULL
    s <- sapply(s, function(x) !all(x %in% i))
    s[1] <- TRUE
    return(s)
  }) %>% unlist
  
  # TRUE em apenas as variáveis que ocorreram alterações no tamanho
  dt[["mud_nchar"]] <- lapply(variaveis, function(x) {
    n <- length(dt$dominio[dt$variavel == x])
    if(is.null(dict$VARIANTES))
      return(rep(FALSE, n))
    else if(is.na(dict$VARIANTES[dict$VARIAVEL == x]) || dict$VARIANTES[dict$VARIAVEL == x] == "")
      return(rep(FALSE, n))
    
    s <- c(0, dt$nchar_max[dict$VARIAVEL == x][-length(dt$nchar_max[dict$VARIAVEL == x])]) %>% as.numeric
    s <- (as.numeric(dt$nchar_max[dict$VARIAVEL == x]) - s) > 0
    return(s)
  }) %>% unlist
  
  fwrite(dt, "dominio-unique-athena.csv", sep = ";")
}

#' Levantamento da disponibilidade dos arquivos, por ano e estado
#' @param database_name character.
#' @param table_name character.
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2,
#'                               "inicio.uf" = 10, "qnt.uf" = 2)
#' Saídas escritas em "arquivos-faltantes-athena.csv"
disponibilidade_arquivos <- function(database_name, table_name, referencia_subs) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  sub <- names(referencia_subs) %>% as.character %>% .[. != "VARIAVEL"] %>% 
    str_remove_all(., "^INICIO.|^QNT.") %>% unique %>% tolower
  
  data <- get_athena_data(database_name, 
                            paste0(
"SELECT \"", referencia_subs$VARIAVEL, "\" AS \"variavel\"",
" FROM ", database_name, ".", table_name,
" GROUP BY \"", referencia_subs$VARIAVEL, "\""))
  
  for(i in sub) {
    s <- toupper(paste0(c("INICIO.", "QNT."), i))
    data[[i]] <- substr(data$variavel, referencia_subs[[s[1]]], 
                        referencia_subs[[s[1]]] + referencia_subs[[s[2]]] - 1)
  }
  data$total <- data$variavel <- NULL
  data <- unique(data)
  
  data_ref <- lapply(data, unique) %>% expand.grid %>% setDT
  data_ref[, disponivel := FALSE][data, disponivel := TRUE, on = sub]
  data_ref <- data_ref[!data_ref$disponivel, ]
  
  if(nrow(data_ref) != 0)
    fwrite(data_ref, "arquivos-faltantes-athena.csv", sep = ";")
}
