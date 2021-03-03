library(data.table)
library(dplyr)

source("R/dicionario.R")
source("R/leitura-data.R")


#' Temporalidade dos registros
#' @param v_temp list. Lista contendo as variáveis a serem analisadas.
#' ex: v_temp <- list(c("DT_INTERNACAO", "DT_SAIDA"))
#' DT_SAIDA > DT_INTERNACAO
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "temporalidade.csv"?
#' @return data.table. Colunas: Min., 1st Qu., Median, Mean, 3rd Qu., Max., NA's, V1, V2, teste
temporalidade_agr <- function(v_temp, parametros_database, out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  v_temp <- lapply(v_temp, toupper)
  
  dt <- lapply(v_temp, function(z) {
    print(z)
    data <- coluna_temp_agregada(z[1], z[2], parametros_database)
    data <- data[data$day_diff >= 0, ]
    s <- summary_all(rep(data$day_diff, data$total))
    return(s)
  }) %>% do.call(rbind.data.frame, .) %>% 
    `colnames<-`(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's"))
  dt[["V1"]] <- sapply(v_temp, function(x) x[1])
  dt[["V2"]] <- sapply(v_temp, function(x) x[2])
  dt[["teste"]] <- paste0("T", seq(v_temp))
  
  if(out) fwrite(dt, "temporalidade.csv", sep = ";", dec = ",")
  return(dt)
}


#' Temporalidade dos registros, a partir da comparação de substrings
#' @param v_temp list. Lista contendo as variáveis a serem analisadas.
#' ex: v_temp <- list(c("DT_INTERNACAO", "DT_SAIDA"))
#' DT_SAIDA > DT_INTERNACAO
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2,
#'                               "inicio.uf" = 10, "qnt.uf" = 2)
#' @param sub character. "ANO || UF"
#' @param out boolean. Escrever as saidas em "temporalidade-sub.csv"?
#' @return data.table. Colunas Tx, sub
temporalidade_sub_agr <- function(v_temp, parametros_database, referencia_subs, sub, out=T) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  if(is.null(referencia_subs[[s[1]]]) || is.null(referencia_subs[[s[2]]]))
    return(NULL)
  
  print(sub)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  v_temp <- lapply(v_temp, toupper)
  if(!all(sapply(v_temp, function(x) all(x %in% toupper(dict$VARIAVEL)))))
    return(NULL)
  
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  subs <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)$variavel
  subs <- substr(subs, referencia_subs[[s[1]]], referencia_subs[[s[1]]]+referencia_subs[[s[2]]]-1) %>% 
    unique %>% sort
  
  dt <- lapply(subs, function(w) {
    sapply(v_temp, function(z) {
      print(paste(z, w))
      data <- coluna_ref_temp_agregada(z[1], z[2], parametros_database, 
                                       referencia_subs$VARIAVEL, referencia_subs[[s[1]]], w)
      data <- data[data$day_diff >= 0, ]
      m <- median(rep(data$day_diff, data$total), na.rm = T)
      if(is.null(m)) m <- NA
      return(m)
    })
  }) %>% do.call(rbind.data.frame, .) %>% 
    `colnames<-`(paste0("T", seq(v_temp)))
  
  dt[[sub]] <- subs
  
  if(out) fwrite(dt, paste0("temporalidade-", sub, ".csv"), sep = ";", dec = ",")
  return(dt)
}


#' Temporalidade dos registros, com aplicação de filtros
#' @param v_temp list. Lista contendo as variáveis a serem analisadas.
#' ex: v_temp <- list(c("DT_INTERNACAO", "DT_SAIDA"))
#' DT_SAIDA > DT_INTERNACAO
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param filtro. list. e.g. filtro <- list("variavel"="UF", "val"="ES")
#' @param out boolean. Escrever as saidas?
temporalidade_filtro_agr <- function(v_temp, parametros_database, filtro, out) {
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  v_temp <- lapply(v_temp, toupper)
  
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  variaveis <- sort(variaveis) %>% toupper
  
  dt <- lapply(v_temp, function(z) {
    print(z)
    data <- coluna_ref_temp_agregada(z[1], z[2], parametros_database, 
                                     filtro$VARIAVEL, 1, filtro$VAL)
    data <- data[data$day_diff >= 0, ]
    s <- summary_all(rep(data$day_diff, data$total))
    return(s)
  }) %>% do.call(rbind.data.frame, .) %>% 
    `colnames<-`(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's"))
  dt[["V1"]] <- sapply(v_temp, function(x) x[1])
  dt[["V2"]] <- sapply(v_temp, function(x) x[2])
  dt[["teste"]] <- paste0("T", seq(v_temp))
  
  if(out) fwrite(dt, paste0("temporalidade-", filtro$VARIAVEL, ".csv"), sep = ";", dec = ",")
  return(dt)
}
