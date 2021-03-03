library(dplyr)
library(data.table)

source("R/dicionario.R")
source("R/leitura-data.R")


#' Outliers agrupado pelas quantidades de registros.
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "outliers.csv"?
#' @return data.table. Colunas variavel, outliers
outliers_agr <- function(parametros_database, out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- toupper(D$numericas)
  variaveis <- variaveis[!is.na(variaveis)]
  if(length(variaveis) == 0) return(NULL)
  
  df <- suppressWarnings(lapply(variaveis, function(x) {
    print(x)
    data <- coluna_agregada(x, parametros_database) 
    data[, variavel := conformidade(variavel, D$tamanhos[[x]], D$variantes[[x]])]
    data[, variavel := qualidade_acuracia(variavel, D, x)]
    
    data <- data[, lapply(.SD, as.numeric)]
    data <- na.omit(data)
    
    if(D$positivos[[x]])
      data <- data[data$variavel >= 0, ]
    if(D$inteiros[[x]])
      data <- data[round(data$variavel) == data$variavel, ]

    out <- sort(unique(boxplot.stats(rep(data$variavel, data$total))$out))
    
    if(length(out) == 0)
      return(c(x, NA))
    else
      return(c(x, paste(c(head(out, n = 10), "...", tail(out, n = 10)), collapse=", ")))
  })) %>% do.call(rbind.data.frame, .) %>% `colnames<-`(c("variavel","outliers"))
  
  if(out) fwrite(df, "outliers.csv", sep = ";")
  return(df)
}


#' Registros inconformes/inacurados de variáveis
#' @param variaveis character. Variáveis a serem analisadas
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "inconformes-inacurados.csv"?
#' @return data.table. Colunas variavel, inconformes, acurados
inconformes_inacurados_agr <- function(variaveis, parametros_database, out=T) {
  if(length(variaveis) == 0) return(NULL)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- sort(variaveis) %>% toupper
  
  dt <- lapply(variaveis, function(x) {
    print(x)
    data <- coluna_agregada(x, parametros_database) 
    data[, "conf" := conformidade(variavel, D$tamanhos[[x]], D$variantes[[x]])]
    data_conf <- data[rowSums(is.na(data)) > 0,]
    if(nrow(data_conf) == 0)
      conf <- NA
    else
      conf <- head(data_conf$variavel, n=5) %>% paste(., collapse = ", ")
    
    data <- na.omit(data)
    data[, "ac" := qualidade_acuracia(variavel, D, x)]
    data <- data[rowSums(is.na(data)) > 0,]
    if(nrow(data) == 0)
      ac <- NA
    else
      ac <- head(data$variavel, n=5) %>% paste(., collapse = ", ")
    
    return(data.table(x, conf, ac))
  }) %>% rbindlist %>% `colnames<-`(c("variavel","inconformes","acurados"))
  
  dt <- na.omit(dt)
  if(nrow(dt) == 0)
    return(NULL)
  
  if(out) fwrite(dt, "inconformes-inacurados.csv", sep = ";")
  return(dt)
}


#' Registros representando informações ignoradas de variáveis
#' @param variaveis character. Variáveis a serem analisadas
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "ignorados.csv"?
#' @return data.table. Colunas variavel, inconformes, acurados
ignorados_agr <- function(variaveis, parametros_database, out=T) {
  if(length(variaveis) == 0) return(NULL)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- sort(variaveis) %>% toupper
  
  clean <- c("IGNORADO", "FALTANTE", "INDEFINIDO", "DESCONHECIDO", 
             "DESCONSIDERADO", "REJEITADO", "SEM INFORMACAO", 
             "NAO INFORMADO", "SEM DEFINICAO")
  
  dt <- lapply(variaveis, function(x) {
    print(x)
    data <- coluna_agregada(x, parametros_database) 
    data[, variavel := conformidade(variavel, D$tamanhos[[x]], D$variantes[[x]])]
    data[, variavel := qualidade_acuracia(variavel, D, x)]
    data <- na.omit(data)
    if(nrow(data) == 0)
      return(NULL)
    
    if(all(!grepl("\\D", data$variavel)) || length(data$variavel) <= 1)
      return(NULL)
    
    data[, variavel := stringi::stri_trans_general(str = variavel, 
                                                   id = "Latin-ASCII")]
    sm <- string_matching(data$variavel, clean, NULL)
    if(is.null(sm))
      return(NULL)
    
    return(cbind("variavel"=x, sm))
  }) %>% .[!sapply(., is.null)] %>% rbindlist
  
  if(nrow(dt) == 0)
    return(NULL)
  
  if(out) fwrite(dt, "ignorados.csv", sep = ";")
  return(dt)
}