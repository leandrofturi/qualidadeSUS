library(data.table)
library(stringr)
library(dplyr)

source("R/qualidade.R")
source("R/dicionario.R")
source("R/leitura-data.R")


#' Diagnostico agrupado pelas quantidades de registros.
#' Funcionam da mesma forma que uma variável agrupada pela função plyr::count,
#'  com nomes de colunas: variavel e total.
#' @param variaveis character. Variáveis a serem analisadas
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "diagnostico.csv"?
#' @return data.table. Colunas: variavel, total, nao.nulo, conformes, acurados
diagnostico_agr <- function(variaveis, parametros_database, out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- sort(variaveis) %>% toupper
  dt <- lapply(variaveis, function(x) {
    print(x)
    data <- coluna_agregada(x, parametros_database) 
    return(qualidade_agr(data, D, x))
  }) %>% .[!sapply(., is.null)]
  
  if(length(dt) == 0) return(NULL)
  
  dt <- do.call(rbind, dt) %>% data.table %>%
    `colnames<-`(c("total", "nao.nulo", "conformes", "acurados")) %>% 
    cbind("variavel" = variaveis, .)
  
  # Ajuste de total
  dt$total <- max(dt$total)
  
  if(out) fwrite(dt, "diagnostico.csv", sep = ";")
  
  return(dt)
}


#' Diagnostico agrupado pelas quantidades de registros, 
#'  a partir da comparação de substrings
#' @param variaveis character[]. Variáveis a serem analisadas
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2,
#'                               "inicio.uf" = 10, "qnt.uf" = 2)
#' @param sub character. "ANO || UF"
#' @param out boolean. Escrever as saidas em "diagnostico-sub.csv"?
#' @return data.table. Colunas: sub, total, nao.nulo, nao.nulo_conformes , conformes, conformes_acurados, acurados
diagnostico_sub_agr <- function(variaveis, parametros_database,
                                referencia_subs, sub, out=T) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  if(is.null(referencia_subs[[s[1]]]) || is.null(referencia_subs[[s[2]]]))
    return(NULL)
  
  print(sub)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  subs <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)$variavel
  subs <- substr(subs, referencia_subs[[s[1]]], referencia_subs[[s[1]]]+referencia_subs[[s[2]]]-1) %>% 
    unique %>% sort
  
  variaveis <- sort(variaveis) %>% toupper
  dt <- lapply(subs, function(y) {
    dt <- lapply(variaveis, function(x) {
      print(paste(x, y))
      data <- coluna_ref_agregada(x, parametros_database, 
                                  referencia_subs$VARIAVEL, referencia_subs[[s[1]]], y)
      return(qualidade_agr(data, D, x))
    }) %>% .[!sapply(., is.null)]
    
    if(is.null(dt)) return(NULL)
    
    dt <- do.call(rbind, dt) %>% data.table %>%
      `colnames<-`(c("total", "nao.nulo", "conformes", "acurados")) %>% 
      cbind("sub" = y, .)
  }) %>% .[!sapply(., is.null)]
  
  if(length(dt) == 0) return(NULL)
  
  dt <- rbindlist(dt)
  dt[["nao.nulo_conformes"]] <- pmax(dt$nao.nulo, dt$conformes)
  dt[["conformes_acurados"]] <- pmax(dt$conformes, dt$acurados)
  dt <- dt[, lapply(.SD, function(x) sum(x, na.rm = T)), by = sub]
  setcolorder(dt, c("sub","total","nao.nulo",
                    "nao.nulo_conformes","conformes",
                    "conformes_acurados","acurados"))
  colnames(dt)[colnames(dt) == "sub"] <- sub
  
  if(out) fwrite(dt, paste0("diagnostico-", sub, ".csv"), sep = ";")
  
  return(dt)
}


#' Diagnostico agrupado pelas quantidades de registros, com aplicação de filtros
#' @param variaveis character[]. Variáveis a serem analisadas
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param filtro. list. e.g. filtro <- list("variavel"="UF", "val"="ES")
#' @param out boolean. Escrever as saidas?
diagnostico_filtro_agr <- function(variaveis, parametros_database, filtro, out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  variaveis <- sort(variaveis) %>% toupper
  
  dt <- lapply(variaveis, function(x) {
    print(x)
    data <- coluna_ref_agregada(x, parametros_database, filtro$VARIAVEL, 1, filtro$VAL)
    return(qualidade_agr(data, D, x))
  }) %>% .[!sapply(., is.null)]
  
  if(length(dt) == 0) return(NULL)
  
  dt <- do.call(rbind, dt) %>% data.table %>% 
    `colnames<-`(c("total", "nao.nulo", "conformes", "acurados")) %>% 
    cbind("variavel" = variaveis, .)
  
  # Ajuste de total
  dt$total <- max(dt$total)
  
  if(out) fwrite(dt, paste0("diagnostico-", filtro$VARIAVEL, ".csv"), sep = ";")
  
  return(dt)
}


#' Qualidade de uma coluna agrupado pelas quantidades de registros.
#' @param data data.table. Da mesma forma que a aplicação da função plyr::count,
#'  com nomes de colunas: variavel e total.
#' @param D list. Saída da função dicionario_formatado
#' @param x character. Nome da variável analisada (para obter os dados de D)
qualidade_agr <- function(data, D, x) {
  if(is.null(data) || nrow(data) == 0) return(NULL)
  
  comp <- c(sum(data$total), max(0, sum(data[!is.na(data$variavel), ]$total), na.rm = T))
  data <- na.omit(data)
  
  data[, variavel := conformidade(variavel, D$tamanhos[[x]], D$variantes[[x]])]
  conf <- max(0, sum(data[!is.na(data$variavel), ]$total), na.rm = T)
  data <- na.omit(data)
  
  data[, variavel := qualidade_acuracia(variavel, D, x)]
  ac <- max(0, sum(data[!is.na(data$variavel), ]$total), na.rm = T)

  return(c(comp,conf,ac))
}


#' Testes de acurácia agrupado pelas quantidades de registros.
#' @param variavel character. Dados a serem analisados
#' @param D list. Saída da função dicionario_formatado
#' @param x character. Nome da variável analisada (para obter os dados de D)
qualidade_acuracia <- function(variavel, D, x) {
  if(!(is.na(D$referencias[[x]]) && is.na(D$formatos[[x]]) && length(D$formatos[[x]]) > 0))
    variavel <- acuracia_data(variavel, D$formatos[[x]], D$referencias[[x]])
  else if(!is.na(D$digitos[[x]]) && length(D$digitos[[x]]) > 0)
    variavel <- acuracia_mun(variavel, D$digitos[[x]])
  else if(!is.na(D$tipos[[x]]) && length(D$tipos[[x]]) > 0)
    variavel <- acuracia_uf(variavel, D$tipos[[x]])
  else if(x %in% D$numericas)
    variavel <- acuracia_numerico(variavel, D$positivos[[x]], D$inteiros[[x]])
  else {
    if(length(D$rm.zeros[[x]]) > 0 && length(D$rm.whitespace[[x]]) > 0)
      variavel <- acuracia_zeros_whitespace(variavel, D$rm.zeros[[x]], D$rm.whitespace[[x]])
    if(!is.na(D$ignorados[[x]]) && length(D$ignorados[[x]]) > 0)
      variavel[variavel %in% D$ignorados[[x]]] <- NA
  }
  return(variavel)
}


#' Variáveis em dict
#' @param parametros_database list. Parâmetros para leitura dos dados
variaveis_dict <- function(parametros_database) {
  dict <- leitura_dict(parametros_database)
  variaveis <- dict$VARIAVEL %>% toupper %>% 
    .[str_detect(., "[:graph:]")] %>% .[!is.na(.)]
  return(variaveis)
}
