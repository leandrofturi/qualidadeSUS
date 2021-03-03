library(stringr)
library(dplyr)
library(data.table)

source("R/leitura-data.R")


#' Formatação do dicionário
#' @param dict data.frame. Tabela com os parâmetros para análise:
#' VARIAVEL: variável a ser analisada
#' MIN/MAX: tamanho mínimo/máximo da quantidade de caracteres
#' VARIANTES: domínio aceito, separados por ", "
#' CLASSE: classe da variável (usada para obter os tipos numeric)
#' FORMATO: formato da data. ex: %d%m%Y
#' REFERENCIA.INF/REFERENCIA.SUP: referências inferiores e superiores,
#'                                onde caso deseja-se comparar com os limites
#'                                da base, coloca-se "inf"ou "sup"
#' DIGITOS: quantidade de dígitos de códigos de estados
#' TIPO: tipo do dado representando UF. "S" para siglas, "N" para nome completo 
#'       do estado e "C" para o código
#' RM.ZEROS: zeros são considerados como inválidos? TRUE/FALSE para todas as 
#'           variáveis
#' RM.WHITESPACE: whitespaces são considerados como inválidos? TRUE/FALSE para 
#'                todas as variáveis
#' IGNORADOS: palavras representando valores inválidos
#' POSITIVO: numerico é positivo?
#' INTEIRO: numerico é inteiro? usando também em unicidade, caso a variável seja numérica
dicionario_formatado <- function(dict) {
  if(is.null(dict))
    return()
  
  colnames(dict) <- stringi::stri_trans_general(str = colnames(dict), 
                                                id = "Latin-ASCII") %>% toupper
  dict[dict == "NA" | dict == ""] <- NA
  if(!"VARIAVEL" %in% colnames(dict))
    return()
  dict$VARIAVEL <- toupper(as.character(dict$VARIAVEL))
  
  tamanhos <- variantes <- numericas <- positivos <- inteiros <- referencias <- 
    formatos <- digitos <- tipos <- rm.zeros <- rm.whitespace <- 
    ignorados <- NULL
  
  if(all(c("MIN","MAX") %in% colnames(dict))) {
    tamanhos <- c(as.numeric(dict$MIN), as.numeric(dict$MAX))
    tamanhos <- setNames(split(tamanhos, seq(nrow(dict))), dict$VARIAVEL)
    if("VARIANTES" %in% colnames(dict)) {
      variantes <- as.character(dict$VARIANTES)
      variantes <- setNames(split(variantes, seq(nrow(dict))), dict$VARIAVEL)
      variantes <- lapply(variantes, function(x) {
        if(x == "" | is.na(x)) NA
        else trimws(unlist(str_split(string = x, pattern = ", ")))})
    }
  }
  
  if("CLASSE" %in% colnames(dict)) {
    dict$CLASSE <- as.character(dict$CLASSE)
    dict$CLASSE <- stringi::stri_trans_general(str = dict$CLASSE, 
                                               id = "Latin-ASCII") %>% toupper
    if("VARIANTES" %in% colnames(dict))
      dict$CLASSE[dict$VARIANTES != "" & !is.na(dict$VARIANTES)] <- "CHAR"
    numericas <- dict$VARIAVEL[dict$CLASSE == "NUMERIC" | 
                               dict$CLASSE == "NUMERICO" | 
                               dict$CLASSE == "NUMERICA"] %>% as.character
  }
  
  if(length(numericas) > 0) {
    positivos <- setNames(split(rep(FALSE, nrow(dict)), seq(nrow(dict))), 
                               dict$VARIAVEL)
    inteiros <- setNames(split(rep(FALSE, nrow(dict)), seq(nrow(dict))), 
                           dict$VARIAVEL)
  }
  
  if("POSITIVO" %in% colnames(dict)) {
    positivos <- dict$POSITIVO %>% as.character %>% toupper
    positivos <- positivos == "TRUE" | positivos == "T"
    positivos <- setNames(split(positivos, seq(nrow(dict))), dict$VARIAVEL)
    positivos[is.na(positivos)] <- FALSE
  }
  
  if("INTEIRO" %in% colnames(dict)) {
    inteiros <- dict$INTEIRO %>% as.character %>% toupper
    inteiros <- inteiros == "TRUE" | inteiros == "T"
    inteiros <- setNames(split(inteiros, seq(nrow(dict))), dict$VARIAVEL)
    inteiros[is.na(inteiros)] <- FALSE
    
    if("VARIANTES" %in% colnames(dict))
      inteiros[sapply(variantes, function(x) all(!is.na(as.numeric(x))))] <- TRUE
  }
  
  if(all(c("FORMATO","REFERENCIA.INF","REFERENCIA.SUP") %in% colnames(dict))) {
    referencias <- c(as.character(dict$REFERENCIA.INF), 
                     as.character(dict$REFERENCIA.SUP))
    referencias[referencias == ""] <- NA
    referencias <- setNames(split(referencias, seq(nrow(dict))),
                            dict$VARIAVEL)
    formatos <- as.character(dict$FORMATO)
    formatos[formatos == ""] <- NA
    formatos <- setNames(split(formatos, seq(nrow(dict))), 
                         dict$VARIAVEL)
  }
  
  if("DIGITOS" %in% colnames(dict)) {
    digitos <- dict$DIGITOS %>% as.numeric
    digitos <- setNames(split(digitos, seq(nrow(dict))), dict$VARIAVEL)
  }
  
  if("TIPO" %in% colnames(dict)) {
    tipos <- dict$TIPO %>% as.character %>% toupper
    tipos[tipos == ""] <- NA
    tipos <- setNames(split(tipos, seq(nrow(dict))), dict$VARIAVEL)
  }
  
  if("RM.ZEROS" %in% colnames(dict)) {
    rm.zeros <- dict$RM.ZEROS %>% as.character %>% toupper
    rm.zeros <- rm.zeros == "TRUE" | rm.zeros == "T"
    
    if("VARIANTES" %in% colnames(dict))
      rm.zeros[dict$VARIANTES != "" & !is.na(dict$VARIANTES)] <- FALSE
    rm.zeros <- setNames(split(rm.zeros, seq(nrow(dict))), dict$VARIAVEL)
    if(length(numericas) > 0)
      rm.zeros[numericas] <- FALSE
  }
  
  if("RM.WHITESPACE" %in% colnames(dict)) {
    rm.whitespace <- dict$RM.WHITESPACE %>% as.character %>% toupper
    rm.whitespace <- rm.whitespace == "TRUE" | rm.whitespace == "T"
    
    if("VARIANTES" %in% colnames(dict))
      rm.whitespace[dict$VARIANTES != "" & !is.na(dict$VARIANTES)] <- FALSE
    rm.whitespace <- setNames(split(rm.whitespace, seq(nrow(dict))), 
                              dict$VARIAVEL)
    if(length(numericas) > 0)
      rm.whitespace[numericas] <- FALSE
  }
  
  if("IGNORADOS" %in% colnames(dict)) {
    ignorados <- as.character(dict$IGNORADOS)
    ignorados <- setNames(split(ignorados, seq(nrow(dict))), dict$VARIAVEL)
    ignorados <- lapply(ignorados, function(x) {
      if(x == "" | is.na(x)) NA
      else unlist(str_split(string = x, pattern = ", "))})
  }
  
  return(list("variaveis" = toupper(dict$VARIAVEL),
              "tamanhos" = tamanhos, 
              "variantes" = variantes, 
              "numericas" = numericas, 
              "positivos" = positivos,
              "inteiros" = inteiros,
              "referencias" = referencias, 
              "formatos" = formatos, 
              "digitos" = digitos, 
              "tipos" = tipos, 
              "rm.zeros" = rm.zeros, 
              "rm.whitespace" = rm.whitespace,
              "ignorados" = ignorados))
}


#' Atualizar as datas limites do dicionário de dados?
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2)
#' As colunas indicadas são obrigatórias!
atualizar_datas_dict <- function(parametros_database, referencia_subs) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  
  if(is.null(referencia_subs[["VARIAVEL"]]) || 
     is.null(referencia_subs[["INICIO.ANO"]]) || 
     is.null(referencia_subs[["QNT.ANO"]]) || 
     is.null(referencia_subs[["INICIO.MES"]]) || 
     is.null(referencia_subs[["QNT.MES"]])) {
    print("FALTA DE CAMPOS DE REFERENCIA!")
    return(dict)
  }
  
  data <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)
  data[["ano"]] <- substr(data$variavel, referencia_subs$INICIO.ANO, 
                          referencia_subs$INICIO.ANO+referencia_subs$QNT.ANO-1)
  data[["mes"]] <- substr(data$variavel, referencia_subs$INICIO.MES, 
                          referencia_subs$INICIO.MES+referencia_subs$QNT.MES-1)
  data[["mes.inicio"]] <- "01"
  end.month <- seq(as.Date("2012-02-01"),length = 12, by = "months") - 1
  end.month <- lubridate::mday(end.month)
  data[["mes.fim"]] <- sapply(data$mes, function(x) 
    end.month[as.numeric(x)] %>% as.character)
  
  ref <- list(c(min(data$ano),
                min(data$mes[data$ano == min(data$ano, na.rm = T)], na.rm = T),
                "01"),
              c(max(data$ano),
                max(data$mes[data$ano == max(data$ano, na.rm = T)], na.rm = T),
                max(data$mes.fim[data$mes == max(data$mes[data$ano == max(data$ano, na.rm = T)], na.rm = T)], na.rm = T)))
  
  dict <- leitura_dict(parametros_database)
  colnames(dict) <- toupper(colnames(dict))
  variaveis <- sort(dict$VARIAVEL) %>% toupper
  rownames(dict) <- variaveis
  
  D <- dicionario_formatado(dict)
  
  v_datas <- lapply(variaveis, function(x) 
    if(!is.na(D$formatos[[x]]) && D$formatos[[x]] != "") x else NULL) %>% 
    plyr::compact(.) %>% unlist
  
  for(x in v_datas) {
    f <- rep(D$formatos[[x]], 2)
    f <- str_replace(f, "%Y|%y", sapply(ref, function(x) x[1]))
    f <- str_replace(f, "%m", sapply(ref, function(x) x[2]))
    f <- str_replace(f, "%d", sapply(ref, function(x) x[3]))
    if(year(toDate(D$referencias[[x]], D$formatos[[x]]))[1] <= 1900)
      dict[x, c("REFERENCIA.INF", "REFERENCIA.SUP")] <- f
    else
      dict[x, c("REFERENCIA.SUP")] <- f[2]
  }
  rownames(dict) <- NULL
  
  return(dict)
}
