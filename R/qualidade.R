library(stringr)
library(dplyr)

source("R/cnv.R")


#' Conformidade dos registros por tamanho e valores válidos de domínio
#' @param data vector. Dados a serem analisados
#' @param tamanho numeric[2]. Tamanhos mínimos e máximos a serem comparados
#' @param variantes character. Valores válidos de domínio a serem comparados
#' @return vector. Dados com NA em posições apresentando falhas
conformidade <- function(data, tamanho, variantes) {
  if(length(tamanho) < 2 || !is.numeric(tamanho))
    return(NA)
  
  data <- as.character(data)
  if(is.na(variantes) || is.null(variantes)) {
    nc <- nchar(data)
    data[!(nc >= tamanho[1] & nc <= tamanho[2])] <- NA
    return(data)
  }
  else if(grepl(".CNV$", toupper(variantes[1])))
    return(suppressWarnings(conformidade_cnv(data, variantes[1])))
  else
    return(acuracia(data, variantes))
}


#' Conformidade dos registros a partir de um arquivo .cnv
#' @param data vector. Dados a serem analisados
#' @param cnv_path character. Caminho para o arquivo .cnv
#' @return vector. Dados com NA em posições apresentando falhas
conformidade_cnv <- function(data, cnv_path) {
  if(!file.exists(cnv_path))
    return(NA)
  
  variantes <- read_cnv(cnv_path)$val %>% 
    lapply(., function(x) str_split(x, ",") %>% unlist %>% as.character %>% 
      trimws %>% .[. != ""]) %>% unlist
  
  #' Sequências do tipo A00-A99
  variantes <- c(variantes, lapply(variantes, create_sequence) %>% unlist)
  
  return(acuracia(data, variantes))
}


#' Acurácia dos registros a partir de valores válidos de domínio
#' @param data vector. Dados a serem analisados
#' @param variantes character. Valores válidos de domínio a serem comparados
#' @return vector. Dados com NA em posições apresentando falhas
acuracia <- function(data, variantes) {
  data[!data %in% variantes] <- NA
  return(data)
}


#' Acurácia dos registros a partir de valores de datas
#' @param data vector. Dados a serem analisados
#' @param formato character. Formato dos valores de data. Vide classe Date
#' @param referencias character[2]. Referências inferiores e superiores para comparação
#' @return vector. Dados com NA em posições apresentando falhas
acuracia_data <- function(data, formato, referencias) {
  if(is.na(formato) || length(referencias) < 2) return(NA)
  
  data_Date <- toDate(data, formato)
  referencias <- toDate(referencias, formato)
  if(any(is.na(referencias))) return(NA)
  
  data[!(data_Date >= referencias[1] & data_Date <= referencias[2])] <- NA
  return(data)
}


#' Acurácia dos registros a partir de valores válidos de código de município
#' @param data vector. Dados a serem analisados
#' @param digitos numeric. Quantidade de dígitos para avaliação
#' @return vector. Dados com NA em posições apresentando falhas
acuracia_mun <- function(data, digitos) {
  municipios_IBGE <- readRDS("R/data/municipios_IBGE.Rds")
  
  if(is.na(digitos) || is.null(digitos)) return(NA)
  
  variantes <- substr(municipios_IBGE, 1, digitos)
  return(acuracia(data, variantes))
}


#' Acurácia dos registros a partir de valores válidos de UF
#' @param data vector. Dados a serem analisados
#' @param tipo character. Tipo do registro representando a UF. 
#' "S" para siglas, "N" para nome completo do estado e "C" para o código
#' @return vector. Dados com NA em posições apresentando falhas
acuracia_uf <- function(data, tipo) {
  uf <- rbind(c("AC",12,"Acre"), c("AL",27,"Alagoas"), c("AP",16,"Amapá"), 
              c("AM",13,"Amazonas"), c("BA",29,"Bahia"), c("CE",23,"Ceará"), 
              c("DF",53,"Distrito Federal"), c("ES",32,"Espírito Santo"), 
              c("GO",52,"Goiás"), c("MA",21,"Maranhão"), 
              c("MT",51,"Mato Grosso"), c("MS",50,"Mato Grosso do Sul"), 
              c("MG",31,"Minas Gerais"), c("PA",15,"Pará"), 
              c("PB",25,"Paraíba"), c("PR",41,"Paraná"), 
              c("PE",26,"Pernambuco"), c("PI",22,"Piauí"), 
              c("RJ",33,"Rio de Janeiro"),
              c("RN",24,"Rio Grande do Norte"), c("RS",43,"Rio Grande do Sul"), 
              c("RO",11,"Rondônia"), c("RR",14,"Roraima"), 
              c("SC",42,"Santa Catarina"), c("SP",35,"São Paulo"), 
              c("SE",28,"Sergipe"), c("TO",17,"Tocantins")) %>% data.table %>% 
    `colnames<-`(c("sigla","codigo","nome"))
  
  if(tipo == "N")
    return(acuracia(data, uf$nome))
  else if(tipo == "S")
    return(acuracia(data, uf$sigla))
  else if(tipo == "C")
    return(acuracia(data, uf$codigo))
  else
    return(NA)
}


#' Acurácia dos registros a partir de registros numéricos
#' @param data vector. Dados a serem analisados
#' @param positivo boolean. Numérico é positivo?
#' @param inteiro boolean. Numérico é inteiro?
#' @return vector. Dados com NA em posições apresentando falhas
acuracia_numerico <- function(data, positivo, inteiro) {
  data_num <- suppressWarnings(as.numeric(data))
  
  if(positivo && inteiro)
    data[!(data_num >= 0 & round(data_num) == data_num)] <- NA
  else if(positivo)
    data[!(data_num >= 0)] <- NA
  else if(inteiro)
    data[!(round(data_num) == data_num)] <- NA

  return(data)
}


#' Acurácia dos registros para dentificar sequencias de zeros ou espaços em branco,
#' visando identificar revistros ignorados.
#' @param data vector. Dados a serem analisados
#' @param rm.zeros boolean. Remover sequências de zeros?
#' @param rm.whitespace boolean. Remover sequências de espaços em branco?
#' @return vector. Dados com NA em posições apresentando falhas
acuracia_zeros_whitespace <- function(data, rm.zeros, rm.whitespace) {
  data <- as.character(data)
  if(rm.whitespace && rm.zeros)
    i <- sapply(data, function(x) 
      paste(str_extract_all(x, "0+|[:space:]+", simplify = T), collapse = ""))
  else if(rm.whitespace)
    i <- sapply(data, function(x) 
      paste(str_extract_all(x, "0+", simplify = T), collapse = ""))
  else if(rm.zeros)
    i <- sapply(data, function(x) 
      paste(str_extract_all(x, "[:space:]+", simplify = T), collapse = ""))
  else
    return(data)
  
  data[nchar(data) == nchar(i)] <- NA
  return(data)
}


#' Temporalidade entre duas datas, retornando summary_all entre as diferenças
#' data2 - data1
#' @param data1 vector. Dados a serem analisados
#' @param formato1 character. Formato dos valores de data. Vide classe Date
#' @param referencias1 character[2]. Referências inferiores e superiores para comparação
#' @param data2 vector. Dados a serem analisados
#' @param formato2 character. Formato dos valores de data. Vide classe Date
#' @param referencias2 character[2]. Referências inferiores e superiores para comparação
#' @return numeric[]
temporalidade <- function(data1, formato1, referencia1, 
                          data2, formato2, referencia2) {
  if(length(data1) != length(data2)) return(NA)
  
  data1 <- acuracia_data(data1, formato1, referencias1)
  data2 <- acuracia_data(data2, formato2, referencias2)
  
  tmp <- as.numeric(data2 - data1) %>% .[. >= 0]
  if(!str_detect(formato1, "%d") || !str_detect(formato2, "%d"))
    tmp <- tmp[tmp > 30]
  
  return(summary_all(tmp))
}


#' Conversão em data 
#' @param data vector. Dados a serem convertidos
#' @param formato character. Formato dos valores de data. Vide classe Date
#' @return Date
toDate <- function(data, formato) {
  if(is.na(formato)) return(NA)
  
  if(!str_detect(formato, "%d"))
    data <- strptime(paste0(data, "01"), paste0(formato, "%d"))
  else
    data <- strptime(data, formato)
  
  return(data)
}


#' Função summary com todos os campos
#' @param data vector. Dados a serem analisados
#' @return numeric
summary_all <- function(data) {
  data <- as.numeric(data)
  if(!any(is.na(data)))
    r <- c(summary(data, digits = 2), 0)
  else
    r <- summary(data, digits = 2)
  
  r <- as.numeric(r) %>%
    `names<-`(c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","NA's"))
  return(r)
}


#' Cria sequências do tipo A00-A99
#' @param data vector. Dados a serem analisados
#' @return character
create_sequence <- function(data) {
  if(!str_detect(data, "-") || !str_detect(data, "[:digit:]")) 
    return(NULL)
  
  val <- str_split(data, "-") %>% unlist %>% as.character %>% 
    trimws %>% .[. != ""]
  
  val_num <- sapply(val, function(x) 
    str_extract(x, "^[:digit:]+|[:digit:]+$"))
  if(length(val_num) < 2 || as.numeric(val_num[1]) > as.numeric(val_num[2])) 
    return(NULL)
  
  n <- max(nchar(val_num))
  val_num <- sprintf(paste0("%0", n, "d"), 
                     as.numeric(val_num[1]):as.numeric(val_num[2]))
  
  val_ptn <- sapply(val, function(x) 
    str_replace_all(x, paste0("^[:digit:]{",n,"}|[:digit:]{",n,"}$"), "%"))
  
  val <- lapply(val_ptn, function(x) 
    lapply(val_num, function(y) str_replace(x, "%", y))) %>% unlist %>% unique
  
  return(val)
}
