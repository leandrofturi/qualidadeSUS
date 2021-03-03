library(dplyr)
library(stringr)
library(bdacodeR)
library(reticulate)
#use_condaenv("notebook", required=TRUE)

source("R/analise-agrupada.R")


#' Leitura e atualização do relatório de padronização, presente em proadi-misc
#' @param referencia_subs list. Vide "analise-agrupada.R"
#' @param table_name character. Tabela de database_name
#' @return dataframe. Colunas FILENAME, QNT_LINHAS, QNT_COLUNAS, colunas, ano
relatorio_atualizado_S3 <- function(table_name, referencia_subs=NULL) {
  database_name <- "proadi_s3"
  
  path <- paste0("s3_report/", toupper(table_name), "_RELATORIO.csv.gz")
  if(!check_file_exist_s3("proadi-misc", path))
    return(NULL)
  
  relatorio <- get_s3_data("proadi-misc", path, "gzip")
    
  names(referencia_subs) <- toupper(names(referencia_subs))
  if(is.null(referencia_subs[["INICIO.ANO"]]) || is.null(referencia_subs[["QNT.ANO"]]))
    return(relatorio)
  
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  data <- coluna_agregada(referencia_subs$VARIAVEL, 
                          list("database_name"=database_name, "table_name"=table_name))
  data <- full_join(data, relatorio, by = c("variavel" = "FILENAME"))
  data[["QNT_LINHAS"]] <- NULL
  data[["ano"]] <- substr(data$variavel, referencia_subs$INICIO.ANO, 
                          referencia_subs$INICIO.ANO+referencia_subs$QNT.ANO-1)
  
  i <- which.max(data$ano[!is.na(data$QNT_COLUNAS)])
  data$QNT_COLUNAS[is.na(data$QNT_COLUNAS)] <- data$QNT_COLUNAS[i]
  
  i <- which.max(data$ano[!is.na(data$colunas)])
  data$colunas[is.na(data$colunas)] <- data$colunas[i]
  
  colnames(data)[colnames(data) != "ano"] <- colnames(relatorio)
  fwrite(data, paste0(toupper(table_name), "_RELATORIO.csv.gz"))
  
  return(data)
}


#' Obtenção do número total de registros por variável vindo do relatório de 
#' padronização, presente em proadi-misc
#' @param relatorio data.frame.Idem retorno de relatorio_atualizado_S3()
#' @param referencia_subs list. Vide "analise-agrupada.R"
#' @param table_name character. Tabela de database_name
#' @return list. Posições geral e as subs presentes em referencia_subs
totais_variavel_relatorio_S3 <- function(relatorio, referencia_subs, table_name) {
  database_name <- "proadi_s3"
  
  relatorio$colunas <- str_split(relatorio$colunas, ", ")
  relatorio$QNT_LINHAS <- as.numeric(relatorio$QNT_LINHAS)
  
  variaveis <- relatorio$colunas %>% unlist %>% toupper %>% unique %>% sort
  
  dt <- list("geral"=sapply(variaveis, function(x)
    sum(relatorio$QNT_LINHAS[sapply(relatorio$colunas, function(y) x %in% y)])) %>% 
    cbind("variavel"=variaveis,"total_raw"=.) %>% data.table)
  
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- str_split(names(referencia_subs), "^INICIO.|^QNT.", simplify=T) %>% unlist %>% 
    unique %>%.[. != "VARIAVEL" & . != ""]
  
  if(length(sub) != (length(names(referencia_subs))-1)/2)
    return(dt)
  
  for(s in sub) {
    relatorio[[s]] <- substr(relatorio$FILENAME, referencia_subs[[paste0("INICIO.",s)]], 
                             referencia_subs[[paste0("INICIO.",s)]]+referencia_subs[[paste0("QNT.",s)]]-1)
    subs <- relatorio[[s]] %>% unique %>% sort
    dt[[s]] <- lapply(subs, function(w)
      sapply(variaveis, function(x)
        sum(relatorio$QNT_LINHAS[sapply(relatorio$colunas, function(y) x %in% y) & 
                                   relatorio[[s]] == w]))) %>% 
      do.call(cbind, .) %>% `colnames<-`(subs) %>% 
      cbind("variavel"=variaveis,.) %>% data.table
  }
  
  return(dt)
}


#' Houve acréscimo dos registros?
#' @param database_name character. database_name
#' @param table_name character. Tabela de database_name
#' @param path_to_data character. Caminho em que o arquivo de quantidade de registros será/está salvo
#' @return boolean
acrescimo_de_registros <- function(database_name, table_name, path_to_data) {
  t <- get_athena_data(database_name,
                         paste0("SELECT COUNT(*) AS \"total\" FROM ", database_name, ".", table_name))
  t <- as.numeric(t$total)
  f <- paste0(path_to_data, table_name, ".info")
  if(file.exists(f)) {
    t_a <- fread(f) %>% as.numeric
    return(t_a != t)
  }
  else {
    writeLines(as.character(t), f)
    return(TRUE)
  }
}


#' Informação geral da base de dados
#' @param variaveis character[]. Variáveis analisadas.
#' @param referencia_subs list. Vide "analise-agrupada.R"
#' @param database_name character. database_name
#' @param table_name character. Tabela de database_name
#' @param path_to_data character. Caminho em que o arquivo de quantidade de registros será/está salvo
#' @return data.frame. Possíveis informações: Base de dados, Data de obtenção dos dados, 
#' Período, Região geográfica, Número máximo de variáveis, Número de registros. 
#' Saídas salvas em "informacoes-gerais.csv"
informacoes_gerais <- function(variaveis, referencia_subs, database_name, table_name, path_to_data) {
  dt <- data.table("X" = c("Base de dados","Data de obtenção dos dados"),
                   "Y" = c(toupper(table_name), format(Sys.Date(), format="%d %b %Y")))
  p <- periodo_representacao(referencia_subs, database_name, table_name)
  if(!is.null(p$periodo)) {
    if(all(is.character(p$periodo) | is.numeric(p$periodo)))
      dt <- rbind(dt, data.table("X"="Período", "Y"=p$periodo %>% 
                                   paste(., collapse = " - ")))
    else
      dt <- rbind(dt, data.table("X"="Período", "Y"=format(p$periodo, format="%b %Y") %>% 
                                   paste(., collapse = " - ")))
  }
  
  if(!is.null(p$regiao))
    dt <- rbind(dt, data.table("X"="Região geográfica", "Y"=p$regiao))
  
  t <- paste0(path_to_data, table_name, ".info") %>% fread %>% as.numeric
  dt <- rbind(dt, data.table("X" = c("Número máximo de variáveis","Número de registros"),
                             "Y" = c(length(variaveis), t)))
  
  fwrite(dt, "informacoes-gerais.csv", sep = ";")
}


#' Período de representação da base de dados
#'   Utilizado pela função informacoes_gerais
#' @param referencia_subs list. Vide "analise-agrupada.R"
#' @param database_name character. database_name
#' @param table_name character. Tabela de database_name
#' @return list.
periodo_representacao <- function(referencia_subs, database_name, table_name) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  periodo <- NULL
  regiao <- NULL
  
  subs <- coluna_agregada(referencia_subs$VARIAVEL, 
                          list("database_name"=database_name, "table_name"=table_name))
  
  p <- str_remove_all(names(referencia_subs), "^INICIO.|^QNT.") %>% 
    .[.!= "VARIAVEL" & .!= "VARIÁVEL"] %>% unique
  
  for(i in p)
    subs[[i]] <- substr(subs$variavel, referencia_subs[[paste0("INICIO.", i)]],
                        referencia_subs[[paste0("INICIO.", i)]]+referencia_subs[[paste0("QNT.", i)]]-1)
  if("MES" %in% p) {
    subs[["ANO"]] <- paste0(subs[["ANO"]], subs[["MES"]])
    periodo <- c(min(subs[["ANO"]], na.rm = T), max(subs[["ANO"]], na.rm = T)) %>% toDate(., "%Y%m")
  }
  else
    periodo <- c(min(subs[["ANO"]], na.rm = T), max(subs[["ANO"]], na.rm = T))
  
  if("UF" %in% p) {
    regiao <- subs[["UF"]] %>% unique
    uf <- rbind(c("AC",12), c("AL",27), c("AP",16), c("AM",13), c("BA",29), 
                c("CE",23), c("DF",53), c("ES",32), c("GO",52), c("MA",21), 
                c("MT",51), c("MS",50), c("MG",31), c("PA",15), c("PB",25), 
                c("PR",41), c("PE",26), c("PI",22), c("RN",24), c("RS",43), 
                c("RJ",33), c("RO",11), c("RR",14), c("SC",42), c("SP",35), 
                c("SE",28), c("TO",17)) %>% data.table %>% `colnames<-`(c("uf","id"))
    if(all(regiao %in% uf$uf) || all(regiao %in% uf$id))
      regiao <- "Todas as 27 Unidades Federativas"
    else regiao <- paste(regiao, collapse = ", ")
  }
  
  return(list("periodo"=periodo, "regiao"=regiao))
}
