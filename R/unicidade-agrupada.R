library(data.table)
library(dplyr)

source("R/dicionario.R")
source("R/leitura-data.R")
source("R/qualidade.R")
source("R/analise-agrupada.R")



#' Unicidade dos registros
#' @param variavel_id character. Variável contendo o identificador do paciente.
#' ex: id_paciente
#' @param variaveis_comp character. Variáveis para comparação.
#' ex: data_nascimento e sexo
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "unicidade.csv"?
#' @return data.table. Colunas: falhas, total, falhas_geral, total_geral, variavel, teste
unicidade_agr <- function(variavel_id, variaveis_comp, parametros_database, out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variavel_id <- toupper(variavel_id)
  variaveis_comp <- toupper(variaveis_comp)

  data <- coluna_unicidade_agregada(variavel_id, variaveis_comp, parametros_database)
  data <- qualidade_unicidade(data, D, variavel_id, variaveis_comp)
  data[, ("unico") := do.call(pmax, .SD), .SDcols = variaveis_comp]
  
  dt <- data.table("falhas" = sapply(data[, mget(variaveis_comp)], function(x) sum(x > 1)), 
                   "total" = sapply(data[, mget(variaveis_comp)], function(x) sum(x > 0)),
                   "falhas_geral" = sum(data$unico  > 1),
                   "total_geral" = nrow(data))
  dt[["variavel"]] <- variaveis_comp
  dt[["teste"]] <- paste0("T", seq(variaveis_comp))
  
  if(out) fwrite(dt, "unicidade.csv", sep = ";")
}


#' Frequencia dos identificadores de unicidade
#' @param variavel_id character. Variável contendo o identificador do paciente.
#' ex: id_paciente
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param out boolean. Escrever as saidas em "unicidade-frequencia.csv"?
#' @return data.table. Colunas: id, categoria
freq_identificadores <- function(variavel_id, parametros_database, out=T) {
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variavel_id <- toupper(variavel_id)
  data <- coluna_agregada(variavel_id, parametros_database)
    
  data[, variavel := conformidade(variavel, D$tamanhos[[variavel_id]], D$variantes[[variavel_id]])]
  data[, variavel := qualidade_acuracia(variavel, D, variavel_id)]
  data <- na.omit(data)
  
  dt <- data.frame("id" = c(
    sum(data$total == 1)*100/sum(data$total > 0),
    sum(data$total == 2)*100/sum(data$total > 0),
    sum(data$total == 3)*100/sum(data$total > 0),
    sum(data$total == 4)*100/sum(data$total > 0),
    sum(data$total >= 5 & data$total < 10)*100/sum(data$total > 0),
    sum(data$total >= 10 & data$total < 50)*100/sum(data$total > 0),
    sum(data$total >= 50 & data$total < 100)*100/sum(data$total > 0),
    sum(data$total >= 100)*100/sum(data$total > 0)),
    "categoria" = c(
      "ID = 1", "ID = 2", "ID = 3", "ID = 4", "5 <= ID < 10", "10 <= ID < 50", 
      "50 <= ID < 100", "ID >= 100"))
  dt$id <- formatC(dt$id, digits=2, format="f")
  
  if(out) fwrite(dt, "unicidade-frequencia.csv", sep = ";", dec = ",")
}


#' Unicidade dos registros, a partir da comparação de substrings
#' @param variavel_id character. Variável contendo o identificador do paciente.
#' ex: id_paciente
#' @param variaveis_comp character. Variáveis para comparação.
#' ex: data_nascimento e sexo
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2,
#'                               "inicio.uf" = 10, "qnt.uf" = 2)
#' @param sub character. "ANO || UF"
#' @param out boolean. Escrever as saidas em "unicidade-sub.csv"?
#' @return data.table. Colunas: sub, total, falhas
unicidade_sub_agr <- function(variavel_id, variaveis_comp, 
                               referencia_subs, sub, parametros_database, out=T) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  if(is.null(referencia_subs[[s[1]]]) || is.null(referencia_subs[[s[2]]]))
    return(NULL)
  
  print(sub)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variavel_id <- toupper(variavel_id)
  variaveis_comp <- toupper(variaveis_comp)
  if(!all(variaveis_comp %in% toupper(dict$VARIAVEL)) || !variavel_id %in% toupper(dict$VARIAVEL))
    return(NULL)
  
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  subs <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)$variavel
  subs <- substr(subs, referencia_subs[[s[1]]], referencia_subs[[s[1]]]+referencia_subs[[s[2]]]-1) %>% 
    unique %>% sort
  
  dt <- lapply(subs, function(w) {
    print(w)
    data <- coluna_ref_unicidade_agregada(variavel_id, variaveis_comp, parametros_database,
                                          referencia_subs$VARIAVEL, referencia_subs[[s[1]]], w)
    data <- qualidade_unicidade(data, D, variavel_id, variaveis_comp)
    
    if(nrow(data) == 0 || is.null(data))
      return(data.table(0, 0))
    
    data[, ("unico") := do.call(pmax, .SD), .SDcols = variaveis_comp]
    t <- sum(data$unico > 1, na.rm = T)
    tt <- nrow(data)
    return(data.table(tt, t))
  }) %>% rbindlist %>% `colnames<-`(c("total","falhas"))
  
  dt[[sub]] <- subs
  
  if(out) fwrite(dt, paste0("unicidade-", sub, ".csv"), sep = ";")
}


#' Unicidade dos registros, com aplicação de filtros
#' @param variavel_id character. Variável contendo o identificador do paciente.
#' ex: id_paciente
#' @param variaveis_comp character. Variáveis para comparação.
#' ex: data_nascimento e sexo
#' @param parametros_database list. Parâmetros para leitura dos dados
#' @param filtro. list. e.g. filtro <- list("variavel"="UF", "val"="ES")
#' @param out boolean. Escrever as saidas?
unicidade_filtro_agr <- function(variavel_id, variaveis_comp, 
                                 filtro, parametros_database, out=T) {
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variavel_id <- toupper(variavel_id)
  variaveis_comp <- toupper(variaveis_comp)
  
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  
  data <- coluna_ref_unicidade_agregada(variavel_id, variaveis_comp, parametros_database,
                                        filtro$VARIAVEL, 1, filtro$VAL)
  data <- qualidade_unicidade(data, D, variavel_id, variaveis_comp)
  data[, ("unico") := do.call(pmax, .SD), .SDcols = variaveis_comp]

  dt <- data.table("falhas" = sapply(data[, mget(variaveis_comp)], function(x) sum(x > 1)), 
                   "total" = sapply(data[, mget(variaveis_comp)], function(x) sum(x > 0)),
                   "falhas_geral" = sum(data$unico  > 1),
                   "total_geral" = nrow(data))
  dt[["variavel"]] <- variaveis_comp
  dt[["teste"]] <- paste0("T", seq(variaveis_comp))
  
  if(out) fwrite(dt, paste0("unicidade-", filtro$VARIAVEL, ".csv"), sep = ";")
  return(dt)
}


#' Testes de qualidade (conformidade e acurácia) agrupado pelas quantidades de registros, 
#'   em dados lidos pelo teste de unicidade
#' @param data character. Dados a serem analisados
#' @param D list. Saída da função dicionario_formatado
#' @param variavel_id character. Nome da variável com identificador (para obter os dados de D)
#' @param variaveis_comp character. Nome da variável com dados (para obter os dados de D)
qualidade_unicidade <- function(data, D, variavel_id, variaveis_comp) {
  data <- na.omit(data)
  for(x in variaveis_comp){
    if(D$inteiros[[x]]==TRUE){data[[x]]=as.numeric(data[[x]])}
  }
  # Qualidade das colunas
  for(x in c(variavel_id,variaveis_comp)) {
    data[, (x) := conformidade(data[[x]], D$tamanhos[[x]], D$variantes[[x]])]
    data[, (x) := qualidade_acuracia(data[[x]], D, x)]
  }
  data <- na.omit(data)
  
  tt <- data[, lapply(.SD, function(x) sum(as.numeric(x))), 
             by=variavel_id, .SDcols="total"]
  data <- data[, lapply(.SD, function(x) length(unique(x))), 
               by=variavel_id, .SDcols=variaveis_comp]
  data <- full_join(data, tt, by = variavel_id)
  
  return(data)
}
