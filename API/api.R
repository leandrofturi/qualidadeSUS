library(plumber)
source("API/graficos.R")

bases <- c("aih","horus","sia_apac","sia_bpai","sia_raas_ad","sia_raas_psi","sim")
database_name <- "vinculasus_al"
table_name <- NULL
dados <- NULL


#' @filter cors
cors <- function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers",
                  req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}


#' Definição da Base de dados
#' @param base Base de dados.
#' @get /table_name
function(base = NULL) {
  if(!base %in% bases)
    return(FALSE)
  table_name <<- base
  dados <<- leitura(database_name, table_name)
  return(TRUE)
}

#' Relatório de dados
#* @serializer contentType list(type="application/pdf")
#* @get /pdf_relatorio
function() {
  if(is.null(table_name))
    return(NULL)
  
  path <- paste0("API/gerados/", "qualidade-dados-", table_name, ".pdf")
  if(!file.exists(path))
    return(NULL)
  to_read <- file(path, "rb")
  readBin(to_read, "raw")
}

#' Gerar novo relatorio?
#' @param gerar T/F.
#' @get /novo_relatorio
function(gerar) {
  if(is.null(table_name) || is.null(database_name))
    FALSE
  
  source("API/relatorio.R")
  TRUE
}

#* Dimensões analisadas
#* @json
#* @get /dimensoes
function() {
  if(is.null(dados))
    return(NULL)
  list(dados$metricas)
}

#* Qualidade da base de dados
#* @json
#* @get /qualidade_base
function() {
  if(is.null(dados))
    return(NULL)
  list(val = dados$ponderada$val[dados$ponderada$metrica == "base"],
       cat = dados$ponderada$cat[dados$ponderada$metrica == "base"])
}

#* Qualidade da completude dos dados
#* @json
#* @get /qualidade_completude
function() {
  if(is.null(dados))
    return(NULL)
  if(!"completude" %in% dados$metricas)
    return(NULL)
  list(val = dados$ponderada$val[dados$ponderada$metrica == "completude"],
       cat = dados$ponderada$cat[dados$ponderada$metrica == "completude"])
}

#* Qualidade da conformidade dos dados
#* @json
#* @get /qualidade_conformidade
function() {
  if(is.null(dados))
    return(NULL)
  if(!"conformidade" %in% dados$metricas)
    return(NULL)
  list(val = dados$ponderada$val[dados$ponderada$metrica == "conformidade"],
       cat = dados$ponderada$cat[dados$ponderada$metrica == "conformidade"])
}

#* Qualidade da acuracia dos dados
#* @json
#* @get /qualidade_acuracia
function() {
  if(is.null(dados))
    return(NULL)
  if(!"acuracia" %in% dados$metricas)
    return(NULL)
  list(val = dados$ponderada$val[dados$ponderada$metrica == "acuracia"],
       cat = dados$ponderada$cat[dados$ponderada$metrica == "acuracia"])
}

#* Qualidade da consistencia dos dados
#* @json
#* @get /qualidade_consistencia
function() {
  if(is.null(dados))
    return(NULL)
  if(!"consistencia" %in% dados$metricas)
    return(NULL)
  list(val = dados$ponderada$val[dados$ponderada$metrica == "consistencia"],
       cat = dados$ponderada$cat[dados$ponderada$metrica == "consistencia"])
}

#* Qualidade da unicidade dos dados
#* @json
#* @get /qualidade_unicidade
function() {
  if(is.null(dados))
    return(NULL)
  if(!"unicidade" %in% dados$metricas)
    return(NULL)
  list(val = dados$ponderada$val[dados$ponderada$metrica == "unicidade"],
       cat = dados$ponderada$cat[dados$ponderada$metrica == "unicidade"])
}

#* Plot de comparacao entre valores de qualidade
#* @png
#* @get /plot_qualidade
function() {
  print(grafico_comparacao_ponderada(dados$ponderada))
}

#* Distribuicao anual de resultados de completude
#* @png
#* @get /plot_completude
function() {
  if(is.null(dados))
    return(NULL)
  if(!"completude" %in% dados$metricas)
    return(NULL)
  print(grafico_anual(dados$anual, "completude", "Completude [%]"))
}

#* Distribuição anual de resultados de conformidade
#* @png
#* @get /plot_conformidade
function() {
  if(is.null(dados))
    return(NULL)
  if(!"conformidade" %in% dados$metricas)
    return(NULL)
  print(grafico_anual(dados$anual, "conformidade", "Conformidade [%]"))
}


#* Distribuição anual de resultados de acuracia
#* @png
#* @get /plot_acuracia
function() {
  if(is.null(dados))
    return(NULL)
  if(!"acuracia" %in% dados$metricas)
    return(NULL)
  print(grafico_anual(dados$anual, "acuracia", "Acuracia [%]"))
}

#* Distribuição anual de resultados de consistencia
#* @png
#* @get /plot_consistencia
function() {
  if(is.null(dados))
    return(NULL)
  if(!"consistencia" %in% dados$metricas)
    return(NULL)
  print(grafico_anual(dados$cons, "consistencia", "Consistencia\n[partes por mil]"))
}


#* Distribuição anual de resultados de unicidade
#* @png
#* @get /plot_unicidade
function() {
  if(is.null(dados))
    return(NULL)
  if(!"unicidade" %in% dados$metricas)
    return(NULL)
  print(grafico_anual(dados$unic, "unicidade", "Unicidade [%]"))
}

#* Distribuição anual de resultados de temporalidade
#* @png
#* @get /plot_temporalidade
function() {
  if(is.null(dados))
    return(NULL)
  if(is.null(dados$temp))
    return(NULL)
  print(grafico_temporalidade_anual(dados$temp))
}


leitura <- function(database_name, table_name) {
  path_to_data <- paste0("bases/scripts/", database_name, ".", table_name, "/")
  ponderada <- data.table("metrica" = NA, "val" = NA)
  
  metricas <- anual <- cons <- unic <- temp <- NULL
  
  path <- paste0(path_to_data, "out/diagnostico-", table_name, ".csv")
  if(file.exists(path)) {
    geral <- read.csv2(path, stringsAsFactors = F)
    geral[["completude"]] <- geral$nao.nulo*100/geral$total
    geral$completude[is.na(geral$completude)] <- 0
    geral[["conformidade"]] <- geral$conformes*100/geral$nao.nulo
    geral[["acuracia"]] <- geral$acurados*100/geral$conformes
    
    ponderada <- rbind(ponderada,
  data.table("metrica" = c("completude", "conformidade", "acuracia"),
    "val" = c(sum((geral$completude*geral$total))/sum(geral$total), 
            sum((geral$conformidade*geral$total)[!is.na(geral$conformidade)]) / 
            sum(geral$total[!is.na(geral$conformidade)]),
            sum((geral$acuracia*geral$total)[!is.na(geral$acuracia)]) / 
            sum(geral$total[!is.na(geral$acuracia)]))))
    
    metricas <- c(metricas, "completude", "conformidade", "acuracia")
  }
  
  path <- paste0(path_to_data, "out/diagnostico-ano-", table_name, ".csv")
  if(file.exists(path)) {
    anual <- read.csv2(path, stringsAsFactors = F)
    anual[["completude"]] <- anual$nao.nulo*100/anual$total
    anual[["conformidade"]] <- anual$conformes*100/anual$nao.nulo_conformes
    anual[["acuracia"]] <- anual$acurados*100/anual$conformes_acurados
    if(max(nchar(as.character(anual$ano))) == 2)
      anual$ano <- paste0("20", anual$ano) %>% as.numeric
  }
  
  path <- paste0(path_to_data, "out/consistencia-ano-", table_name, ".csv")
  if(file.exists(path)) {
    cons <- read.csv2(path, stringsAsFactors = F)
    if(max(nchar(as.character(cons$ano))) == 2)
      cons$ano <- paste0("20", cons$ano) %>% as.numeric
    
    n_cons <- sum(grepl("^T", colnames(cons)), na.rm = T)
    names <- lapply(seq(n_cons), function(x) c(paste0("T", x), paste0("t", x)))
    cons[["consistencia"]] <- apply(cons[,paste0("t", seq(n_cons))], 1, sum) *1000/
      apply(cons[, paste0("T", seq(n_cons))], 1, sum)
    
    ponderada <- rbind(ponderada,
  data.table("metrica" = "consistencia",
    "val" = sum((100 - sapply(cons[,paste0("t", seq(n_cons))], sum)*100/
              sapply(cons[,paste0("T", seq(n_cons))], sum)) * 
              sapply(cons[,paste0("T", seq(n_cons))], sum)) / 
            sum(unlist(cons[,paste0("T", seq(n_cons))]))))
    
    metricas <- c(metricas, "consistencia")
  }
  
  path <- paste0(path_to_data, "out/unicidade-ano-", table_name, ".csv")
  if(file.exists(path)) {
    unic <- read.csv2(path, stringsAsFactors = F)
    unic[["unicidade"]] <- 100 - unic$falhas *100/ unic$total
    if(max(nchar(as.character(unic$ano))) == 2)
      unic$ano <- paste0("20", unic$ano) %>% as.numeric
    
    path <- paste0(path_to_data, "out/unicidade-", table_name, ".csv")
    if(file.exists(path)) {
      tmp <- read.csv2(path, stringsAsFactors = F)
      tmp[["unicidade"]] <- 100 - tmp$falhas_geral*100 / tmp$total_geral
      
      ponderada <- rbind(ponderada,
  data.table("metrica" = "unicidade", "val" = tmp$unicidade[1]))
      
      metricas <- c(metricas, "unicidade")
    }
  }
  
  path <- paste0(path_to_data, "out/temporalidade-ano-", table_name, ".csv")
  if(file.exists(path)) {
    temp <- read.csv2(path, stringsAsFactors = F)
    if(max(nchar(as.character(temp$ano))) == 2)
      temp$ano <- paste0("20", temp$ano) %>% as.numeric
    
    metricas <- c(metricas, "temporalidade")
  }
  
  ponderada <- na.omit(ponderada)
  if(nrow(ponderada) > 0)
    ponderada <- rbind(ponderada,
      data.table("metrica" = "base", 
        "val" = prod(as.numeric(ponderada$val))/100^(nrow(ponderada)-1)))
  else
    ponderada <- data.table("metrica" = "base", 
                            "val" = NA)
  ponderada[["cat"]] <- sapply(ponderada$val, function(x) {
    if(is.na(x)) "vazia"
    else if(x < 50) "ruim"
    else if(x < 75) "regular"
    else if(x < 90) "otima"
    else "excelente"})
  ponderada$val <- formatC(ponderada$val, digits = 2, format = "f")

  return(list("ponderada"=ponderada, "metricas"=metricas,
              "anual"=anual, "cons"=cons, "unic"=unic, "temp"=temp))
}
