library(dplyr)
rm(list = ls() %>% .[!. %in% c("database_name","table_name")])

library(data.table)
library(stringr)
library(stringi)
library(knitr)
library(kableExtra)

defaultW <- getOption("warn")
options(warn = -1)

source("API/graficos.R")


#' Definição da base ###########################################################
if(!exists("table_name") || !exists("database_name")) stop("Cade a definição da base???")
path_to_data <- paste0("bases/scripts/", database_name, ".", table_name, "/")


#' Disponibilidade dos dados ###################################################
#' Tabela de arquivos de valores faltantes
path_dt_disponibilidade <- paste0(path_to_data, "out/arquivos-faltantes-athena.csv")
if(file.exists(path_dt_disponibilidade)) {
  dt_disponibilidade <- read.csv2(path_dt_disponibilidade, stringsAsFactors = F)
  dt_disponibilidade$disponivel <- NULL
  colnames(dt_disponibilidade)[colnames(dt_disponibilidade) == "ano"] <- "Ano"
  colnames(dt_disponibilidade)[colnames(dt_disponibilidade) == "mes"] <- "Mês"
  colnames(dt_disponibilidade)[colnames(dt_disponibilidade) == "uf"] <- "UF"
}


#' Variáveis existentes e mudanças ocorridas ###################################
#' Quantidade de variáveis
path_relatorio <- paste0(path_to_data, "out/", toupper(table_name), "_RELATORIO.csv")
if(file.exists(path_relatorio)) {
  relatorio <- read.csv2(path_relatorio, stringsAsFactors = F)
  ggsave(grafico_quantidade(relatorio),
         file = "API/relatorio/imagens/quantidade.png",
         width = 4, height = 2, dpi = 300, scale = 1.75)
}

#' Tabela de domínio
path_dt_dom <- paste0(path_to_data, "out/dominio-unique-athena.csv")
if(file.exists(path_dt_dom)) {
  dt_dom <- read.csv2(path_dt_dom, stringsAsFactors = F)
  dt_dom$variavel <- tolower(dt_dom$variavel)
  variaveis <- dt_dom$variavel %>% unique %>% sort
  anos <- dt_dom$ano %>% unique %>% sort %>% as.character
  
  #' Registros totalmente nulos em período
  #' Não disponivel para os sia-apac/bpai/raas_ad/raas_psi
  #if(any(dt_dom$null)) {
  #  dt_NA <- dt_dom[dt_dom$null, c("variavel","ano","null")]
  #  dt_NA <- dcast(dt_NA, variavel ~ ano, value.var="null")
  #  dt_NA[is.na(dt_NA)] <- TRUE
  #  setDT(dt_NA)
  #  dt_NA <- dt_NA[, lapply(.SD, function(x) 
  #    cell_spec(rep(" ", length(x)), format="latex", 
  #              background=ifelse(x, "white", "#39928f"))), by="variavel"]
    #dt_NA$variavel <- str_replace_all(dt_NA$variavel, "", "\\\\")
  #  colnames(dt_NA)[1] <- "Variável"
  #}
  
  #' Tabela de mudanças
  dts_mud <- lapply(anos, function(w)
    dt_dom[(dt_dom$mud_dom | dt_dom$mud_nchar) & dt_dom$ano == w, 
                 c("variavel","dominio","nchar_max")] %>% 
      `colnames<-`(c("Variável","Domínio","Tamanho"))) %>% `names<-`(anos) %>% 
    .[!sapply(., is.null)]
  if(length(dts_mud) == 0) rm(dts_mud)
  
  #' Tabela de domínio para variáveis sem descrição
  dt_dom <- dt_dom[, c("variavel","dominio")] %>% 
    `colnames<-`(c("Variável","Domínio"))
  setDT(dt_dom)
  dt_dom <- dt_dom[, lapply(.SD, function(x)
    str_split(x, ", ") %>% unlist %>% unique %>% .[!is.na(.) & . != "NA"] %>% head(., n=10) %>% paste0(., collapse=", ")
  ), by="Variável"]
  dt_dom <- dt_dom[!is.na(dt_dom$Domínio) & dt_dom$Domínio != "", ]
}


#' Resultados ##################################################################
#' Completude, conformidade e acurácia
path_dt_geral <- paste0(path_to_data, "out/diagnostico.csv")
if(file.exists(path_dt_geral)) {
  dt_geral <- read.csv2(path_dt_geral, stringsAsFactors = F)
  dt_geral[["completude"]] <- dt_geral$nao.nulo*100/dt_geral$total
  dt_geral$completude[is.na(dt_geral$completude)] <- 0
  dt_geral[["conformidade"]] <- dt_geral$conformes*100/dt_geral$nao.nulo
  dt_geral[["acuracia"]] <- dt_geral$acurados*100/dt_geral$conformes
  
  ggsave(grafico_estratificacao(dt_geral, "completude", "Completude", "completa"), 
         file = "API/relatorio/imagens/comp.png",
         width = 4, height = 2, dpi = 300, scale = 1.5)
  ggsave(grafico_estratificacao(dt_geral, "conformidade", "Conformidade", "conforme"), 
         file = "API/relatorio/imagens/conf.png",
         height = 2, dpi = 300, scale = 1.5)
  ggsave(grafico_estratificacao(dt_geral, "acuracia", "Acurácia", "acurada"),
         file = "API/relatorio/imagens/ac.png",
         height = 2, dpi = 300, scale = 1.5)

  #' Resultados por ano
  path_dt_anual <- paste0(path_to_data, "out/diagnostico-ano.csv")
  if(file.exists(path_dt_anual)) {
    dt_anual <- read.csv2(path_dt_anual, stringsAsFactors = F)
    dt_anual[["completude"]] <- dt_anual$nao.nulo*100/dt_anual$total
    dt_anual[["conformidade"]] <- dt_anual$conformes*100/dt_anual$nao.nulo_conformes
    dt_anual[["acuracia"]] <- dt_anual$acurados*100/dt_anual$conformes_acurados
    if(max(nchar(as.character(dt_anual$ano))) == 2)
      dt_anual$ano <- paste0("20", dt_anual$ano) %>% as.numeric
    
    ggsave(grafico_comparacao_anual(dt_anual), 
           file = "API/relatorio/imagens/anual.png",
           width = 4, height = 2, dpi = 300, scale = 1.75)
    
    ggsave(grafico_anual(dt_anual, "completude", "Completude [%]"), 
           file = "API/relatorio/imagens/comp-anual.png",
           width = 4, height = 2, dpi = 300, scale = 1.75)
    ggsave(grafico_anual(dt_anual, "conformidade", "Conformidade [%]"), 
           file = "API/relatorio/imagens/conf-anual.png",
           width = 4, height = 2, dpi = 300, scale = 1.75)
    ggsave(grafico_anual(dt_anual, "acuracia", "Acurácia [%]"), 
           file = "API/relatorio/imagens/ac-anual.png",
           width = 4, height = 2, dpi = 300, scale = 1.75)
  }
  
  #' Resultados por UF
  path_dt_estadual <- paste0(path_to_data, "out/diagnostico-uf.csv")
  if(file.exists(path_dt_estadual)) {
    dt_estadual <- read.csv2(path_dt_estadual, stringsAsFactors = F)
    dt_estadual[["completude"]] <- dt_estadual$nao.nulo*100/dt_estadual$total
    dt_estadual[["conformidade"]] <- dt_estadual$conformes*100/dt_estadual$nao.nulo_conformes
    dt_estadual[["acuracia"]] <- dt_estadual$acurados*100/dt_estadual$conformes_acurados
    dt_estadual$uf <- toupper(dt_estadual$uf)
    
    ggsave(grafico_estadual(dt_estadual, "completude", "Completude [%]"), 
           file = "API/relatorio/imagens/comp-uf.png",
           width = 4, height = 3, dpi = 300, scale = 1.75)
    ggsave(grafico_estadual(dt_estadual, "conformidade", "Conformidade [%]"), 
           file = "API/relatorio/imagens/conf-uf.png",
           width = 4, height = 3, dpi = 300, scale = 1.75)
    ggsave(grafico_estadual(dt_estadual, "acuracia", "Acurácia [%]"), 
           file = "API/relatorio/imagens/ac-uf.png",
           width = 4, height = 3, dpi = 300, scale = 1.75)
  }
  
  #' Tabela de registros inconformes/inacurados
  path_dt_inconf_inac <- paste0(path_to_data, "out/inconformes-inacurados.csv")
  if(file.exists(path_dt_inconf_inac)) {
    dt_inconf_inac <- read.csv2(path_dt_inconf_inac, stringsAsFactors = F) %>%
      `colnames<-`(c("Variável","Registros inconformes", "Registros inacurados")) %>% na.omit
    
    setDT(dt_inconf_inac)
    dt_inconf_inac <- dt_inconf_inac[, lapply(.SD, function(x) 
      str_replace_all(x, "[:space:]", "\\\\s"))]
    
    dt_inconf <- dt_inconf_inac[, c(1,2)] %>% na.omit
    if(nrow(dt_inconf) == 0) rm(dt_inconf)
    dt_inac <- dt_inconf_inac[, c(1,3)] %>% na.omit
    if(nrow(dt_inac) == 0) rm(dt_inac)
  }
  
  #' Acurácia
  #' Tabela de outliers
  path_dt_outlier <- paste0(path_to_data, "out/outliers.csv")
  if(file.exists(path_dt_outlier)) {
    dt_outlier <- read.csv2(path_dt_outlier, stringsAsFactors = F) %>%
      `colnames<-`(c("Variável","Valores atípicos"))
    dt_outlier$Variável <- tolower(dt_outlier$Variável)
  }
  
  #' Tabela de ignorados
  path_dt_ignorado <- paste0(path_to_data, "out/ignorados-", table_name, ".csv")
  if(file.exists(path_dt_ignorado)) {
    dt_ignorado <- read.csv2(path_dt_ignorado, stringsAsFactors = F) %>%
      `colnames<-`(c("Variável","Registros ignorados"))
    dt_ignorado$Variável <- tolower(dt_ignorado$Variável)
    dt_ignorado$`Registros ignorados` <- str_split(dt_ignorado$`Registros ignorados`, ", ") %>% 
      sapply(., function(x) str_replace_all(unique(x), "[:blank:]", "\\\\s") %>% 
      paste0(., collapse = ", "))
  }
}


#' Consistencia
path_dt_cons_descricao <- paste0(path_to_data, "out/consistencia-descricao.csv")
path_dt_cons_anual <- paste0(path_to_data, "out/consistencia-ano.csv")
if(file.exists(path_dt_cons_descricao) && file.exists(path_dt_cons_anual)) {
  dt_cons_descricao <- read.csv2(path_dt_cons_descricao, stringsAsFactors = F)
  dt_cons_descricao$desc <- tolower(dt_cons_descricao$desc)
  n_cons <- nrow(dt_cons_descricao)

  dt_cons_anual <- read.csv2(path_dt_cons_anual, stringsAsFactors = F)
  dt_cons_anual[is.na(dt_cons_anual)] <- 0
  if(max(nchar(as.character(dt_cons_anual$ano))) == 2)
    dt_cons_anual$ano <- paste0("20", dt_cons_anual$ano) %>% as.numeric
    
  names <- lapply(seq(n_cons), function(x) c(paste0("T", x), paste0("t", x)))
    
  dt_cons_anual[["consistencia"]] <- apply(dt_cons_anual[,paste0("t", seq(n_cons))], 1, sum) *1000/
    apply(dt_cons_anual[, paste0("T", seq(n_cons))], 1, sum)
  
  dt_cons_anual_aux <- dt_cons_anual
  ggsave(grafico_anual(dt_cons_anual, "consistencia", "Consistência\n[partes por mil]"),
         file = "API/relatorio/imagens/cons-anual.png",
         width = 4, height = 2, dpi = 300, scale = 1.75)
  
  dt_cons <- sapply(names, function(x) sum(dt_cons_anual[x[2]])*1000 / sum(dt_cons_anual[x[1]])) %>% 
    data.frame %>% `colnames<-`("falhas")
  dt_cons <- cbind(dt_cons_descricao[, c("teste","desc")], dt_cons) %>% 
      `colnames<-`(c("Teste","Descrição","Falhas [partes por mil]"))
  dt_cons$`Falhas [partes por mil]` <- formatC(dt_cons$`Falhas [partes por mil]`, 
                                               digits = 3, format = "f") %>% trimws
  dt_cons$`Falhas [partes por mil]`[dt_cons$`Falhas [partes por mil]` == "NaN"] <- ""

  #' Resultados por UF
  path_dt_cons_estadual <- paste0(path_to_data, "out/consistencia-uf.csv")
  if(file.exists(path_dt_cons_estadual)) {
    dt_cons_estadual <- read.csv2(path_dt_cons_estadual, stringsAsFactors = F)
    colnames(dt_cons_estadual)[colnames(dt_cons_estadual) == "UF"] <- "uf"
    
    names <- lapply(seq(n_cons), function(x) c(paste0("T", x), paste0("t", x)))
    dt_cons_estadual[["consistencia"]] <- apply(dt_cons_estadual[,paste0("t", seq(n_cons))], 1, sum) *1000/
      apply(dt_cons_estadual[, paste0("T", seq(n_cons))], 1, sum)
    
    ggsave(grafico_estadual(dt_cons_estadual, "consistencia", "Consistência\n[partes por mil]"),
           file =  "API/relatorio/imagens/cons-uf.png",
           width = 4, height = 3, dpi = 300, scale = 1.75)
  }
}

#' Unicidade
path_dt_unic <- paste0(path_to_data, "out/unicidade.csv")
if(file.exists(path_dt_unic)) {
  dt_unic <- read.csv2(path_dt_unic, stringsAsFactors = F)
  dt_unic[["unicidade"]] <- 100 - dt_unic$falhas*100 / dt_unic$total
  dt_unic[["unicidade_geral"]] <- 100 - dt_unic$falhas_geral*100 / dt_unic$total_geral
  dt_unic$variavel <- tolower(dt_unic$variavel)
  n_unic <- nrow(dt_unic)
  
  dt_unic <- dt_unic[, c("teste","variavel","unicidade")] %>% 
    `colnames<-`(c("Teste","Variável","Unicidade [%]"))
  dt_unic$`Unicidade [%]` <- formatC(dt_unic$`Unicidade [%]`, digits = 2, format = "f")
  
  path_dt_unic_anual <- paste0(path_to_data, "out/unicidade-ano.csv")
  if(file.exists(path_dt_unic_anual)) {
    dt_unic_anual <- read.csv2(path_dt_unic_anual, stringsAsFactors = F)
    dt_unic_anual[["unicidade"]] <- 100 - dt_unic_anual$falhas *100/ dt_unic_anual$total
    
    ggsave(grafico_anual(dt_unic_anual, "unicidade", "Unicidade [%]"), 
           file = "API/relatorio/imagens/unic-anual.png",
           width = 4, height = 2, dpi = 300, scale = 1.75)
  }
  path_dt_unic_estadual <- paste0(path_to_data, "out/unicidade-uf.csv")
  if(file.exists(path_dt_unic_estadual)) {
    dt_unic_estadual <- read.csv2(path_dt_unic_estadual, stringsAsFactors = F)
    dt_unic_estadual[["unicidade"]] <- 100 - dt_unic_estadual$falhas *100/ dt_unic_estadual$total
    
    ggsave(grafico_estadual(dt_estadual, "unicidade", "Unicidade [%]"), 
           file = "API/relatorio/imagens/unic-uf.png",
           width = 4, height = 3, dpi = 300, scale = 1.75)
  }
  path_dt_unic_freq <- paste0(path_to_data, "out/unicidade-frequencia.csv")
  if(file.exists(path_dt_unic_freq)) {
    dt_unic_freq <- read.csv2(path_dt_unic_freq, stringsAsFactors = F)
    dt_unic_freq$id <- formatC(dt_unic_freq$id, digits = 3, format = "f")
    dt_unic_freq <- dt_unic_freq[, c("categoria","id")] %>% 
      `colnames<-`(c("Categoria","Frequência [%]"))
  }
}

#' Temporalidade ###############################################################
path_dt_temp <- paste0(path_to_data, "out/temporalidade.csv")
if(file.exists(path_dt_temp)) {
  dt_temp <- read.csv2(path_dt_temp, stringsAsFactors = F)
  path_dt_temp_anual <- paste0(path_to_data, "out/temporalidade-ano.csv")
  if(file.exists(path_dt_temp_anual)) {
    dt_temp_anual <- read.csv2(path_dt_temp_anual, stringsAsFactors = F)
    if(max(nchar(as.character(dt_temp_anual$ano))) == 2)
      dt_temp_anual$ano <- paste0("20", dt_temp_anual$ano) %>% as.numeric
    
    ggsave(grafico_temporalidade_anual(dt_temp_anual),
           file = "API/relatorio/imagens/temp-anual.png",
           width = 4, height = 2, dpi = 300, scale = 1.75)
  }
  path_dt_temp_estadual <- paste0(path_to_data, "out/temporalidade-uf.csv")
  if(file.exists(path_dt_temp_estadual)) {
    dt_temp_estadual <- read.csv2(path_dt_temp_estadual, stringsAsFactors = F)
    dt_temp_estadual$uf <- toupper(dt_temp_estadual$uf)
  }
  
  dt_temp <- dt_temp[, c("teste","V1","V2","Median","Min.","Max.")] %>% 
    `colnames<-`(c("Teste","Variável Inicial","Variável final","Mediana","Min.", "Max."))
  dt_temp$`Variável Inicial` <- tolower(dt_temp$`Variável Inicial`)
  dt_temp$`Variável final` <- tolower(dt_temp$`Variável final`)
}
  

#' Considerações finais ########################################################
#' Médias ponderadas
dt_ponderada <- data.table("metrica" = NA, "val" = NA)
if(exists("dt_geral")) {
  dt_ponderada <- rbind(dt_ponderada,
                        data.table("metrica" = c("Completude", "Conformidade", "Acurácia"),
                                   "val" = 
c(sum((dt_geral$completude*dt_geral$total))/sum(dt_geral$total), 
sum((dt_geral$conformidade*dt_geral$total)[!is.na(dt_geral$conformidade)]) / 
  sum(dt_geral$total[!is.na(dt_geral$conformidade)]),
sum((dt_geral$acuracia*dt_geral$total)[!is.na(dt_geral$acuracia)]) / 
  sum(dt_geral$total[!is.na(dt_geral$acuracia)]))))
}
if(exists("dt_cons") && exists("dt_cons_anual")) {
  tmp <- !(as.numeric(dt_cons$`Falhas [partes por mil]`) %>% is.na)
  dt_ponderada <- rbind(dt_ponderada,
                        data.table("metrica" = "Consistência",
"val" = sum((100 - as.numeric(dt_cons$`Falhas [partes por mil]`[tmp])/10) * 
              sapply(dt_cons_anual[, paste0("T", seq(n_cons))[tmp]], sum)) / 
                sum(sapply(dt_cons_anual[, paste0("T", seq(n_cons))[tmp]], sum))))
}
if(exists("dt_unic")) {
  aux <- read.csv2(path_dt_unic, stringsAsFactors = F)
  dt_ponderada <- rbind(dt_ponderada,
                        data.table("metrica" = "Unicidade",
"val" = 100 - aux$falhas_geral[1]*100 / aux$total_geral[1]))
} 
dt_ponderada <- dt_ponderada[-1,]

ggsave(grafico_comparacao_ponderada(dt_ponderada), 
       file = "API/relatorio/imagens/ponderada.png",
       width = 3, height = 2, dpi = 300, scale = 1.75)

ponderada <- dt_ponderada
colnames(ponderada) <- c("Métrica","Valor")
ponderada$Valor <- formatC(as.numeric(ponderada$Valor), digits = 2, format = "f")

ponderada <- list("completude" = dt_ponderada$val[dt_ponderada$metrica == "Completude"],
                  "conformidade" = dt_ponderada$val[dt_ponderada$metrica == "Conformidade"],
                  "acuracia" = dt_ponderada$val[dt_ponderada$metrica == "Acurácia"],
                  "consistencia" = dt_ponderada$val[dt_ponderada$metrica == "Consistência"],
                  "unicidade" = dt_ponderada$val[dt_ponderada$metrica == "Unicidade"])
ponderada <- lapply(ponderada, function(x) {
  if(length(x) == 0) return(NULL)
  x <- as.numeric(x)
  list("val" = formatC(x, digits = 2, format = "f"),
       "cat" = if(is.na(x)) "vazia"
       else if(x < 50) "ruim"
       else if(x < 75) "regular"
       else if(x < 90) "ótimo"
       else "excelente")
})
ponderada <- ponderada[!sapply(ponderada, is.null)]


#' Dicionário de dados #########################################################
path_dt_dict <- paste0(path_to_data, "dict-", table_name, ".csv")
if(file.exists(path_dt_dict)) {
  dt_dict <- read.csv2(path_dt_dict, stringsAsFactors = F)
  colnames(dt_dict) <- toupper(colnames(dt_dict)) %>% 
    stringi::stri_trans_general(str = ., id = "Latin-ASCII")
  
  #' Tabela de domínio
  if(exists("dt_dom")) {
    dt_dom <- dt_dom[dt_dom$Variável %in% tolower(dt_dict$VARIAVEL[is.na(dt_dict$VARIANTES) | 
                     dt_dict$VARIANTES == ""]), ]
  }
  
  #' Tabela de registros ignorados
  if(all(c("IGNORADOS","VARIANTES") %in% colnames(dt_dict))) {
    dt_ign <- lapply(seq(nrow(dt_dict)), function(x) {
      spl <- str_split(dt_dict$IGNORADOS[x], ", ", simplify = T) %>% unlist
      spl[!spl %in% unlist(str_split(dt_dict$VARIANTES[x], ", ", simplify = T))]
    })
    dt_ign <- dt_ign[!sapply(dt_ign, function(x) length(x) == 0 || x == "")]
    if(length(dt_ign) == 0) rm(dt_ign)
  }
  
  #' Formatação do dicionário
  cols <- c("VARIAVEL","DESCRICAO","MIN","MAX","VALORES.VALIDOS")
  cols <- cols[cols %in% colnames(dt_dict)]
  if(all(c("VARIAVEL","MIN","MAX") %in% cols)) {
    dt_dict$VARIAVEL <- tolower(dt_dict$VARIAVEL)
    
    if(!"VALORES.VALIDOS" %in% cols)
      dt_dict[["VALORES.VALIDOS"]] <- ""
    if(!"DESCRICAO" %in% cols)
      dt_dict[["DESCRICAO"]] <- ""
    
    dt_dict$VALORES.VALIDOS[is.na(dt_dict$VALORES.VALIDOS)] <- ""
    dt_dict$DESCRICAO[is.na(dt_dict$DESCRICAO)] <- ""
    dt_dict$DESCRICAO <- str_remove_all(dt_dict$DESCRICAO, "(?!\\))[[:punct:]]$")
    dt_dict$DESCRICAO <- paste0(dt_dict$DESCRICAO, ". ", dt_dict$VALORES.VALIDOS)
    dt_dict$DESCRICAO <- str_remove_all(dt_dict$DESCRICAO, "^[:punct:] ")
    
    dt_dict$MAX <- paste0("[", dt_dict$MIN, ", ", dt_dict$MAX, "]")
    dt_dict <- dt_dict[, c("VARIAVEL","DESCRICAO","MAX")]
    
    colnames(dt_dict)[colnames(dt_dict) == "VARIAVEL"] <- "Variável"
    colnames(dt_dict)[colnames(dt_dict) == "DESCRICAO"] <- "Descrição"
    colnames(dt_dict)[colnames(dt_dict) == "MAX"] <- "Tamanho"
  }
  else dt_dict <- NULL
}

#' Resultados gerais ###########################################################
if(exists("dt_geral")) {
  setDT(dt_geral)
  dt_geral$total <- dt_geral$nao.nulo <- dt_geral$conformes <- dt_geral$acurados <- NULL
  dt_geral <- dt_geral[, lapply(.SD, function(x) trimws(formatC(x, digits = 2, format = "f"))), 
                       by = "variavel"] %>% 
    `colnames<-`(c("Variável","Completude [%]","Conformidade [%]","Acurácia [%]"))
  dt_geral$Variável <- tolower(dt_geral$Variável)
  dt_geral[dt_geral == "NaN"] <- "0.00"
  dt_geral[dt_geral == "NA"] <- ""
}

#' Tabela de resultados anuais
if(exists("dt_anual")) {
  dt_anual$total <- dt_anual$nao.nulo <- dt_anual$nao.nulo_conformes <-
    dt_anual$conformes <- dt_anual$conformes_acurados <- dt_anual$acurados <- NULL
  
  colnames(dt_anual)[colnames(dt_anual) == "ano"] <- "Ano"
  colnames(dt_anual)[colnames(dt_anual) == "completude"] <- "Completude [%]"
  colnames(dt_anual)[colnames(dt_anual) == "conformidade"] <- "Conformidade [%]"
  colnames(dt_anual)[colnames(dt_anual) == "acuracia"] <- "Acurácia [%]"
  colnames(dt_anual)[colnames(dt_anual) == "consistencia"] <- "Consistência [%]"
  colnames(dt_anual)[colnames(dt_anual) == "unicidade"] <- "Unicidade [%]"
  
  setDT(dt_anual)
  dt_anual <- dt_anual[, lapply(.SD, function (x) formatC(x, digits = 2, format = "f")), by = "Ano"]
  dt_anual <- dt_anual[order(dt_anual$Ano), ]
}

#' Tabela de resultados estaduais
if(exists("dt_estadual")) {
  dt_estadual$total <- dt_estadual$nao.nulo <- dt_estadual$nao.nulo_conformes <-
    dt_estadual$conformes <- dt_estadual$conformes_acurados <- dt_estadual$acurados <- NULL
  
  colnames(dt_estadual)[colnames(dt_estadual) == "uf"] <- "UF"
  colnames(dt_estadual)[colnames(dt_estadual) == "completude"] <- "Completude"
  colnames(dt_estadual)[colnames(dt_estadual) == "conformidade"] <- "Conformidade"
  colnames(dt_estadual)[colnames(dt_estadual) == "acuracia"] <- "Acurácia"
  colnames(dt_estadual)[colnames(dt_estadual) == "consistencia"] <- "Consistência"
  colnames(dt_estadual)[colnames(dt_estadual) == "unicidade"] <- "Unicidade"
  
  setDT(dt_estadual)
  dt_estadual <- dt_estadual[, lapply(.SD, function (x) formatC(x, digits = 2, format = "f")), by = "UF"]
  dt_estadual <- dt_estadual[order(dt_estadual$UF), ]
}

#' Testes de inconsistência #####################################################
if(exists("dt_cons_anual") && exists("dt_cons_descricao")) {
  dt_cons_anual <- dt_cons_anual[, c("ano",paste0("t", seq(nrow(dt_cons_descricao))))] %>% 
    `colnames<-`(c("Ano",paste0("T", seq(nrow(dt_cons_descricao)))))
  
  cons_descricao <- dt_cons_descricao[, c("teste","desc")] %>% 
    `colnames<-`(c("Teste","Descrição"))
  if(!is.null(dt_cons_descricao[["desc_completa"]]))
    cons_descricao$Descrição <- dt_cons_descricao$desc_completa
  cons_descricao <- apply(cons_descricao, 1, function(x) {
    paste0("* ", "\\textbf{", x[1], "}: ", x[2], "\n")
  }) %>% paste(., collapse = "")
}

#' Definição das dimensões
dimensoes <- dt_ponderada$metrica
if(exists("dt_temp")) dimensoes <- c(dimensoes, "Temporalidade")

dimensoes_ano <- NULL
if(exists("dt_anual")) {
  dimensoes_ano <- colnames(dt_anual)
  if(exists("dt_cons_anual")) dimensoes_ano <- c(dimensoes_ano, "Consistência")
  if(exists("dt_unic_anual")) dimensoes_ano <- c(dimensoes_ano, "Unicidade")
  if(exists("dt_temp_anual")) dimensoes_ano <- c(dimensoes_ano, "Temporalidade")
}
dimensoes_uf <- NULL
if(exists("dt_estadual")) {
  dimensoes_uf <- colnames(dt_estadual)
  if(exists("dt_cons_estadual")) dimensoes_uf <- c(dimensoes_uf, "Consistência")
  if(exists("dt_unic_estadual")) dimensoes_uf <- c(dimensoes_uf, "Unicidade")
}


#' Qualidade geral da base
qualidade <- prod(as.numeric(dt_ponderada$val))/100^(nrow(dt_ponderada)-1)
qualidade <- list("val" = formatC(qualidade, digits = 2, format = "f"),
                  "cat" = if(is.na(qualidade)) "vazia"
                          else if(qualidade < 50) "ruim"
                          else if(qualidade < 75) "regular"
                          else if(qualidade < 90) "ótima"
                          else "excelente")

#' Texto sobre a base
try({sobre_base <- read_lines(paste0(path_to_data, table_name, ".txt"))})
#' Informações gerais
path_dt_base <- paste0(path_to_data, "out/informacoes-gerais-", table_name, ".csv")
if(file.exists(path_dt_base)) {
  dt_base <- read.csv2(path_dt_base, stringsAsFactors = F)
  colnames(dt_base) <- dt_base[1, ]
  dt_base <- dt_base[-1, ]
}


options(knitr.kable.NA = '')

#' Geração do relatório ########################################################

writeLines(paste0("\\base{", str_replace_all(table_name, "\\_", "-"), "}"), "API/relatorio/base.tex")

if(!dir.exists("API/gerados"))
  dir.create("API/gerados")

rmarkdown::render("API/relatorio/qualidade_dados.Rmd",
                  output_format = "pdf_document", 
                  output_file = paste0("../gerados/qualidade-dados-", table_name, ".pdf"))

#if(file.exists("API/relatorio/base.tex"))
#  file.remove("API/relatorio/base.tex")

#options(warn = defaultW)
#rm(list = ls())