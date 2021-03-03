library(dplyr)
library(data.table)

source("R/proadi-athena.R")
source("R/dominio-athena.R")
source("R/funcoes-filtro.R")

if(!exists("database_name") || !exists("table_name"))
  stop("ERRO! database_name | table_name")

path_to_data <- paste0("bases/scripts/filtros/", database_name, ".", table_name, "/")


fwrite <- function(data, path, sep=";", dec=",") {
  s=sep; d=dec
  data.table::fwrite(data, paste0(path_to_data, "out/", path), sep=s, dec=d, row.names=F)
}

print("Acréscimo de registros")
if(acrescimo_de_registros(database_name, table_name, path_to_data)) {
  parametros_database <- list("database_name"=database_name,
                              "table_name"=table_name)
  if(!dir.exists(path_to_data))
    dir.create(path_to_data)
  if(dir.exists(paste0(path_to_data,"out/")))
    unlink(paste0(path_to_data,"out/"), recursive = T, force = T)
  dir.create(paste0(path_to_data,"out/"))
  
  source("R/geral.R")
  
  if(database_name == "proadi_s3") {
    print("Ajustes com o relatório de padronização")
    relatorio <- relatorio_atualizado_S3(table_name, referencia_subs)
    if(!is.null(relatorio)) {
      total_ajustado <- totais_variavel_relatorio_S3(relatorio, referencia_subs, table_name)
      
      dt <- full_join(dt, total_ajustado$geral, by = "variavel")
      if(any(dt$nao.nulo > dt$total_raw))
        dt$total_raw <- NULL
      else {
        dt$total <- NULL
        colnames(dt)[colnames(dt) == "total_raw"] <- "total"
        fwrite(dt, "diagnostico.csv", sep = ";")
      }
      
      if(exists("referencia_subs")) {
        if(!is.null(dt_ANO)) {
          total_ajustado$ANO$variavel <- NULL
          total_ajustado$ANO <- sapply(total_ajustado$ANO, function(x) sum(as.numeric(x))) %>% 
            cbind(., names(.)) %>% data.table %>% `colnames<-`(c("total_raw","ano"))
          total_ajustado$ANO$total_raw <- as.numeric(total_ajustado$ANO$total_raw)
          
          dt <- full_join(dt_ANO, totais, by = "ano")
          if(any(dt$nao.nulo > dt$total_raw))
            dt$total_raw <- NULL
          else {
            dt$total <- NULL
            colnames(dt)[colnames(dt) == "total_raw"] <- "total"
            fwrite(dt, paste0("diagnostico-ano.csv"), sep = ";")
          }
        }
        if(!is.null(dt_UF)) {
          total_ajustado$UF$variavel <- NULL
          total_ajustado$UF <- sapply(total_ajustado$UF, function(x) sum(as.numeric(x))) %>% 
            cbind(., names(.)) %>% data.table %>% `colnames<-`(c("total_raw","uf"))
          total_ajustado$UF$total_raw <- as.numeric(total_ajustado$UF$total_raw)
          
          dt <- full_join(dt_UF, totais, by = "uf")
          if(any(dt$nao.nulo > dt$total_raw))
            dt$total_raw <- NULL
          else {
            dt$total <- NULL
            colnames(dt)[colnames(dt) == "total_raw"] <- "total"
            fwrite(dt, paste0("diagnostico-uf.csv"), sep = ";")
          }
        }
      }
    }
  }
  
  print("Disponibilidade dos arquivos")
  disponibilidade_arquivos(database_name, table_name, referencia_subs)
  print("Tabela de dominios")
  tabela_dominio_filtro(variaveis, referencia_subs, database_name, table_name,filtro)
  print("Informações gerais da base de dados")
  informacoes_gerais(variaveis, referencia_subs, database_name, table_name, path_to_data)
}