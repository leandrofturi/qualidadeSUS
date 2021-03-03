options(warn = -1)

source("R/dominio.R")
source("R/athena.R")
source("R/athena-agrupado.R")
source("R/athena-substring-agrupado.R")

database_name_report <- c("proadi_s3")

################################################################################

if(!exists("database_name") || !exists("table_name"))
  stop("ERRO! database_name | table_name")
    
path_to_data <- paste0("bases/scripts/", database_name, ".", table_name, "/")
if(!dir.exists(path_to_data))
  dir.create(path_to_data)
if(dir.exists(paste0(path_to_data,"out/")))
  unlink(paste0(path_to_data,"out/"), recursive = T, force = T)
dir.create(paste0(path_to_data,"out/"))

print("Acréscimo de registros")
if(acrescimo_de_registros(database_name, table_name, path_to_data)) {
  
  ##############################################################################
  
  dict_path <- paste0(path_to_data,"dict-",table_name,".csv")
  
  if(!exists("referencia_subs") || !file.exists(dict_path))
    stop("ERRO! referencia_subs | dict")
  
  dict <- read.csv2(dict_path, stringsAsFactors = F)
  #' Remoção de acentos 
  colnames(dict) <- stringi::stri_trans_general(str = colnames(dict), 
                                                id = "Latin-ASCII") %>% toupper
  path_cnv <- paste0("bases/dicionarios/cnv/", table_name, "/")
  if(!all(is.na(dict$VARIANTES)))
    dict$VARIANTES[str_detect(dict$VARIANTES, ".CNV$|.cnv$")] <- paste0(path_cnv, 
                                                                      dict$VARIANTES[str_detect(dict$VARIANTES, ".CNV$|.cnv$")])
  
  names(referencia_subs) <- toupper(names(referencia_subs))
  #' Atualizar datas do dicionário
  print("Atualizar datas do dicionário")
  dict <- atualizar_datas_dict(dict_path, referencia_subs, database_name, table_name)
  
  print("Variáveis")
  variaveis <- variaveis_athena(database_name, table_name) %>% toupper
  #' Remoção de variáveis estranhas que aparecem
  variaveis <- variaveis[variaveis %in% toupper(dict$VARIAVEL)] %>% 
    .[str_detect(., "[:graph:]")] %>% .[!is.na(.)]
  
  ##############################################################################
  
  ws <- getwd()
  setwd(paste0(path_to_data, "out/"))
  
  ##############################################################################
  
  #' Disponibilidade dos dados
  print("Disponibilidade dos dados")
  disponibilidade_arquivos(database_name, table_name, referencia_subs)
  #' Registros nulos em determinado ano
  print("Registros nulos em determinado ano")
  NAs_anual(variaveis, referencia_subs, database_name, table_name)
  #' Identificação de mudanças de tamanho e domínio
  print("Identificação de mudanças de tamanho e domínio")
  mudanca_tamanho_anual(variaveis, referencia_subs, database_name, table_name)
  mudanca_dominio_anual(variaveis, referencia_subs, database_name, table_name)
  #' Regfistros irrelevantes
  print("Regfistros irrelevantes")
  irrelevantes_agr(variaveis, dict, database_name, table_name)
  
  #' Levantamento de registros frequentes em variáveis sem descrição
  if("DESCRICAO" %in% colnames(dict) && "VALORES.VALIDOS" %in% colnames(dict))
    v_desc <- dict$VARIAVEL[(is.na(dict[["DESCRICAO"]]) | dict[["DESCRICAO"]] == "") & 
                (is.na(dict[["VALORES.VALIDOS"]]) | dict[["VALORES.VALIDOS"]] == "")]
  else if("VALORES.VALIDOS" %in% colnames(dict))
    v_desc <- dict$VARIAVEL[is.na(dict[["VALORES.VALIDOS"]]) | dict[["VALORES.VALIDOS"]] == ""]
  else
    v_desc <- variaveis
  
  print("Tabela de domínio")
  tabela_dominio(v_desc, database_name, table_name)
  
  ##############################################################################
  
  #' Completude, conformidade e acurácia
  print("Completude, conformidade e acurácia")
  dt <- diag_agr_athena(variaveis, database_name, table_name, dict)
  
  #' Tabelas auxiliares
  print("Tabelas auxiliares")
  v_freq <- dt$variavel[dt$conformes != dt$nao.nulo] %>% .[!is.na(.)]
  # frequencia_agr(v_freq[!is.na(v_freq)], database_name, table_name)
  inconformes_agr(v_freq, dict, database_name, table_name)
  v_numericas <- dicionario_formatado(dict)$numericas %>% .[!is.na(.)]
  outliers_agr(v_numericas, dict, database_name, table_name)
  
  
  #' Ajustes de acordo com o relatório de integração
  if(check_file_exist_s3("proadi-misc", paste0("s3_report/", toupper(table_name), "_RELATORIO.csv.gz")) && 
     database_name %in% database_name_report) {
    print("Relatório de integração")
    relatorio <- S3_2datatable("proadi-misc", paste0("s3_report/", toupper(table_name), "_RELATORIO.csv.gz"),
                               file_extension = "gzip")
    relatorio <- atualizar_relatorio_S3(relatorio, referencia_subs, database_name, table_name)
    
    #' Ajustes de registros nulos em determinado ano
    NAs_anual(variaveis, referencia_subs, database_name, table_name, relatorio)
    
    totais <- totais_variavel_relatorio_S3(relatorio, referencia_subs, table_name)
    dt <- full_join(dt, totais, by = "variavel")
    if(any(dt$nao.nulo > dt$total_raw))
      dt$total_raw <- NULL
    else {
      dt$total <- NULL
      colnames(dt)[colnames(dt) == "total_raw"] <- "total"
      fwrite(dt, paste0("diagnostico-", table_name, ".csv"), sep = ";")
    }
  }
  
  if(!is.null(referencia_subs$INICIO.ANO) && !is.null(referencia_subs$QNT.ANO)) {
    dt <- diag_subs_agr_athena(variaveis, database_name, table_name, dict, referencia_subs, "ANO")
      
    #' Ajustes de acordo com o relatório de integração
    relatorio <- paste0("totais-variavel-ano-", table_name, ".csv")
    if(file.exists(relatorio)) {
      totais <- read.csv2(relatorio, stringsAsFactors = F) %>% setDT
      totais$variavel <- NULL
      anos <- str_remove_all(colnames(totais), "^[:alpha:]")
      totais <- totais[, lapply(.SD, sum)] %>% transpose %>% 
          `colnames<-`("total_raw") %>% cbind(.,"ano"=ano)
        
      dt <- full_join(dt, totais, by = "ano")
      if(any(dt$nao.nulo > dt$total_raw))
        dt$total_raw <- NULL
      else {
        dt$total <- NULL
        colnames(dt)[colnames(dt) == "total_raw"] <- "total"
        fwrite(dt, paste0("diagnostico-ano-", table_name, ".csv"), sep = ";")
      }
    }
  }
    
  if(!is.null(referencia_subs$INICIO.UF) && !is.null(referencia_subs$QNT.UF)) {
    dt <- diag_subs_agr_athena(variaveis, database_name, table_name, dict, referencia_subs, "UF")
      
    #' Ajustes de acordo com o relatório de integração
    relatorio <- paste0("totais-variavel-uf-", table_name, ".csv")
    if(file.exists(relatorio)) {
      totais <- read.csv2(relatorio, stringsAsFactors = F) %>% setDT
      totais$variavel <- NULL
      uf <- colnames(totais) %>% as.character
      totais <- totais[, lapply(.SD, sum)] %>% transpose %>% 
        `colnames<-`("total_raw") %>% cbind(.,"uf"=uf)
        
      dt <- full_join(dt, totais, by = "uf")
      if(any(dt$nao.nulo > dt$total_raw))
        dt$total_raw <- NULL
      else {
        dt$total <- NULL
        colnames(dt)[colnames(dt) == "total_raw"] <- "total"
        fwrite(dt, paste0("diagnostico-uf-", table_name, ".csv"), sep = ";")
      }
    }
  }

  #' Temporalidade
  if(exists("v_temp")) {
    print("Temporalidade")
    temporalidade_sql(v_temp, dict, database_name, table_name)
    
    if(!is.null(referencia_subs$INICIO.ANO) && !is.null(referencia_subs$QNT.ANO))
      temporalidade_subs_sql(v_temp, dict, referencia_subs, "ANO", database_name, table_name)
    
    if(!is.null(referencia_subs$INICIO.UF) && !is.null(referencia_subs$QNT.UF))
      temporalidade_subs_sql(v_temp, dict, referencia_subs, "UF", database_name, table_name)
  }
  
  #' Unicidade
  if(exists("variavel_id") && exists("variaveis_comp")) {
    print("Unicidade")
    unicidade_sql(variavel_id, variaveis_comp, dict, database_name, table_name)
    freq_unicidade_sql(variavel_id, dict, database_name, table_name)
    
    if(!is.null(referencia_subs$INICIO.ANO) && !is.null(referencia_subs$QNT.ANO))
      unicidade_subs_sql(variavel_id, variaveis_comp, dict, 
                         referencia_subs, "ANO", database_name, table_name)
  }
  
  #' Informações gerais
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
  
  t <- paste0("../", table_name, ".info") %>% fread %>% as.numeric
  dt <- rbind(dt, data.table("X" = c("Número máximo de variáveis","Número de registros"),
                            "Y" = c(length(variaveis), t)))

  fwrite(dt, paste0("informacoes-gerais-", table_name, ".csv"), sep = ";")
  
  ##############################################################################

  setwd(ws)
}
