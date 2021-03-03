diagnostico_sub_agr_filtro <- function(variaveis, parametros_database,
                                referencia_subs, sub, filtro ,out=T) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  if(is.null(referencia_subs[[s[1]]]) || is.null(referencia_subs[[s[2]]]))
    return(NULL)
  
  print(sub)
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
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
      data <- coluna_ref_agregada_filtro(x, parametros_database, 
                                  referencia_subs$VARIAVEL, referencia_subs[[s[1]]], y,
                                  filtro$VARIAVEL, 1, filtro$VAL)
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

coluna_ref_agregada_filtro <- function(variavel, parametros_database,
                                variavel_ref, inicio_ref, ref, variavel_reff, iniciof_ref , reff) {
  names(parametros_database) <- tolower(names(parametros_database))
  q <- paste0("substring(CAST(\"", variavel_ref, "\" AS varchar), ", inicio_ref,", ", nchar(ref), ") LIKE '", ref, "' AND substring(CAST(\"", variavel_reff, "\" AS varchar), ", inicio_reff,", ", nchar(reff), ") LIKE '", reff, "'", 
              collapse=", ")
  q <- paste0(
    "SELECT \"", variavel, "\" AS \"variavel\",",
    " COUNT(\"", variavel, "\") AS \"total\"",
    " FROM ", parametros_database$database_name, ".", parametros_database$table_name,
    " WHERE ", q, 
    " GROUP BY \"", variavel, "\"",
    " ORDER BY \"total\" DESC")
  data <- get_athena_data(parametros_database$database_name, q, show_time=T)
  if(is.null(data)) return(NULL)
  
  data[data == "NA" | data == "NULL" | data == "null"] <- NA
  data[, variavel := as.character(variavel)]
  data[, total := as.numeric(total)]
  return(data)
}
temporalidade_agr_filtro <- function(v_temp, parametros_database, filtro, out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  v_temp <- lapply(v_temp, toupper)
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  dt <- lapply(v_temp, function(z) {
    print(z)
    data <- coluna_ref_temp_agregada(z[1], z[2], parametros_database,filtro$VARIAVEL, 1, filtro$VAL )
    data <- data[data$day_diff >= 0, ]
    s <- summary_all(rep(data$day_diff, data$total))
    return(s)
  }) %>% do.call(rbind.data.frame, .) %>% 
    `colnames<-`(c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's"))
  dt[["V1"]] <- sapply(v_temp, function(x) x[1])
  dt[["V2"]] <- sapply(v_temp, function(x) x[2])
  dt[["teste"]] <- paste0("T", seq(v_temp))
  
  if(out) fwrite(dt, "temporalidade.csv", sep = ";", dec = ",")
  return(dt)
}
coluna_ref_temp_agregada_filtro <- function(x, y, parametros_database, 
                                     variavel_ref, inicio_ref, ref, variavel_reff, inicio_reff , reff) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  database_name <- parametros_database$database_name
  table_name <- parametros_database$table_name
  
  x <- toupper(x)
  y <- toupper(y)
  
  data <- get_athena_data(database_name,
                          paste0(
                            "SELECT date_diff('day', tmp.", x, ", tmp.", y, ") AS \"day_diff\",",
                            "       COUNT(date_diff('day', tmp.", x, ", tmp.", y, ")) AS \"total\"",
                            " FROM (", 
                            "           SELECT TRY(date_parse(CAST(\"", x, "\" AS VARCHAR), '", D$formatos[[x]], "')) AS \"", x, "\", ",
                            "                  TRY(date_parse(CAST(\"", y, "\" AS VARCHAR), '", D$formatos[[y]], "')) AS \"", y, "\"",
                            "           FROM ", database_name, ".", table_name,
                            "           WHERE substring(CAST(\"", variavel_ref, "\" AS varchar), ", inicio_ref,", ", nchar(ref), ") LIKE '", ref, "' AND substring(CAST(\"", variavel_reff, "\" AS varchar), ", inicio_reff,", ", nchar(reff), ") LIKE '", reff, "'",
                            ") tmp",
                            " WHERE (tmp.", x, " >= date_parse('", D$referencias[[x]][1], "','", D$formatos[[x]], "') AND ",
                            "        tmp.", x, " <= date_parse('", D$referencias[[x]][2], "','", D$formatos[[x]], "') AND ",
                            "        tmp.", y, " >= date_parse('", D$referencias[[y]][1], "','", D$formatos[[y]], "') AND ", 
                            "        tmp.", y, " <= date_parse('", D$referencias[[y]][2], "','", D$formatos[[y]], "'))", 
                            " GROUP BY date_diff('day', tmp.", x, ", tmp.", y, ")", 
                            " ORDER BY \"total\" DESC"
                          ))
  data <- data[, lapply(.SD, as.numeric)]
  if(!grepl("%d", D$formatos[[x]]) || !grepl("%d", D$formatos[[y]]))
    data <- data[data$day_diff >= 31 | data$day_diff <= -31, ]
  
  return(data)
}

temporalidade_sub_agr_filtro <- function(v_temp, parametros_database, referencia_subs, sub,filtro, out=T) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  if(is.null(referencia_subs[[s[1]]]) || is.null(referencia_subs[[s[2]]]))
    return(NULL)
  
  print(sub)
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  v_temp <- lapply(v_temp, toupper)
  if(!all(sapply(v_temp, function(x) all(x %in% toupper(dict$VARIAVEL)))))
    return(NULL)
  
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  subs <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)$variavel
  subs <- substr(subs, referencia_subs[[s[1]]], referencia_subs[[s[1]]]+referencia_subs[[s[2]]]-1) %>% 
    unique %>% sort
  
  dt <- lapply(subs, function(w) {
    sapply(v_temp, function(z) {
      print(paste(z, w))
      data <- coluna_ref_temp_agregada_filtro(z[1], z[2], parametros_database, 
                                       referencia_subs$VARIAVEL, referencia_subs[[s[1]]], w,
                                       filtro$VARIAVEL, 1, filtro$VAL)
      data <- data[data$day_diff >= 0, ]
      m <- median(rep(data$day_diff, data$total), na.rm = T)
      if(is.null(m)) m <- NA
      return(m)
    })
  }) %>% do.call(rbind.data.frame, .) %>% 
    `colnames<-`(paste0("T", seq(v_temp)))
  
  dt[[sub]] <- subs
  
  if(out) fwrite(dt, paste0("temporalidade-", sub, ".csv"), sep = ";", dec = ",")
  return(dt)
}

coluna_ref_unicidade_agregada_filtro <- function(variavel_id, variaveis_comp, parametros_database,
                                          variavel_ref, inicio_ref, ref, variavel_reff, inicio_reff , reff) {
  names(parametros_database) <- tolower(names(parametros_database))
  database_name <- parametros_database$database_name
  table_name <- parametros_database$table_name
  
  variavel_id <- toupper(variavel_id)
  x <- toupper(variaveis_comp)
  x <- paste0("CAST(", database_name, ".", table_name, ".", x, " AS VARCHAR)", collapse = ", '*', ")
  
  y <- paste0("substring(CAST(\"", variavel_ref, "\" AS varchar), ", inicio_ref,", ", nchar(ref), ") LIKE '", ref, "' AND substring(CAST(\"", variavel_reff, "\" AS varchar), ", inicio_reff,", ", nchar(reff), ") LIKE '", reff, "'", 
              collapse=", ")
  
  data <- get_athena_data(database_name,
                          paste0(
                            "SELECT CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR) , '*', ",  x, ") AS \"variavel\", ",
                            "       COUNT(CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR), '*', ", x, "))  AS \"total\"",
                            " FROM (",
                            "       SELECT \"", variavel_id, "\"",
                            "       FROM ", database_name, ".", table_name,
                            "       WHERE ", y,
                            "       GROUP BY \"", variavel_id, "\"",
                            "       HAVING COUNT(\"", variavel_id, "\") > 1) tmp",
                            " INNER JOIN ", database_name, ".", table_name, " ON tmp.", variavel_id, " = ", database_name, ".", table_name, ".", variavel_id,
                            " GROUP BY CONCAT(CAST(", database_name, ".", table_name, ".", variavel_id, " AS VARCHAR), '*', ", x, ")",
                            " ORDER BY \"total\" DESC"))
  
  nms <- make.unique(c(variavel_id, variaveis_comp), sep="_")
  data[, (nms) := tstrsplit(variavel, "*", fixed=T, fill=NA, keep=c(1, seq(length(variaveis_comp))+1))]
  data$variavel <- NULL
  data[data == "NA" | data == "NULL" | data == "null"] <- NA
  
  options(scipen=999)
  data <- data[, lapply(.SD, function(x) 
    if(all(is.finite(as.numeric(x))))
      as.character(as.numeric(x))
    else 
      as.character(x))]
  data[, total := as.numeric(total)]
  
  setcolorder(data, c(nms,"total"))
  return(data)
}
unicidade_agr_filtro <- function(variavel_id, variaveis_comp, 
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
  
  if(out) fwrite(dt, paste0("unicidade.csv"), sep = ";")
  return(dt)
}
unicidade_sub_agr_filtro <- function(variavel_id, variaveis_comp, 
                              referencia_subs, sub, parametros_database, filtro,out=T) {
  names(referencia_subs) <- toupper(names(referencia_subs))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  if(is.null(referencia_subs[[s[1]]]) || is.null(referencia_subs[[s[2]]]))
    return(NULL)
  
  print(sub)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
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
    data <- coluna_ref_unicidade_agregada_filtro(variavel_id, variaveis_comp, parametros_database,
                                          referencia_subs$VARIAVEL, referencia_subs[[s[1]]], w,
                                          filtro$VARIAVEL, 1, filtro$VAL)
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
freq_identificadores_filtro <- function(variavel_id, parametros_database,filtro, out=T) {
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  
  variavel_id <- toupper(variavel_id)
  data <- coluna_ref_agregada(variavel_id, parametros_database,
                              filtro$VARIAVEL, 1, filtro$VAL)
  
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

inconformes_inacurados_agr_filtro <- function(variaveis, parametros_database, filtro,out=T) {
  if(length(variaveis) == 0) return(NULL)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- sort(variaveis) %>% toupper
  
  dt <- lapply(variaveis, function(x) {
    print(x)
    data <- coluna_ref_agregada(x, parametros_database,
                                filtro$VARIAVEL, 1, filtro$VAL) 
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

outliers_agr_filtro <- function(parametros_database,filtro,out=T) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- toupper(D$numericas)
  variaveis <- variaveis[!is.na(variaveis)]
  if(length(variaveis) == 0) return(NULL)
  
  df <- suppressWarnings(lapply(variaveis, function(x) {
    print(x)
    data <- coluna_ref_agregada(x, parametros_database,
                                filtro$VARIAVEL, 1, filtro$VAL)  
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
ignorados_agr_filtro <- function(variaveis, parametros_database,filtro, out=T) {
  if(length(variaveis) == 0) return(NULL)
  
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  variaveis <- sort(variaveis) %>% toupper
  
  clean <- c("IGNORADO", "FALTANTE", "INDEFINIDO", "DESCONHECIDO", 
             "DESCONSIDERADO", "REJEITADO", "SEM INFORMACAO", 
             "NAO INFORMADO", "SEM DEFINICAO")
  
  dt <- lapply(variaveis, function(x) {
    print(x)
    data <- coluna_ref_agregada(x, parametros_database,
                                filtro$VARIAVEL, 1, filtro$VAL) 
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

tabela_dominio_filtro <- function(variaveis, referencia_subs, database_name, table_name,filtro) {
  if(length(variaveis) == 0) return(NULL)
  names(referencia_subs) <- toupper(names(referencia_subs))
  referencia_subs$VARIAVEL <- as.character(referencia_subs$VARIAVEL)
  
  variaveis <- sort(variaveis)
  y <- paste0("substring(CAST(\"", filtro$VARIAVEL, "\" AS varchar), ", 1,", ", nchar(filtro$VAL), ") LIKE '", filtro$VAL, "'", 
              collapse=", ")
  dts <- lapply(variaveis, function(x) {
    print(x)
    q <- paste0(
      "SELECT CASE WHEN COUNT(DISTINCT \"", x, "\") <= 30 THEN array_join(array_sort(array_agg(DISTINCT CAST(\"", x, "\" AS varchar))), ', ', 'NA') END AS \"dominio\", ",
      " COUNT(DISTINCT \"", x, "\") AS \"distinct\", ",
      " MIN(LENGTH(CAST(\"", x, "\" AS varchar))) AS \"nchar_min\", ",
      " MAX(LENGTH(CAST(\"", x, "\" AS varchar))) AS \"nchar_max\", ",
      " (COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) LIKE 'NA') THEN 1 END) + ",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) LIKE 'NULL') THEN 1 END) +",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) LIKE 'null') THEN 1 END) +",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) IS NULL) THEN 1 END)) = ",
      " (COUNT(\"", x, "\") + ",
      " COUNT(CASE WHEN (CAST(\"", x, "\" AS VARCHAR) IS NULL) THEN 1 END)) AS \"null\", ",
      " substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ") AS \"ano\"",
      " FROM ", database_name, ".", table_name,
      " WHERE ", y,
      " GROUP BY substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ")",
      " ORDER BY substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ")")
    
    dt <- get_athena_data(database_name, query = q)
    dt <- dt[, lapply(.SD, as.character)]
    return(split(dt, as.character(dt$ano)))
  }) %>% `names<-`(variaveis)
  
  ano <- get_athena_data(database_name, 
                         paste0(
                           "SELECT substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ") AS \"ano\"",
                           " FROM ", database_name, ".", table_name,
                           " GROUP BY substring(CAST(\"", referencia_subs$VARIAVEL, "\" AS varchar), ", referencia_subs$INICIO.ANO,", ", referencia_subs$QNT.ANO, ")")) %>% 
    .[["ano"]] %>% sort
  
  dt <- lapply(variaveis, function(x)
    lapply(ano, function(y) c("variavel"=x, dts[[x]][[y]])) %>% rbindlist) %>% rbindlist
  
  dict <- leitura_dict(parametros_database)
  dict$VARIAVEL <- toupper(dict$VARIAVEL)
  
  # TRUE em apenas as variáveis que ocorreram alterações no domínio
  dt[["mud_dom"]] <- lapply(variaveis, function(x) {
    n <- length(dt$dominio[dt$variavel == x])
    if(is.null(dict$VARIANTES))
      return(rep(FALSE, n))
    else if(is.na(dict$VARIANTES[dict$VARIAVEL == x]) || dict$VARIANTES[dict$VARIAVEL == x] == "")
      return(rep(FALSE, n))
    
    s <- str_split(dt$dominio[dt$variavel == x], ", ")
    i <- Reduce(intersect, s[!sapply(s, function(x) any(is.na(x)))])
    
    if(length(i) <= 0) i <- NULL
    s <- sapply(s, function(x) !all(x %in% i))
    s[1] <- TRUE
    return(s)
  }) %>% unlist
  
  # TRUE em apenas as variáveis que ocorreram alterações no tamanho
  dt[["mud_nchar"]] <- lapply(variaveis, function(x) {
    n <- length(dt$dominio[dt$variavel == x])
    if(is.null(dict$VARIANTES))
      return(rep(FALSE, n))
    else if(is.na(dict$VARIANTES[dict$VARIAVEL == x]) || dict$VARIANTES[dict$VARIAVEL == x] == "")
      return(rep(FALSE, n))
    
    s <- c(0, dt$nchar_max[dict$VARIAVEL == x][-length(dt$nchar_max[dict$VARIAVEL == x])]) %>% as.numeric
    s <- (as.numeric(dt$nchar_max[dict$VARIAVEL == x]) - s) > 0
    return(s)
  }) %>% unlist
  
  fwrite(dt, "dominio-unique-athena.csv", sep = ";")
}
consistencia_data_subs_filtro <- function(V1, V2, referencia_subs, sub, 
                                   parametros_database,filtro) {
  dict <- leitura_dict(parametros_database)
  D <- dicionario_formatado(dict)
  
  names(filtro) <- toupper(names(filtro))
  filtro$VARIAVEL <- as.character(filtro$VARIAVEL)
  
  names(referencia_subs) <- toupper(names(referencia_subs))
  referencia_subs$VARIAVEL <- toupper(as.character(referencia_subs$VARIAVEL))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  subs <- coluna_agregada(referencia_subs$VARIAVEL, parametros_database)$variavel
  subs <- substr(subs, referencia_subs[[s[1]]], referencia_subs[[s[1]]]+referencia_subs[[s[2]]]-1) %>% 
    unique %>% sort
  
  dt <- lapply(subs, function(w) {
    print(paste(toupper(V1), toupper(V2), w))
    data <- coluna_ref_temp_agregada_filtro(toupper(V1), toupper(V2), parametros_database, 
                                     referencia_subs$VARIAVEL, referencia_subs[[s[1]]], w,
                                     filtro$VARIAVEL,1,filtro$VAL)
    if(is.null(data) || nrow(data) == 0)
      return(c(NA, NA))
    
    c <- sum(data$total[data$day_diff < 0])
    t <- sum(data$total)
    return(c(t, c))
  }) %>% do.call(rbind.data.frame, .) %>% `colnames<-`(c("total", "falhas"))
  dt[[sub]] <- subs
  setcolorder(dt, c(sub, "total","falhas"))
  
  return(dt)
}
consistencia_query_subs_filtro <- function(variaveis, query, referencia_subs, sub, 
                                    parametros_database,filtro) {
  names(parametros_database) <- tolower(names(parametros_database))
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  names(referencia_subs) <- toupper(names(referencia_subs))
  variaveis_query <- str_extract_all(query, "\"[:graph:]+\"", simplify = T) %>% 
    str_replace_all(., "\"|\"", "")
  
  y <- paste0("substring(CAST(\"", filtro$VARIAVEL, "\" AS varchar), ", 1,", ", nchar(filtro$VAL), ") LIKE '", filtro$VAL, "'", 
              collapse=", ")
  TT <- total_nao_nulos_filtro(variaveis_query, referencia_subs, sub, parametros_database,filtro)
  
  tt <- get_athena_data(database_name,
                        paste0(
                          "SELECT COUNT(CASE WHEN (TRY(", query, ")) THEN 1 END) AS \"falhas\",",
                          "       substring(\"", referencia_subs$VARIAVEL, "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ") AS \"", sub, "\"",
                          " FROM ", parametros_database$database_name, ".", parametros_database$table_name,
                          " WHERE substring(CAST(\"", filtro$VARIAVEL, "\" AS varchar), ", 1,", ", nchar(filtro$VAL), ") LIKE '", filtro$VAL, "'", 
                          " GROUP BY substring(\"", referencia_subs$VARIAVEL, "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")",
                          " ORDER BY substring(\"", referencia_subs$VARIAVEL, "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")"
                        ))
  dt <- full_join(tt, TT, by = sub)
  setcolorder(dt, c(sub, "total","falhas"))
  
  return(dt)
}


#' Total de registros em que são ambos não nulos em variaveis
#' Usando para consistências
#' @param variaveis character[].
#' @param referencia_subs data.frame. Tabela contendo parâmetros para obtenção 
#' do ano/mês/estado dos registros.
#' referencia_subs <- data.frame("variavel" = "NO_ARQ_REMESSA", 
#'                               "inicio.ano" = 12, "qnt.ano" = 4,
#'                               "inicio.mes" = 16, "qnt.mes" = 2)
#' @param sub character. "ANO || UF"
#' @param parametros_database list. Parâmetros para leitura dos dados
total_nao_nulos_filtro <- function(variaveis, referencia_subs, sub, parametros_database,filtro) {
  q_comp <- paste0(
    " (NOT (CAST(\"", variaveis, "\" AS VARCHAR) LIKE 'NA')) AND",
    " (NOT (CAST(\"", variaveis, "\" AS VARCHAR) LIKE 'NULL')) AND",
    " (NOT (CAST(\"", variaveis, "\" AS VARCHAR) LIKE 'null')) AND",
    " (NOT \"", variaveis, "\" IS NULL) ", collapse = " AND ")
  
  sub <- tolower(sub)
  s <- toupper(paste0(c("INICIO.", "QNT."), sub))
  names(referencia_subs) <- toupper(names(referencia_subs))
  t <- get_athena_data(database_name,
                       paste0(
                         "SELECT COUNT(CASE WHEN", q_comp, "THEN 1 END) AS \"total\",", 
                         "       substring(\"", as.character(referencia_subs$VARIAVEL), "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ") AS \"", sub, "\"",
                         " FROM ", database_name, ".", table_name,
                         " WHERE substring(CAST(\"", filtro$VARIAVEL, "\" AS varchar), ", 1,", ", nchar(filtro$VAL), ") LIKE '", filtro$VAL, "'", 
                         " GROUP BY substring(\"", as.character(referencia_subs$VARIAVEL), "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")",
                         " ORDER BY substring(\"", as.character(referencia_subs$VARIAVEL), "\", ", referencia_subs[[s[1]]],", ", referencia_subs[[s[2]]], ")"
                       ))
  t[["total"]] <- as.numeric(t[["total"]])
  t[[sub]] <- as.character(t[[sub]])
  return(t)
}
