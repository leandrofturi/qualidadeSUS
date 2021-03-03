library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(reshape)
library(viridis)


grafico_comparacao_ponderada <- function(dt_ponderada) {
  if(is.null(dt_ponderada)) return(NULL)
  if(all(colnames(dt_ponderada) %in% c("metrica","val"))) return(NULL)
  
  dt_ponderada <- dt_ponderada[dt_ponderada$metrica != "base", ]
  dt_ponderada$metrica[dt_ponderada$metrica == "completude"] <- "Completude"
  dt_ponderada$metrica[dt_ponderada$metrica == "conformidade"] <- "Conformidade"
  dt_ponderada$metrica[dt_ponderada$metrica == "acuracia"] <- "Acurácia"
  dt_ponderada$metrica[dt_ponderada$metrica == "consistencia"] <- "Consistência"
  dt_ponderada$metrica[dt_ponderada$metrica == "unicidade"] <- "Unicidade"
  dt_ponderada$metrica <- factor(dt_ponderada$metrica, 
                                 levels = dt_ponderada$metrica)
  dt_ponderada$val <- as.numeric(dt_ponderada$val)
  
  g <- ggplot(dt_ponderada) + 
    geom_bar(stat = "identity", aes(y = val, x = metrica, fill = val)) + 
    ylim(c(0, 100)) + 
    coord_flip() +
    scale_fill_gradient(low = "firebrick", high = "forestgreen", 
                        breaks = c(0, 50, 100), 
                        labels = c("0%", "50%", "100%"),
                        limits = c(0, 100)) +
    labs(fill = "") +
    xlab("Dimensão") + ylab("Avaliação") +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())
  
  return(g)
}


grafico_comparacao_anual <- function(dt_anual) {
  if(is.null(dt_anual)) return(NULL)
  
  dt_anual_aux <- dt_anual
  dt_anual_aux[["total"]] <- dt_anual_aux[["nao.nulo"]] <- 
    dt_anual_aux[["nao.nulo_conformes"]] <- dt_anual_aux[["conformes"]] <- 
    dt_anual_aux[["conformes_acurados"]] <- dt_anual_aux[["acurados"]] <- NULL

  colnames(dt_anual_aux)[colnames(dt_anual_aux) == "completude"] <- "Completude"
  colnames(dt_anual_aux)[colnames(dt_anual_aux) == "conformidade"] <- "Conformidade"
  colnames(dt_anual_aux)[colnames(dt_anual_aux) == "acuracia"] <- "Acurácia"
  colnames(dt_anual_aux)[colnames(dt_anual_aux) == "consistencia"] <- "Consistência"
  colnames(dt_anual_aux)[colnames(dt_anual_aux) == "unicidade"] <- "Unicidade"
  dt_anual_aux <- reshape::melt(dt_anual_aux, id = "ano")
  
  g <- ggplot(dt_anual_aux) + 
    geom_line(aes(x = ano, y = value, colour = variable)) +
    ylim(c(0, 100)) + 
    scale_colour_viridis_d(name = "Dimensão") + 
    scale_x_continuous(labels = dt_anual$ano, 
                       breaks = dt_anual$ano) +
    xlab("Ano") + ylab("Avaliação [%]") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6))
  
  return(g)
}


grafico_estratificacao <- function(dt, nome_coluna, nome_dimensao, nome_eixo) {
  if(is.null(dt)) return(NULL)
  if(is.null(dt[[nome_coluna]])) return(NULL)
  
  if(is.null(dt[[nome_coluna]])) return(NULL)
  
  na <- sum(is.na(dt[[nome_coluna]]))
  dt <- dt[!is.na(dt[[nome_coluna]]), ]
  df <- data.frame("name" = c("Vazia", paste0("Totalmente in", nome_eixo), "Ruim", 
                              "Regular","Ótimo","Excelente", 
                              paste0("Totalmente ", nome_eixo)),
                   "freq" = c(na, sum(dt[[nome_coluna]] == 0),
                              sum(dt[[nome_coluna]] > 0 & dt[[nome_coluna]] < 50),
                              sum(dt[[nome_coluna]] >= 50 & dt[[nome_coluna]] < 75),
                              sum(dt[[nome_coluna]] >= 75 & dt[[nome_coluna]] < 90),
                              sum(dt[[nome_coluna]] >= 90 & dt[[nome_coluna]] < 100),
                              sum(dt[[nome_coluna]] == 100)))
  df$name <- factor(df$name, levels = df$name)
  g <- ggplot(data = df, aes(y = freq, x = name)) + 
    geom_bar(stat = "identity", fill = "#39928f", inherit.aes = T) + coord_flip() +
    ylab("Quantidade de variáveis") + xlab(nome_dimensao) +
    geom_text(aes(label = freq), hjust = -0.5, inherit.aes = T, size = 2.75) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank())
  
  return(g)
}


grafico_anual <- function(dt_anual, nome_coluna, nome_dimensao) {
  if(is.null(dt_anual)) return(NULL)
  if(is.null(dt_anual[[nome_coluna]])) return(NULL)
  
  colnames(dt_anual) <- make.unique(tolower(colnames(dt_anual))) %>% stringi::stri_trans_general("Latin-ASCII")
  nome_coluna <- tolower(nome_coluna) %>% stringi::stri_trans_general("Latin-ASCII")
  
  
  if(is.null(dt_anual[[nome_coluna]])) return(NULL)
  
  dt_anual$ano <- as.character(dt_anual$ano)
  dt_anual[[nome_coluna]] <- as.numeric(dt_anual[[nome_coluna]])
  g <- ggplot(data = dt_anual, aes_string(x = "ano", y = nome_coluna, group = 1)) +
    geom_line(stat = "identity", color = "#39928f") +
    scale_x_discrete(labels = dt_anual$ano, breaks = dt_anual$ano) + 
    xlab("Ano") + ylab(nome_dimensao) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6))
  
  if(str_detect(nome_dimensao, "%"))
    g <- g + ylim(c(0, 100))
  
  return(g)
}


grafico_estadual <- function(dt_estadual, nome_coluna, nome_dimensao) {
  if(is.null(dt_estadual)) return(NULL)
  if(is.null(dt_estadual[[nome_coluna]])) return(NULL)
  
  dt <- rbind(c("AC",12), c("AL",27), c("AP",16), c("AM",13), c("BA",29), 
              c("CE",23), c("DF",53), c("ES",32), c("GO",52), c("MA",21), 
              c("MT",51), c("MS",50), c("MG",31), c("PA",15), c("PB",25), 
              c("PR",41), c("PE",26), c("PI",22), c("RN",24), c("RS",43), 
              c("RJ",33), c("RO",11), c("RR",14), c("SC",42), c("SP",35), 
              c("SE",28), c("TO",17)) %>% data.table %>% `colnames<-`(c("uf","id"))

  mapaUF <- readRDS("R/data/mapaUF.Rds")
  
  dt <- full_join(dt, dt_estadual, by = "uf")
  dt[is.na(dt)] <- NaN
  
  g <- ggplot(dt) +
    geom_map(map = mapaUF, color = 'gray30', 
             aes_string(map_id = "id", fill = nome_coluna)) +
    geom_path(data = mapaUF, color = 'gray30', size = .1,
             aes(x = long, y = lat, group = group)) +
    theme_void() +
    coord_equal() +
    scale_fill_viridis() +
    labs(fill = nome_dimensao)
  
  return(g)
}


grafico_temporalidade_anual <- function(dt_temp_anual) {
  if(is.null(dt_temp_anual)) return(NULL)
  
  setDT(dt_temp_anual)
  dt_temp_anual <- dt_temp_anual[, lapply(.SD, as.numeric), by = ano]
  dt_temp_anual <- melt(dt_temp_anual, id = c("ano"))
  
  g <- ggplot(dt_temp_anual) + 
    geom_line(aes(x = ano, y = value, colour = variable)) +
    scale_colour_viridis_d(name = "Teste") + 
    scale_x_continuous(labels = dt_temp_anual$ano, breaks = dt_temp_anual$ano) +
    xlab("Ano") + ylab("Temporalidade (mediana)") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6))
  
  return(g)
}


grafico_NA_anual <- function(dt_NA) {
  if(is.null(dt_NA)) return(NULL)
  
  ano <- as.numeric(dt_NA$ano)
  dt_NA$ano <- NULL
  dt <- expand.grid(x = ano, y = tolower(colnames(dt_NA)))
  dt[["z"]] <- unlist(dt_NA)
  cols <- c("FALSE" = "#39928f", "TRUE" = "white")
  
  g <- ggplot(dt, aes(x, y, fill = z)) + 
    geom_tile(size = 0.01) + 
    scale_fill_manual(values = c(cols)) + 
    scale_x_continuous(labels = ano, breaks = ano) +
    xlab("Ano") + ylab("Variável") +
    theme(axis.text.y = element_text(hjust = 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6)) +
    guides(fill = FALSE)
    
  return(g)
}


grafico_quantidade <- function(relatorio) {
  if(is.null(relatorio)) return(NULL)
  
  if(!"ano" %in% colnames(relatorio))
    return(NULL)
  
  dt <- setDT(relatorio[, c("ano", "QNT_COLUNAS")])
  dt$ano <- as.numeric(dt$ano)
  dt <- dt[, lapply(.SD, max), by = "ano"]
  dt <- dt[order(dt$ano), ]
  g <- ggplot(data = dt, aes(x = ano, y = QNT_COLUNAS, group = 1)) +
    geom_line(stat = "identity", color = "#39928f") +
    scale_x_continuous(labels = dt$ano, breaks = dt$ano) + 
    xlab("Ano") + ylab("Quantidade de variáveis") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.6))
  
  return(g)
}
