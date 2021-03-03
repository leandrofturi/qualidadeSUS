library(dplyr)
library(stringr)
library(readr)
library(data.table)


#' Leitura de arquivo .CNV
#' @param cnv_path character. Caminho para o arquivo .cnv
read_cnv <- function(cnv_path) {
  if(!file.exists(cnv_path)) 
    stop("ERRO! arquivo não encontrado.")
  
  enc <- guess_encoding(cnv_path)$encoding[1]
  r <- read_lines(cnv_path, n_max = 1)
  r <- str_extract_all(r, "[:digit:]+", simplify = T) %>% as.numeric
  #' r[1] numero de linhas
  #' r[2] tamanho dos variantes

  dt <- read.table(cnv_path, skip = 1, sep = "\n", nrows = r[1],
                  stringsAsFactors = F, fileEncoding = enc) %>%
    apply(1, function(x) {
      match <- str_split(x, "\\s{2,}", simplify = T)
      match[match != ""]
    }) %>% t %>% data.table(stringsAsFactors = F)
  
  val <- sapply(dt[, 3], function(x) str_split(x, ",|-", simplify = T)) %>% 
    unlist %>% as.character %>% .[. != ""]
  if(any(nchar(val) != r[2]) || nrow(dt) != r[1]) {
    dt <- read.fwf(cnv_path, skip = 1, widths = c(9, 51, 100), nrows = r[1],
                   stringsAsFactors = F, fileEncoding = enc, 
                   colClasses = rep("character", 3)) %>%
      sapply(trimws) %>% data.table(stringsAsFactors = F)
  }
  
  val <- sapply(dt[, 3], function(x) str_split(x, ",|-", simplify = T)) %>% 
    unlist %>% as.character %>% .[. != ""]
  if(any(nchar(val) != r[2]) || nrow(dt) != r[1]) {
    dt <- read.fwf(cnv_path, skip = 1, widths = c(11, 101, 100), nrows = r[1],
                   stringsAsFactors = F, fileEncoding = enc, 
                   colClasses = rep("character", 3))
      sapply(trimws) %>% data.table(stringsAsFactors = F)
  }
  
  val <- sapply(dt[, 3], function(x) str_split(x, ",|-", simplify = T)) %>% 
    unlist %>% as.character %>% .[. != ""]
  
  if(any(nchar(val) != r[2]) || nrow(dt) != r[1])
    stop("ERRO! problema em leitura de CNV.")
  
  colnames(dt) <- c("seq", "desc", "val")
  return(dt)
}
