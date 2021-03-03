system("pip install sparse_dot_topn")

library(bdacodeR)
library(dplyr)
library(stringi)
library(data.table)
library(reticulate)
#use_condaenv("notebook", required=TRUE)


#' String matching
#' https://bergvca.github.io/2017/10/14/super-fast-string-matching.html
#' @param clean character. Registros para comparação
#' @param dirty character. Registros para comparação
#' @param name character. Nome para arquivo .RDS. NULL para não escrita
string_matching <- function(dirty, clean, name) {
  reticulate::use_condaenv("r-reticulate")
  reticulate::source_python("/home/jovyan/qualidade/py/string_matching.py")
  
  clean <- as.character(unlist(clean))
  dirty <- as.character(unlist(dirty))
  sm <- string_matching(dirty, clean)
  if(is.null(sm)) return(NULL)
  colnames(sm) <- (c("dirty", "clean", "similaridade"))
  setDT(sm)
  
  if(!is.null(name))
    saveRDS(sm, paste0("string-matching-", name, "-", ".Rds"))
  return(sm)
}
