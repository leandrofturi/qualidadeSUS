library(data.table)
library(stringr)
library(dplyr)

source("R/analise-agrupada.R")
source("R/unicidade-agrupada.R")


diagnostico_filtro_agr_multi <- function(variaveis_list, parametros_database_list, filtro_list, out) {
  nms <- names(variaveis_list)
  if(nms != names(parametros_database_list) || nms != names(filtro_list))
    stop("ERRO! Problema nos parametros de entrada!")

  dt <- lapply(nms, function(z) {
    print(z)
    dt <- diagnostico_filtro_agr(variaveis_list[[z]], parametros_database_list[[z]], 
                                 filtro_list[[z]], out=F)
    if(!is.null(dt))
      dt$variavel <- paste0(z, ".", dt$variavel)
    return(dt)
    }) %>% .[!sapply(., is.null)]
  
  if(length(dt) == 0) return(NULL)
  
  dt <- rbindlist(dt)
  
  if(out) fwrite(dt, paste0("diagnostico-multibase", filtro$VARIAVEL, ".csv"), sep = ";")
  return(dt)
}


unicidade_filtro_agr_multi <- function(variavel_id_list, variaveis_comp_list, 
                                       parametros_database_list, filtro_list, out) {
  nms <- names(variavel_id_list)
  if(nms != names(variaveis_comp_list) || nms != names(parametros_database_list) || 
     nms != names(filtro_list))
    stop("ERRO! Problema nos parametros de entrada!")
  
  dt <- lapply(nms, function(z) {
    print(z)
    dt <- unicidade_filtro_agr(variavel_id_list[[z]], variaveis_comp_list[[z]], 
                               filtro_list[[z]], parametros_database_list[[z]], out=F)
    if(!is.null(dt))
      dt$variavel <- paste0(z, ".", dt$variavel)
    return(dt)
  }) %>% .[!sapply(., is.null)]
  
  if(length(dt) == 0) return(NULL)
  dt <- rbindlist(dt)
  
  if(out) fwrite(dt, paste0("unicidade-multibase-", filtro$VARIAVEL, ".csv"), sep = ";")
  return(dt)
}

