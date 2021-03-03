options(warn = -1)

if(!exists("qualidade_path"))
  stop("ERRO! qualidade_path")

source(paste0(qualidade_path, "R/dominio.R"))
source(paste0(qualidade_path, "R/string-matching.R"))

################################################################################

if(!exists("database_name") || !exists("table_name"))
  stop("ERRO! database_name | table_name")

path_to_data <- paste0(qualidade_path, "bases/scripts/", table_name, "/")
if(!dir.exists(path_to_data))
  dir.create(path_to_data)

dir_atual <- getwd()
setwd(path_to_data)

variaveis <- athena_2dataframe(database_name,
  paste0("select * from ", database_name, ".", table_name, " limit 1")) %>% colnames
print("Levantamento de domínio")
dominio(variaveis, database_name, table_name)
print("Registros ignorados")
registros_ignorados(variaveis, database_name, table_name)

setwd(dir_atual)