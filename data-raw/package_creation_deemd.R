########################
## Criando um pacote ###
########################

#inicia o pacote
install.packages("devtools")
devtools::setup(rstudio = FALSE)

#preencher o DESCRIPTION

# cria pasta para dados brutos
devtools::use_data_raw()

#salve este arquivo em data-raw

# Ignora essa rotina em data-raw
devtools::use_build_ignore("data-raw")

# Ignora Rproj do Rstudio
devtools::use_build_ignore("deemd.Rproj")

#Escrevas as funcoes e salve em R

# documenta as funcoes
devtools::document()

# testa o pacote, provavelmente recebera um erro de 
# dependencia
devtools::check()

# coloca as dependencias no pacote
devtools::use_package("EMD")
devtools::use_package("Rlibeemd")

# teste o pacote novamente
devtools::check()

# corriga os possiveis erros ou adivertencias apontados 
## corrige os acentos
tools::showNonASCII(readLines("R/deemd.R"))


# teste o pacote novamente
devtools::check()

# dados de maneira externa no pacote
devtools::use_data_raw()

#Adding `data-raw` to `.Rbuildignore`
devtools::use_build_ignore("data-raw")
# salve os dados brutos nesta pasta juntamente com
# o codigo para obter os dados transformados


# cria um vignette que reproduz os resultados
devtools::use_vignette("nimcno-brasil")

## instala o pacote 
# vrifica por erros
devtools::document()
devtools::check()
# instala
devtools::install()


# apenas carrega o pacote
devtools::load_all()


# Teste do pacote

