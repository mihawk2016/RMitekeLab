library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.2 environment
## 2017-02-05: Version 0.1

#### ENVIRONMENT ####
METAQUOTE.ANALYSTIC <- new.env()
assign('INFOS', list(), envir = METAQUOTE.ANALYSTIC)

