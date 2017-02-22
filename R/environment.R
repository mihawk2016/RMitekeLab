library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.2 environment
## 2017-02-05: Version 0.1

#### ENVIRONMENT ####
METAQUOTE.ANALYSTIC <- new.env(parent = baseenv())
# assign('INFOS', list(), envir = METAQUOTE.ANALYSTIC)
# assign('html.parse', list(), envir = METAQUOTE.ANALYSTIC)
# assign('MISMATCH', c(), envir = METAQUOTE.ANALYSTIC)
# assign('TICKETS.RAW', list(), envir = METAQUOTE.ANALYSTIC)
assign('TICKETS.TEMP', list(), envir = METAQUOTE.ANALYSTIC)

# assign('LOCAL.DATA', NULL, envir = METAQUOTE.ANALYSTIC)