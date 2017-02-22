library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1

#### MISMATCH ENVIRONMENT ####
init.mismatch <- function() {
  assign('MISMATCH', c(), envir = METAQUOTE.ANALYSTIC)
}

get.mismatch <- function() {
  get('MISMATCH', envir = METAQUOTE.ANALYSTIC)
}

append.to.mismatch <- function(mismatch.file) {
  METAQUOTE.ANALYSTIC$MISMATCH %<>%
    c(mismatch.file)
}
