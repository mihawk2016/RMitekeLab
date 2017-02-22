library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1

#### html.parse ENVIRONMENT ####
init.html.parse <- function() {
  assign('html.parse', list(), envir = METAQUOTE.ANALYSTIC)
}

get.html.parse <- function(index) {
  html.parse <- get('html.parse', envir = METAQUOTE.ANALYSTIC)
  if (!missing(index)) {
    html.parse <- html.parse[index]
  }
  html.parse
}

append.to.html.parse <- function(html.parse) {
  METAQUOTE.ANALYSTIC$html.parse %<>%
    c(list(html.parse))
}
