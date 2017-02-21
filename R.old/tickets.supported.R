library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####


#### @PATCH NOTE@ ####
## 2017-02-16: Version 0.1

tickets.supported <- function(tickets.raw, support.symbols=SUPPORT.SYMBOLS) {
  tickets.raw[j = SYMBOL := item.to.symbol(ITEM[1], support.symbols), by = ITEM] %>%
    extract(i = GROUP == 'MONEY' | !is.na(SYMBOL))
} # FINISH

not.supported.items <- function(tickets.raw, support.symbols=SUPPORT.SYMBOLS) {
  items <- unique(tickets.raw[GROUP != 'MONEY', ITEM])
  mapping.symbols <- sapply(items, item.to.symbol, support.symbols)
  not.supported <- items[which(is.na(mapping.symbols))]
  if (length(not.supported)) {
    return(not.supported)
  }
  NULL
} # FINISH

