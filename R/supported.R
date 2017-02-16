library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####


#### @PATCH NOTE@ ####
## 2017-02-16: Version 0.1

tickets.supported <- function(tickets.raw) {
  tickets.raw[j = SYMBOL := item.to.symbol(SYMBOL), by = ITEM] %>%
    extract(i = GROUP == 'MONEY' | !is.na(SYMBOL))
}

not.supported.items <- function(tickets.raw) {
  items <- unique(tickets.raw[, ITEM])
  mapping.symbols <- item.to.symbol(items)
  items[which(is.na(mapping.symbols))]
}

