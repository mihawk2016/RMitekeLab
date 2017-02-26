library(compiler)
compilePKGS(T)








#### TICKETS ####
output.tickets <- function(tickets, groups, columns, file.name) {
  tickets %>%
    setkey(GROUP) %>%
    extract(
      j = c('OTIME', 'CTIME') := list(time.numeric.to.posixct(OTIME), time.numeric.to.posixct(CTIME))
    ) %>%
    extract(
      i = groups,
      j = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
            'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', columns)
    ) %>%
    write.csv(file = file.name)
} #


#### UTILS ####
output.file.name <- function(infos, type=c('REPORT', 'TICKETS')) {
  if (nrow(infos) == 1) {
    infos[, FILE]
  } else {
    'FileSet'
  } %>%
    sprintf('%s-%s', substr(type, 1, 1), .)
}

