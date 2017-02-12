library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1

#### TICKETS COLUMNS ####
TICKETS.COLUMNS <- list(
  UNIFORM = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
              'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'GROUP', 'COMMENT', 'EXIT'),
  MONEY = c('TICKET', 'OTIME', 'PROFIT'),
  CLOSED = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
             'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'EXIT'),
  OPEN = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
           'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
  PENDING = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
              'CTIME', 'CPRICE'),
  WORKING = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE')
)


#### TICKETS ENVIRONMENT ####
init.tickets.raw <- function() {
  assign('TICKETS.RAW', list(), envir = METAQUOTE.ANALYSTIC)
}

init.tickets.temp <- function() {
  assign('TICKETS.TEMP', list(), envir = METAQUOTE.ANALYSTIC)
}

get.tickets.temp <- function() {
  get('TICKETS.TEMP', envir = METAQUOTE.ANALYSTIC)
}

get.tickets.raw <- function(index) {
  tickets.raw <- get('TICKETS,RAW', envir = METAQUOTE.ANALYSTIC)
  if (!missing(index)) {
    tickets.raw <- tickets.raw[index]
  }
  tickets.raw
}

append.to.tickets.temp <- function(tickets) {
  METAQUOTE.ANALYSTIC$TICKETS.TEMP %<>%
    c(list(tickets))
}

append.to.raw.tickets <- function() {
  tickets <-
    get.tickets.temp() %>%
    rbindlist
  init.tickets.temp()
  METAQUOTE.ANALYSTIC$TICKETS.RAW %<>%
    c(list(tickets))
}

#### BUILD TICKETS ####
build.tickets = function(table, group, columns=TICKETS.COLUMNS[[group]], uniform.columns=TICKETS.COLUMNS$UNIFORM) {
  # ''' build tickets '''
  # 2017-01-21: Version 1.1 add 'EXIT' in Closed & Uniform
  # 2017-01-21: Version 1.0 change logic expr to || short way connection
  # 2017-01-17: Version 0.2 add Comment
  # 2017-01-17: Version 0.1
  if (is.null(table) || !nrow(table)) {
    return(NULL) 
  }
  table.columns <- colnames(table)
  table[, GROUP := group]
  ## default 0 check
  zero.columns <- columns[!(columns %in% table.columns)]
  if (length(zero.columns)) {
    table[, (zero.columns) := list(0)]
  }
  ## comment
  if (!'COMMENT' %in% table.columns) {
    table[, COMMENT := '']
  } else if (group == 'CLOSED') {
    table[, EXIT := comment.to.exit(COMMENT)]
  }
  group.columns <- c(columns, c('GROUP', 'COMMENT'))
  table <- table[, (group.columns), with = FALSE]
  ## NAs check
  na.columns <- uniform.columns[!uniform.columns %in% group.columns]
  if (length(na.columns)) {
    table[, (na.columns) := list(NA)]
  }
  append.to.tickets.temp(table)
} # FINISH