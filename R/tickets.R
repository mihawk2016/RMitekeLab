library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1

#### TICKETS COLUMNS ####
TICKETS.COLUMNS <- list(
  UNIFORM = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
              'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'COMMENT', 'GROUP', 'EXIT'),
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
  if (missing(index)) {
    index <- 1:length(get('TICKETS.RAW', envir = METAQUOTE.ANALYSTIC))
  }
  lapply(index, fetch.tickets.raw)
  
  # tickets.raw <- get('TICKETS.RAW', envir = METAQUOTE.ANALYSTIC)
  # if (!missing(index)) {
  #   tickets.raw <- tickets.raw[index]
  # }
  # tickets.raw
}

append.to.tickets.temp <- function(tickets) {
  METAQUOTE.ANALYSTIC$TICKETS.TEMP %<>%
    c(list(tickets))
}

append.to.tickets.raw <- function(index) {
  tickets <-
    get.tickets.temp() %>%
    rbindlist(use.names = TRUE) %>%
    extract(j = TICKETS.COLUMNS$UNIFORM, with = F)## ToDo: more to do, like 'format' & 'sort'
  init.tickets.temp()
  METAQUOTE.ANALYSTIC$TICKETS.RAW[[index]] <- tickets
}

fetch.tickets.raw <- function(index) {
  tickets.raw <- get('TICKETS.RAW', envir = METAQUOTE.ANALYSTIC)[[index]]
  if (!is.null(tickets.raw)) {
    return(tickets.raw)
  }
  infos <- get.infos(index)[[1]]
  type <- infos$TYPE
  file <- infos$PATH
  parse <- get.html.parse(index)[[1]]
  switch(
    type,
    'MT4-EA' = NULL,
    'MT4-Trade' = fetch.html.data.tickets.mt4trade(file, parse)
  )
  append.to.tickets.raw(index)
}
#### FETCH TICKETS ####

fetch.html.data.tickets.mt4trade <- function(mq.file, mq.file.parse) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1) %>%
    as.data.table
  ticket.index <-
    table[j = V1] %>%
    str_detect('[:digit:]') %>%
    which
  if (!length(ticket.index)) {
    return(NULL)
  }
  table %<>%
    extract(ticket.index) %>%
    extract(j = COMMENT := xml_find_first(mq.file.parse, './/table') %>%
              xml_find_all('.//tr') %>%
              xml_find_first('.//td') %>%
              xml_attr('title', default = '') %>%
              extract(-1) %>%
              extract(ticket.index)) %>%
    set_colnames(TICKETS.COLUMNS$UNIFORM[1:15]) %>%
    setkey(CTIME) %>%
    extract('', CTIME := NA_character_) %>%
    extract(j = NAs := rowSums(is.na(.))) %>%
    setkey(NAs)
  money.tickets <-
    table[NAs == 9] %>%
    extract(j = PROFIT := ITEM) %>%
    extract(j = .(TICKET, OTIME, PROFIT, COMMENT)) %>%
    build.tickets('MONEY')
  closed.tickets <-
    table[NAs == 0] %>%
    build.tickets('CLOSED')
  open.tickets <-
    table[NAs == 1] %>%
    build.tickets('OPEN')
  pending.tickets <-
    table[NAs == 3] %>%
    build.tickets('PENDING')
  working.tickets <-
    table[NAs == 4] %>%
    build.tickets('WORKING')
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

#### CALCULATION ####
symbol.base.currency <- function(symbol) {
  # ''' symbol's base currency '''
  # 2017-01-23: Version 1.0
  substr(symbol, 4, 6)
} # FINISH

symbol.quote.currency <- function(symbol) {
  # ''' symbol's quote currency '''
  # 2017-01-23: Version 1.0
  substr(symbol, 1, 3)
} # FINISH

build.symbol <- function(currency1, currency2, support.symbols=SUPPORT.SYMBOLS) {
  # ''' build symbol from 2 currencies '''
  # 2016-08-12: Version 1.0 FIX support.symbols: self$get.support.symbols() should usd all symbol table
  
  match.currency1.symbols <-
    support.symbols %>%
    extract(support.symbols %>% str_detect(currency1))
  match.currency1.symbols %>%
    extract(match.currency1.symbols %>% str_detect(currency2)) %>% {
      if (length(.) == 1) {
        .
      } else {
        ''
      }
    }
} # FINISH

cal.tick.value = function(symbol, times, get.open.fun, timeframe='M1', currency=DEFAULT.CURRENCY,
                          symbols.setting=SYMBOLS.SETTING) {
  # ''' cal tick.value '''
  # 2017-01-23: Version 0.1
  base.currency <- symbol.base.currency(symbol)
  tick.value.point <- symbols.setting[symbol, TICKVALUE.POINT]
  if (base.currency == currency) {
    return(tick.value.point)
  }
  target.symbol <- build.symbol(base.currency, currency, symbols.setting[j = SYMBOL])
  if (target.symbol == '') {
    return(1)
  }
  target.open <- get.open.fun(target.symbol, times, timeframe)
  tick.value.point %>% {
    if (base.currency == symbol.base.currency(target.symbol)) {
      divide_by(target.open)
    } else {
      multiply_by(target.open)
    }
  }
}

cal.margin.required = function(symbol, times, get.open.fun, timeframe='M1', currency=DEFAULT.CURRENCY, 
                               leverage=DEFAULT.LEVERAGE, symbols.setting=SYMBOLS.SETTING) {
  # ''' cal margin required '''
  # 2017-01-23: Version 0.1
  quote.currency <- symbol.quote.currency(symbol)
  margin.required.point <- symbols.setting[symbol, CONTRACT.SIZE] / leverage
  if (quote.currency == currency) {
    return(margin.required.point)
  }
  target.symbol <- build.symbol(quote.currency, currency, symbols.setting[j = SYMBOL])
  if (target.symbol == '') {
    return(1000)
  }
  target.open <- get.open.fun(target.symbol, times, timeframe)
  margin.required.point %>% {
    if (quote.currency == symbol.quote.currency(target.symbol)) {
      multiply_by(target.open)
    } else {
      divide_by(target.open)
    }
  }
}

cal.profits <- function(volume, tickvalue, pips) {
  # ''' calculate profit from: volume, tickvalue, pips '''
  # 2016-08-15: Version 1.0
  volume * tickvalue * pips
}

cal.pips <- function(type, open.price, close.price, digit) {
  # ''' calculate pips (V) '''
  # 2017-02-10: Version 1.1 ifelse mode
  # 2017-01-22: Version 1.0
  ifelse(grepl('buy', type, ignore.case = TRUE), close.price - open.price, open.price - close.price) %>%
    multiply_by(10 ^ digit)
} # FINISH

comment.to.exit <- function(comments) {
  # ''' comment to exit '''
  # 2017-02-09: Version 1.2 add ignore.case argument, need not to toupper()
  # 2017-01-17: Version 1.1 add support for comments type - data.frame
  # 2016-12-01: Version 1.0
  comments %<>% gsub('/| / ', '', .)
  exit <- rep(NA, length(comments))
  exit[grep('SO', comments, ignore.case = TRUE)] <- 'SO'
  exit[grep('SL', comments, ignore.case = TRUE)] <- 'SL'
  exit[grep('TP', comments, ignore.case = TRUE)] <- 'TP'
  exit
} # FINISH

item.to.symbol <- function(item, support.symbols=SUPPORT.SYMBOLS) {
  # ''' item to symbol '''
  
  if (is.na(item) || item == '' || grepl('^BX', item, ignore.case = TRUE)) {
    return('') 
  }
  support.symbols %>%
    extract(support.symbols %>% str_detect(item)) %>% {
      if (length(.) == 1) {
        .
      } else {
        ''
      }
    }
}

####