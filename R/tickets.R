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

get.tickets.raw <- function(index, get.open.fun, timeframe='M1', symbols.setting=SYMBOLS.SETTING) {
  if (missing(index)) {
    index <- 1:length(get('TICKETS.RAW', envir = METAQUOTE.ANALYSTIC))
  }
  lapply(index, fetch.tickets.raw, get.open.fun, timeframe, symbols.setting)
}

append.to.tickets.temp <- function(tickets) {
  METAQUOTE.ANALYSTIC$TICKETS.TEMP %<>%
    c(list(tickets))
} # FINISH

append.to.tickets.raw <- function(index) {
  tickets <-
    get.tickets.temp() %>%
    rbindlist(use.names = TRUE) %>%
    extract(j = TICKETS.COLUMNS$UNIFORM, with = F)## ToDo: more to do, like 'format' & 'sort'
  init.tickets.temp()
  METAQUOTE.ANALYSTIC$TICKETS.RAW[[index]] <- tickets
} 

fetch.tickets.raw <- function(index, get.open.fun, timeframe='M1', symbols.setting=SYMBOLS.SETTING) {
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
    'MT4-EA' = fetch.html.data.tickets.mt4ea(file, parse, get.open.fun, timeframe, infos$CURRENCY, symbols.setting),
    'MT4-Trade' = fetch.html.data.tickets.mt4trade(file, parse),
    'MT5-EA' = fetch.html.data.tickets.mt5ea(file),
    'MT5-Trade' = fetch.html.data.tickets.mt5trade(file),
    'MT4M-Closed' = fetch.html.data.tickets.mt4m_closed(file),
    'MT4M-Raw' = fetch.html.data.tickets.mt4m_raw(file)
  )
  append.to.tickets.raw(index)
} # FINISH

#### FETCH TICKETS ####
fetch.html.data.tickets.mt4ea <- function(mq.file, mq.file.parse, get.open.fun, timeframe='M1',
                                          currency, symbols.setting=SYMBOLS.SETTING) {
  suppressWarnings({
    table <-
      readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'GBK', which = 2,
                    colClasses = c('character', time.char.to.num, 'character', num.char.to.num, num.char.to.num,
                                   num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, 'character'))
  })
  table <-
    as.data.table %>%
    set_colnames(c('deal', 'time', 'type', 'ticket', 'volume', 'price', 'sl', 'tp', 'profit', 'balance')) %>%
    extract(type != 'modify', -c('deal', 'balance'))
  xml.text <-
    mq.file.parse %>%
    xml_find_first('.//table') %>%
    xml_find_all('.//td') %>%
    xml_text
  deposit <-
    xml.text %>%
    extract(24) %>%
    as.numeric
  time.string <- xml.text[4]
  len.time.string <- nchar(time.string)
  deposit.time <-
    time.string %>%
    substr(len.time.string - 23, len.time.string - 14) %>%
    format.time.all.to.numeric
  end.time <-
    time.string %>%
    substr(len.time.string - 10, len.time.string - 1) %>%
    format.time.all.to.numeric
  money.tickets <-
    data.table(
      TICKET = 0,
      OTIME = deposit.time,
      PROFIT = deposit
    ) %>%
    build.tickets('MONEY')
  rows <- nrow(table)
  if (!rows) {
    return(money.tickets)
  }
  item <-
    xml.text %>%
    extract(2) %>%
    gsub(' ([ \\(\\)[:alpha:]])*', '', .)
  table.index <- 1:rows
  table.types <- table[, type]
  table.tickets <- table[, ticket]
  pending.close.part.index <- which(table.types == 'delete')
  if (length(pending.close.part.index) > 0) {
    pending.tickets.ticket <- table.tickets[pending.close.part.index]
    table.index %<>% setdiff(pending.close.part.index)
    pending.open.part.index <- table.index[table.tickets[table.index] %in% pending.tickets.ticket]
    table.index %<>% setdiff(pending.open.part.index)
    merge(table[pending.open.part.index], table[pending.close.part.index], by = 'ticket') %>%
      set_colnames(c('TICKET', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                     'CTIME', '', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')) %>%
      extract(j = (c('ITEM', 'COMMENT', 'TYPE')) := list(item, 'cancelled', toupper(TYPE))) %>%
      build.tickets('PENDING')
  }
  pending.of.closed.ticktets.index <- table.index[grepl('(buy|sell) (limit|stop)', table.types[table.index], ignore.case = TRUE)]
  if (length(pending.of.closed.ticktets.index) > 0) {
    table.index %<>% setdiff(pending.of.closed.ticktets.index)
  }
  if (length(table.index) > 0) {
    closed.tickets.open.part.index <- table.index[grepl('(buy|sell)', table.types[table.index], ignore.case = TRUE)]
    closed.tickets.close.part.index <- table.index %<>% setdiff(closed.tickets.open.part.index)
    closed.tickets <- merge(table[closed.tickets.open.part.index], table[closed.tickets.close.part.index], by = 'ticket')
    part.closed.index <- which(closed.tickets[, volume.x != volume.y])
    if (length(part.closed.index) > 0) {
      closed.tickets[part.closed.index, volume.x := volume.y]
      closed.tickets[part.closed.index + 1, time.x := NA]
      closed.tickets[, time.x := na.locf(time.x)]
    }
    closed.tickets %<>%
      set_colnames(c('TICKET', 'OTIME', 'TYPE', '', 'OPRICE', '', '', '',
                     'CTIME', 'COMMENT', 'VOLUME', 'CPRICE', 'SL', 'TP', 'PROFIT')) %>%
      extract(j = c('ITEM', 'TYPE') := list(item, toupper(TYPE))) %>%
      extract(CTIME >= end.time - 60 & COMMENT == 'close at stop', EXIT := paste(COMMENT, 'so', sep = ' / '))
    symbol <- item.to.symbol(item)
    if (!is.na(symbol)) {
      if (is.na(currency)) {
        currency <- DEFAULT.CURRENCY
      }
      closed.tickets[, c('SWAP', 'PROFIT') := {
        pips <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
        tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, timeframe, currency, symbols.setting)
        new.profit <- cal.profits(VOLUME, tickvalue, pips)
        list(PROFIT - new.profit, new.profit)
      }]
    }
    build.tickets(closed.tickets, 'CLOSED')
  }
} # FINISH

fetch.html.data.tickets.mt4trade <- function(mq.file, mq.file.parse) {
  suppressWarnings({
    table <-
      readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                    colClasses = c('numeric', time.char.to.num, toupper, num.char.to.num, toupper,
                                   num.char.to.num, num.char.to.num, num.char.to.num, time.char.to.num,
                                   num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num)) %>%
      as.data.table
  })
  ticket.index <-
    table[j = V1] %>%
    is.na %>%
    not %>%
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
    extract(j = NAs := rowSums(is.na(.))) %>%
    setkey(NAs)
  table[NAs == 10] %>%
    extract(j = PROFIT := num.char.to.num(ITEM)) %>%
    extract(j = .(TICKET, OTIME, PROFIT, COMMENT)) %>%
    build.tickets('MONEY')
  table[NAs == 0] %>%
    build.tickets('CLOSED')
  table[NAs == 1] %>%
    build.tickets('OPEN')
  table[NAs == 4] %>%
    build.tickets('PENDING')
  table[NAs == 5] %>%
    build.tickets('WORKING')
} # FINISH

fetch.html.data.tickets.mt5ea <- function(mq.file) {
  suppressWarnings({
    table <-
      readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 2,
                    colClasses = c('character', num.char.to.num, toupper, toupper, toupper, num.char.to.num,
                                   num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num,
                                   num.char.to.num, num.char.to.num, 'character')) %>%
      as.data.table(key = V1)
  })
    
    
  
}

fetch.html.data.tickets.mt5trade <- function(mq.file) {
  suppressWarnings({
    table <-
      readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
    
  })
  
  
}

fetch.html.data.tickets.mt4m_closed <- function(mq.file) {
  suppressWarnings({
    table <-
      readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                    colClasses = c('numeric', rep('character', 2), time.char.to.num, toupper, toupper,
                                   num.char.to.num, num.char.to.num, time.char.to.num, num.char.to.num, num.char.to.num,
                                   num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num, 'character'))
  })
  as.data.table(table) %>%
    extract(j = c('V2', 'V3', 'V13', 'V16', 'V17') := list(NULL, NULL, NULL, NULL, paste(V17, V2, sep = ' | '))) %>%
    extract(i = 2:(nrow(.) - 1)) %>%
    set_colnames(c('TICKET', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'OPRICE', 'CTIME',
                   'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'COMMENT')) %>%
    build.tickets('CLOSED')
} # FINISH

fetch.html.data.tickets.mt4m_raw <- function(mq.file) {
  suppressWarnings({
    table <-
      readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1,
                    colClasses = c('numeric', 'character', time.char.to.num, toupper, toupper, num.char.to.num,
                                   num.char.to.num, num.char.to.num, num.char.to.num, time.char.to.num, num.char.to.num,
                                   rep('character', 6), num.char.to.num, num.char.to.num, num.char.to.num, num.char.to.num,
                                   rep('character', 2)))
  })
  table %<>%
    as.data.table %>%
    extract(j = c('V2', 'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V22', 'V23') := 
              list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, paste(V23, V2, sep = ' | '))) %>%
    extract(i = 2:(nrow(.) - 7)) %>%
    set_colnames(c('TICKET', 'OTIME', 'TYPE', 'ITEM', 'VOLUME', 'SL', 'TP', 'OPRICE', 'CTIME',
                   'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'COMMENT')) %>%
    setkey(TYPE)
  table['BALANCE'] %>%
    build.tickets('MONEY')
  table[c('BUY LIMIT', 'BUY STOP', 'SELL LIMIT', 'SELL STOP')] %>%
    build.tickets('PENDING')
  table[c('BUY', 'SELL')] %>%
    build.tickets('CLOSED')
} # FINISH

#### FORMAT COLCLASSES ####
time.char.to.num <- function(time.char) {
  new.time <- rep(NA, length(time.char))
  len <- nchar(time.char)
  index.19 <- which(len == 19)
  index.16 <- union(which(len == 16), grep(',', time.char))
  index.10 <- which(len == 10)
  new.time[index.19] <- ymd_hms(time.char[index.19], tz = 'GMT')
  new.time[index.16] <- ymd_hm(time.char[index.16], tz = 'GMT')
  new.time[index.10] <- ymd(time.char[index.10], tz = 'GMT')
  as.numeric(new.time)
}

num.char.to.num <- function(num.char) {
  as.numeric(gsub(' ', '', num.char))
}

# #### UTILS ####
# bind.comment.and.login <- function(comment, login) {
#   ifelse(comment == '' || is.na(comment), login, paste(comment, login, sep = ' | '))
# }
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
    return(NA) 
  }
  support.symbols %>%
    extract(support.symbols %>% str_detect(item)) %>% {
      if (length(.) == 1) {
        .
      } else {
        NA
      }
    }
}

####