library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-12: Version 0.1

#### INFOS TABLE ####
INFOS.COLUMNS <- c('PATH', 'FILE', 'TYPE', 'ACCOUNT', 'NAME', 'BROKER', 'CURRENCY', 'LEVERAGE', 'TIME')
INFOS.TABLE <-
  data.table(
    matrix(ncol = length(INFOS.COLUMNS), dimnames = list(NULL, INFOS.COLUMNS))
  )

#### INFOS ENVIRONMENT ####
init.infos <- function() {
  assign('INFOS', list(), envir = METAQUOTE.ANALYSTIC)
}

get.infos <- function(index) {
  infos <- get('INFOS', envir = METAQUOTE.ANALYSTIC)
  if (!missing(index)) {
    infos <- infos[index]
  }
  infos
}

init.infos.temp <- function() {
  assign('INFOS.TEMP', copy(INFOS.TABLE), envir = METAQUOTE.ANALYSTIC)
}

append.to.infos <- function() {
  # with(METAQUOTE.ANALYSTIC, INFOS <- c(INFOS, list(INFOS.TEMP)))
  METAQUOTE.ANALYSTIC$INFOS <- c(METAQUOTE.ANALYSTIC$INFOS, list(METAQUOTE.ANALYSTIC$INFOS.TEMP))
}

#### FETCH INFOS ####
fetch.html.data.infos.mt4ea <- function(mq.file.parse) {
  
  head.lines <- xml_text(xml_find_all(mq.file.parse, '//b')[2:3])
  first.table <- xml_find_first(mq.file.parse, '//table')
  time.string <- xml_text(xml_find_all(first.table, '//td')[4])
  nchar.time.string <- nchar(time.string)
  set.infos.type('MT4-EA')
  set.infos.name(head.lines[1])
  set.infos.broker(head.lines[2])
  set.infos.time(substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
} # FINISH

fetch.html.data.infos.mt4trade <- function(mq.file.parse) {
  
  first.row <- xml_text(xml_find_all(xml_find_first(xml_find_first(mq.file.parse, '//table'), './/tr'), './/b'))
  set.infos.type('MT4-Trade')
  set.infos.account(first.row[grep('Account', first.row)])
  set.infos.name(first.row[grep('Name', first.row)])
  set.infos.broker(xml_text(xml_find_first(mq.file.parse, '//b')))
  set.infos.currency(first.row[grep('Currency', first.row)])
  set.infos.leverage(first.row[grep('Leverage', first.row)])
  set.infos.time(tail(first.row, 1))
} # FINISH

fetch.html.data.infos.mt5ea <- function(mq.file.parse) {
  
  table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/td'))
  time.string <- table.values[grep('Period:', table.values) + 1]
  nchar.time.string <- nchar(time.string)
  set.infos.type('MT5-EA')
  set.infos.name(table.values[grep('Expert:', table.values) + 1])
  set.infos.broker(table.values[grep('Broker:', table.values) + 1])
  set.infos.currency(table.values[grep('Currency:', table.values) + 1])
  set.infos.leverage(table.values[grep('Leverage:', table.values) + 1])
  set.infos.time(substr(time.string, nchar.time.string - 10, nchar.time.string - 1))
} # FINISH

fetch.html.data.infos.mt5trade <- function(mq.file.parse) {
  
  table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/th'))
  account.currency.leverage <- table.values[grep('Account:', table.values) + 1]
  set.infos.type('MT5-Trade')
  set.infos.account(account.currency.leverage)
  set.infos.name(table.values[grep('Name:', table.values) + 1])
  set.infos.broker(table.values[grep('Broker:', table.values) + 1])
  set.infos.currency(account.currency.leverage)
  set.infos.leverage(account.currency.leverage)
  set.infos.time(format.infos.time(table.values[grep('Date:', table.values) + 1]) - 8 * 3600)
} # FINISH

fetch.html.data.infos.mt4m_closed <- function() {
  set.infos.type('MT4M-Closed')
} # FINISH

fetch.html.data.infos.mt4m_raw <- function() {
  set.infos.type('MT4M-Raw')
} # FINISH

#### INFOS SETTER ####
set.infos.path <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = PATH := value]
}

set.infos.file <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = FILE := value]
  append.to.infos()
}

set.infos.type <- function(value) {
  init.infos.temp()
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = TYPE := value]
}

set.infos.account <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = ACCOUNT := format.infos.account(value)]
}

set.infos.name <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = NAME := format.infos.name(value)]
}

set.infos.broker <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = BROKER := format.infos.broker(value)]
}

set.infos.currency <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = CURRENCY := format.infos.currency(value)]
}

set.infos.leverage <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = LEVERAGE := format.infos.leverage(value)]
}

set.infos.time <- function(value) {
  METAQUOTE.ANALYSTIC$INFOS.TEMP[j = TIME := format.infos.time(value)]
}

#### FORMAT ####
format.infos.account <- function(account) {
  if (is.na(account) || is.numeric(account)) {
    return(account)
  }
  if (account == '') {
    return(NA)
  }
  if (is.character(account)) {
    account <- gsub('Account: ', '', account)
    match1 <- regexpr('[[:digit:]]*', account)
    if (match1) {
      account <- substr(account, match1, attr(match1, 'match.length') + match1 - 1)
    }
    return(as.numeric(account))
  }
  NA
}

format.infos.name <- function(name) {
  # ''' format report info: name '''
  # 2017-01-16: Version 0.1
  if (is.na(name)) {
    return(name)
  }
  name <- gsub('Name: ', '', name)
  ifelse(name == '', NA, name)
} # FINISH

format.infos.broker <- function(broker) {
  # ''' format report info: broker '''
  # 2017-01-16: Version 0.1
  if (is.na(broker)) {
    return(broker)
  }
  gsub(' .*', '', broker)
} # FINISH

format.infos.currency <- function(currency) {
  # ''' format report info: currency '''
  # 2017-01-16: Version 0.1
  if (is.na(currency)) {
    return(currency)
  }
  currency <- gsub('Currency: ', '', currency)
  match1 <- regexpr('[[:upper:]]+', currency)
  if (match1) {
    currency <- substr(currency, match1, attr(match1, 'match.length') + match1 - 1)
  }
  ifelse(currency == '', NA, currency)
} # FINISH

format.infos.leverage <- function(leverage) {
  # ''' format report info: leverage '''
  # 2017-01-16: Version 0.1
  if (is.na(leverage) | is.numeric(leverage)) {
    return(leverage)
  }
  if (leverage == '') {
    return(NA)
  }
  if (is.character(leverage)) {
    match1 <- regexpr('1:[[:digit:]]+', leverage)
    if (match1) {
      leverage <- substr(leverage, match1 + 2, attr(match1, 'match.length') + match1 - 1)
    }
    return(as.numeric(leverage))
  }
  NA
} # FINISH

format.infos.time <- function(time) {
  # ''' format report info: time '''
  # 2017-01-16: Version 0.1
  format.time.all.to.numeric(time)
} # FINISH

format.time.all.to.numeric <- function(time) {
  if (is.na(time) || is.numeric(time)) {
    return(time)
  }
  if (is.character(time)) {
    if (grepl(',', time)) {
      time <- format.mt4trade.infos.time(time)
    } else {
      time <- gsub('-', '.', time)
      format <- '%Y.%m.%d %H:%M:%S'
      sub_format <- substr(format, 1, nchar(time) - 2)
      time <- as.POSIXct(time, format = sub_format, tz = 'GMT')
    }
    return(as.numeric(time))
  }
  NA
} # FINISH

format.mt4trade.infos.time <- function(time) {
  # ''' format mt4trade info time '''
  # 2016-08-16: Done
  local_time <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'us')
  new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
  Sys.setlocale('LC_TIME', local_time)
  new_time
} # FINISH

format.time.numeric.to.posixct <- function(time) {
  as.POSIXct(time, origin = '1970-01-01', tz = 'GMT')
} # FINISH
