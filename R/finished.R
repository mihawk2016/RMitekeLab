library(compiler)
compilePKGS(T)

#### PATCH NOTE ####
## 2017-02-12: Version 0.2 environment
## 2017-02-05: Version 0.1






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

fetch.html.data.infos.mt4m_closed <- function(mq.file.parse) {
  set.infos.type('MT4M-Closed')
} # FINISH

fetch.html.data.infos.mt4m_raw <- function(mq.file.parse) {
  set.infos.type('MT4M-Raw')
} # FINISH

