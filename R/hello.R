## 2017-02-05: Version 0.1

library(compiler)
compilePKGS(T)

read.mq.file <- function(mq.files, cluster) {
  # ''' read mq files (V) '''
  # @param mq.files: MetaQuote files.
  # @return:
  # 2017-02-12: Version 0.3 fix file path for input mode
  # 2017-02-07: Version 0.2 parallel
  # 2017-02-05: Version 0.1
  if (is.data.frame(mq.files)) {
    mq.files <- mq.files$datapath
    mq.names <- mq.files$name
  } else {
    mq.names <- basename(mq.files)
  }
  if (missing(cluster) || length(mq.files) < 4) {
    mapply(fetch.file.data, mq.files, mq.names, SIMPLIFY = FALSE)
  } else (
    clusterMap(cluster, fetch.file.data, mq.files, mq.names, SIMPLIFY = FALSE)
  )
  METAQUOTE.ANALYSTIC$TICKETS.RAW %<>%
    c(vector('list', length(get.infos()) - length(get('TICKETS.RAW', envir = METAQUOTE.ANALYSTIC))))
}

fetch.file.data <- function(mq.file, mq.file.name) {
  # ''' fetch mq file's data (S) '''
  # @param mq.files: MetaQuote file.
  # @param mq.names: MetaQuote file-name.
  # @return: data of MetaQuote file.
  # 2017-02-05: Version 0.1
  data <-
    if (grepl('.(html|htm)$', mq.file.name)) {
      fetch.html.data(mq.file)
    } else if (grepl('.(xlsx|xls)$', mq.file.name)) {
      ## ToDo ####
      fetch.excel.data(mq.file, mq.file.name)
    } else if (grepl('.(csv)$', mq.file.name)) {
      ## ToDo ####
      fetch.csv.data(mq.file, mq.file.name)
    } else {
      NULL
    }
  if (is.null(data)) {
    append.to.mismatch(mq.file.name)
  } else {
    set.infos.path(mq.file)
    set.infos.file(mq.file.name)
  }
}


fetch.html.data <- function(mq.file) {
  # ''' fetch mq html's data (S) '''
  # @param mq.files: MetaQuote file.
  # @param mq.names: MetaQuote file-name.
  # @return: data of MetaQuote file.
  # 2017-02-05: Version 0.1
  title <-
    tryCatch(
      readLines(file(mq.file, encoding = 'UTF-8'), 4, ok = FALSE, warn = FALSE),
      error = function(e) readLines(file(mq.file, encoding = 'UTF-16'), 4, ok = FALSE, warn = FALSE)
    ) %>%
    extract((.) %>%
              str_detect('<title>') %>%
              which) %>%
    read_html() %>%
    xml_find_first('.//title') %>%
    xml_text
  if (grepl('Strategy Tester:', title)) {
    read_html(mq.file, encoding = 'GBK') %T>%
      fetch.html.data.infos.mt4ea %>%
      append.to.html.parse
  } else if (grepl('Statement:', title)) {
    read_html(mq.file, encoding = 'GBK') %T>%
      fetch.html.data.infos.mt4trade %>%
      append.to.html.parse
  } else if (grepl('Strategy Tester Report', title)) {
    read_html(mq.file, encoding = 'UTF-16') %T>%
      fetch.html.data.infos.mt5ea %>%
      append.to.html.parse
  } else if (grepl('Trade History Report', title)) {
    read_html(mq.file, encoding = 'UTF-16') %T>%
      fetch.html.data.infos.mt5trade %>%
      append.to.html.parse
  } else if (grepl('Closed Trades Report', title)) {
    fetch.html.data.infos.mt4m_closed()
    append.to.html.parse('NEEDLESS')
  } else if (grepl('Raw Report', title)) {
    fetch.html.data.infos.mt4m_raw()
    append.to.html.parse('NEEDLESS')
  } else {
    return(NULL)
  }
}
# 
# #### FETCH TICKETS ####
# fetch.html.data.tickets.mt4ea <- function(mq.file, mq.file.parse) {
#   table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'GBK', which = 2,
#                          colClasses = c(rep('character', 3), rep('numeric', 7))) %>%
#     as.data.table %>%
#     setNames(c('deal', 'time', 'type', 'tickets', 'volume', 'price', 'sl', 'tp', 'profit', 'balance')) %>%
#     extract(type != 'modify', -c('deal', 'balance'))
#   # 
#   # table.values <- xml_text(xml_find_all(xml_find_all(xml_find_all(mq.file.parse, '//table')[2], './/tr'), './/td'))
#   # colspan <- as.numeric(xml_attr(xml_find_all(xml_find_all(mq.file.parse, '//table')[2], './/td'), 'colspan'))
#   # print(xml_length(xml_find_all(xml_find_all(mq.file.parse, '//table'), './/tr'), only_elements = FALSE))
#   # build.html.table(table.values, colspan, 10)
#   
#   # data.ta
#   # head.lines <- xml_text(xml_find_all(mq.file.parse, '//table')[2])
#   # first.table <- xml_find_first(mq.file.parse, '//table')
#   # time.string <- xml_text(xml_find_all(first.table, '//td')[4])
#   # nchar.time.string <- nchar(time.string)
#   # build.infos(
#   #   type = 'MT4-EA',
#   #   name = head.lines[1],
#   #   broker = head.lines[2],
#   #   time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
#   # )
# }
# 
# fetch.html.data.tickets.mt4trade <- function(mq.file) {
#   
#   table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   # first.row <- xml_text(xml_find_all(xml_find_first(xml_find_first(mq.file.parse, '//table'), './/tr'), './/b'))
#   # build.infos(
#   #   type = 'MT4-Trade',
#   #   account = first.row[grep('Account', first.row)],
#   #   name = first.row[grep('Name', first.row)],
#   #   broker = xml_text(xml_find_first(mq.file.parse, '//b')),
#   #   currency = first.row[grep('Currency', first.row)],
#   #   leverage = first.row[grep('Leverage', first.row)],
#   #   time = tail(first.row, 1)
#   # )
# }
# 
# fetch.html.data.tickets.mt5ea <- function(mq.file) {
#   
#   table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 2)
#   # table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/td'))
#   # time.string <- table.values[grep('Period:', table.values) + 1]
#   # nchar.time.string <- nchar(time.string)
#   # build.infos(
#   #   type = 'MT5-EA',
#   #   name = table.values[grep('Expert:', table.values) + 1],
#   #   broker = table.values[grep('Broker:', table.values) + 1],
#   #   currency = table.values[grep('Currency:', table.values) + 1],
#   #   leverage = table.values[grep('Leverage:', table.values) + 1],
#   #   time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
#   # )
# }
# 
# fetch.html.data.tickets.mt5trade <- function(mq.file) {
#   
#   table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   # table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/th'))
#   # account.currency.leverage <- table.values[grep('Account:', table.values) + 1]
#   # build.infos(
#   #   type = 'MT5-Trade',
#   #   account = account.currency.leverage,
#   #   name = table.values[grep('Name:', table.values) + 1],
#   #   broker = table.values[grep('Broker:', table.values) + 1],
#   #   currency = account.currency.leverage,
#   #   leverage = account.currency.leverage,
#   #   time = format.infos.time(table.values[grep('Date:', table.values) + 1]) - 8 * 3600
#   # )
# }
# 
# fetch.html.data.tickets.mt4m_closed <- function(mq.file) {
#   table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   
#   # td.nodes <- xml_find_all(xml_find_all(mq.file.parse, '//table'), './/td')
#   # table.values <- xml_text(td.nodes)
#   # colspan <- as.numeric(xml_attr(td.nodes, 'colspan'))
#   # build.html.table(table.values, colspan, 17)
# }
# 
# fetch.html.data.tickets.mt4m_raw <- function(mq.file) {
#   table <- readHTMLTable(mq.file, colClasses = c(rep('character', 23)), stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
#   # build.infos(
#   #   type = 'MT4M-Raw'
#   # )
# }
