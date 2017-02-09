## 2017-02-05: Version 0.1

library(compiler)
compilePKGS(T)



read.mq.file <- function(mq.files, cluster) {
  # ''' read mq files (V) '''
  # @param mq.files: MetaQuote files.
  # @return:
  # 2017-02-05: Version 0.1
  mq.names <- mq.file.name(mq.files)
  if (missing(cluster) || length(mq.files) < 4) {
    mapply(fetch.file.data, mq.files, mq.names)
  } else (
    clusterMap(cluster, fetch.file.data, mq.files, mq.names)
  )
  
  # fetch.file.data(mq.files, mq.names)
}

mq.file.name <- function(mq.files) {
  # ''' get mq file names (V) '''
  # @param mq.files: MetaQuote files.
  # @return: names of MetaQuote files.
  # 2017-02-07: Version 1.0
  # 2017-02-06: Version 0.2 ifelse not good for this. is.data.frame or is.character return 1-length vector.
  # 2017-02-05: Version 0.1
  if (is.data.frame(mq.files)) {
    return(mq.files$name)
  }
  return(basename(mq.files))
} # FINISH

fetch.file.data <- function(mq.file, mq.file.name) {
  # ''' fetch mq file's data (S) '''
  # @param mq.files: MetaQuote file.
  # @param mq.names: MetaQuote file-name.
  # @return: data of MetaQuote file.
  # 2017-02-05: Version 0.1
  if (grepl('.(html|htm)$', mq.file.name)) {
    data <- fetch.html.data(mq.file)
  } else if (grepl('.(xlsx|xls)$', mq.file.name)) {
    ## ToDo ####
    data <- fetch.excel.data(mq.file, mq.file.name)
  } else if (grepl('.(csv)$', mq.file.name)) {
    ## ToDo ####
    data <- fetch.csv.data(mq.file, mq.file.name)
  } else {
    data <- NULL
  }
  data
  
  ## ToDo ####
}


fetch.html.data <- function(mq.file) {
  # ''' fetch mq html's data (S) '''
  # @param mq.files: MetaQuote file.
  # @param mq.names: MetaQuote file-name.
  # @return: data of MetaQuote file.
  # 2017-02-05: Version 0.1
  parse <- tryCatch(
    read_html(mq.file, encoding = 'GBK'),
    error = function(e) read_html(mq.file, encoding = 'UTF-8')
  )
  title <- xml_text(xml_find_first(parse, '//title'))
  # infos <-
  if (grepl('Strategy Tester:', title)) {
    infos <- fetch.html.data.infos.mt4ea(parse)
    tickets <- fetch.html.data.tickets.mt4ea(mq.file)
    # return(MetaQuote.HTML.MT4EA.Report$new(file.path, file.name, html.parse))
  } else if (grepl('Statement:', title)) {
    infos <- fetch.html.data.infos.mt4trade(parse)
    tickets <- fetch.html.data.tickets.mt4trade(mq.file)
    # return(MetaQuote.HTML.MT4Trade.Report$new(file.path, file.name, html.parse))
  } else if (grepl('Strategy Tester Report', title)) {
    infos <- fetch.html.data.infos.mt5ea(parse)
    tickets <- fetch.html.data.tickets.mt5ea(mq.file)
    # return(MetaQuote.HTML.MT5EA.Report$new(file.path, file.name, html.parse))
  } else if (grepl('Trade History Report', title)) {
    infos <- fetch.html.data.infos.mt5trade(parse)
    tickets <- fetch.html.data.tickets.mt5trade(mq.file)
    # return(MetaQuote.HTML.MT5Trade.Report$new(file.path, file.name, html.parse))
  } else if (grepl('Closed Trades Report', title)) {
    infos <- fetch.html.data.infos.mt4m_closed(parse)
    tickets <- fetch.html.data.tickets.mt4m_closed(mq.file)
    # return(MetaQuote.HTML.MT4M_Closed.Report$new(file.path, file.name))
  } else if (grepl('Raw Report', title)) {
    infos <- fetch.html.data.infos.mt4m_raw(parse)
    tickets <- fetch.html.data.tickets.mt4m_raw(mq.file)
    # return(MetaQuote.HTML.MT4M_Raw.Report$new(file.path, file.name))
  }
  # return(NULL)
}

#### FETCH INFOS ####
fetch.html.data.infos.mt4ea <- function(mq.file.parse) {
  
  head.lines <- xml_text(xml_find_all(mq.file.parse, '//b')[2:3])
  first.table <- xml_find_first(mq.file.parse, '//table')
  time.string <- xml_text(xml_find_all(first.table, '//td')[4])
  nchar.time.string <- nchar(time.string)
  build.infos(
    type = 'MT4-EA',
    name = head.lines[1],
    broker = head.lines[2],
    time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
  )
}

fetch.html.data.infos.mt4trade <- function(mq.file.parse) {
  
  first.row <- xml_text(xml_find_all(xml_find_first(xml_find_first(mq.file.parse, '//table'), './/tr'), './/b'))
  build.infos(
    type = 'MT4-Trade',
    account = first.row[grep('Account', first.row)],
    name = first.row[grep('Name', first.row)],
    broker = xml_text(xml_find_first(mq.file.parse, '//b')),
    currency = first.row[grep('Currency', first.row)],
    leverage = first.row[grep('Leverage', first.row)],
    time = tail(first.row, 1)
  )
}

fetch.html.data.infos.mt5ea <- function(mq.file.parse) {
  
  table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/td'))
  time.string <- table.values[grep('Period:', table.values) + 1]
  nchar.time.string <- nchar(time.string)
  build.infos(
    type = 'MT5-EA',
    name = table.values[grep('Expert:', table.values) + 1],
    broker = table.values[grep('Broker:', table.values) + 1],
    currency = table.values[grep('Currency:', table.values) + 1],
    leverage = table.values[grep('Leverage:', table.values) + 1],
    time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
  )
}

fetch.html.data.infos.mt5trade <- function(mq.file.parse) {
  
  table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/th'))
  account.currency.leverage <- table.values[grep('Account:', table.values) + 1]
  build.infos(
    type = 'MT5-Trade',
    account = account.currency.leverage,
    name = table.values[grep('Name:', table.values) + 1],
    broker = table.values[grep('Broker:', table.values) + 1],
    currency = account.currency.leverage,
    leverage = account.currency.leverage,
    time = format.infos.time(table.values[grep('Date:', table.values) + 1]) - 8 * 3600
  )
}

fetch.html.data.infos.mt4m_closed <- function(mq.file.parse) {
  build.infos(
    type = 'MT4M-Closed'
  )
}

fetch.html.data.infos.mt4m_raw <- function(mq.file.parse) {
  build.infos(
    type = 'MT4M-Raw'
  )
}

#### INFOS BUILDER ####
build.infos <- function(file=NA, type=NA, account=NA, name=NA, broker=NA, currency=NA, leverage=NA, time=NA) {
  
  data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    File = file,
    Type = type,
    Account = format.infos.account(account),
    Name = format.infos.name(name),
    Broker = format.infos.broker(broker),
    Currency = format.infos.currency(currency),
    Leverage = format.infos.leverage(leverage),
    Time = format.infos.time(time)
  )
}

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
    if (match1 > 0) {
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
  if (match1 > 0) {
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
    if (match1 > 0) {
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
}

format.mt4trade.infos.time <- function(time) {
  # ''' format mt4trade info time '''
  # 2016-08-16: Done
  local_time <- Sys.getlocale('LC_TIME')
  Sys.setlocale('LC_TIME', 'us')
  new_time <- as.POSIXct(time, '%Y %b %d, %H:%M', tz = 'GMT')
  Sys.setlocale('LC_TIME', local_time)
  new_time
} # 2016-08-16: Done

format.time.numeric.to.posixct <- function(time) {
  as.POSIXct(time, origin = '1970-01-01', tz = 'GMT')
}


#### FETCH TICKETS ####
fetch.html.data.tickets.mt4ea <- function(mq.file, mq.file.parse) {
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'GBK', which = 2,
                         colClasses = c(rep('character', 3), rep('integer', 7)))
  # 
  # table.values <- xml_text(xml_find_all(xml_find_all(xml_find_all(mq.file.parse, '//table')[2], './/tr'), './/td'))
  # colspan <- as.numeric(xml_attr(xml_find_all(xml_find_all(mq.file.parse, '//table')[2], './/td'), 'colspan'))
  # print(xml_length(xml_find_all(xml_find_all(mq.file.parse, '//table'), './/tr'), only_elements = FALSE))
  # build.html.table(table.values, colspan, 10)
  
  # data.ta
  # head.lines <- xml_text(xml_find_all(mq.file.parse, '//table')[2])
  # first.table <- xml_find_first(mq.file.parse, '//table')
  # time.string <- xml_text(xml_find_all(first.table, '//td')[4])
  # nchar.time.string <- nchar(time.string)
  # build.infos(
  #   type = 'MT4-EA',
  #   name = head.lines[1],
  #   broker = head.lines[2],
  #   time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
  # )
}

fetch.html.data.tickets.mt4trade <- function(mq.file) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  # first.row <- xml_text(xml_find_all(xml_find_first(xml_find_first(mq.file.parse, '//table'), './/tr'), './/b'))
  # build.infos(
  #   type = 'MT4-Trade',
  #   account = first.row[grep('Account', first.row)],
  #   name = first.row[grep('Name', first.row)],
  #   broker = xml_text(xml_find_first(mq.file.parse, '//b')),
  #   currency = first.row[grep('Currency', first.row)],
  #   leverage = first.row[grep('Leverage', first.row)],
  #   time = tail(first.row, 1)
  # )
}

fetch.html.data.tickets.mt5ea <- function(mq.file) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 2)
  # table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/td'))
  # time.string <- table.values[grep('Period:', table.values) + 1]
  # nchar.time.string <- nchar(time.string)
  # build.infos(
  #   type = 'MT5-EA',
  #   name = table.values[grep('Expert:', table.values) + 1],
  #   broker = table.values[grep('Broker:', table.values) + 1],
  #   currency = table.values[grep('Currency:', table.values) + 1],
  #   leverage = table.values[grep('Leverage:', table.values) + 1],
  #   time = substr(time.string, nchar.time.string - 10, nchar.time.string - 1)
  # )
}

fetch.html.data.tickets.mt5trade <- function(mq.file) {
  
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  # table.values <- xml_text(xml_find_all(xml_find_first(mq.file.parse, '//table'), './/th'))
  # account.currency.leverage <- table.values[grep('Account:', table.values) + 1]
  # build.infos(
  #   type = 'MT5-Trade',
  #   account = account.currency.leverage,
  #   name = table.values[grep('Name:', table.values) + 1],
  #   broker = table.values[grep('Broker:', table.values) + 1],
  #   currency = account.currency.leverage,
  #   leverage = account.currency.leverage,
  #   time = format.infos.time(table.values[grep('Date:', table.values) + 1]) - 8 * 3600
  # )
}

fetch.html.data.tickets.mt4m_closed <- function(mq.file) {
  table <- readHTMLTable(mq.file, stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  
  # td.nodes <- xml_find_all(xml_find_all(mq.file.parse, '//table'), './/td')
  # table.values <- xml_text(td.nodes)
  # colspan <- as.numeric(xml_attr(td.nodes, 'colspan'))
  # build.html.table(table.values, colspan, 17)
}

fetch.html.data.tickets.mt4m_raw <- function(mq.file) {
  table <- readHTMLTable(mq.file, colClasses = c(rep('character', 23)), stringsAsFactors = FALSE, encoding = 'UTF-8', which = 1)
  # build.infos(
  #   type = 'MT4M-Raw'
  # )
}
