library(compiler)
compilePKGS(T)

#### @PATCH NOTE@ ####
## 2017-02-13: Version 0.2 speed of mysql is good enough, hang-up the local method
## 2017-02-13: Version 0.1

MYSQL.SETTING <- list(
  HOST = '192.168.2.103',
  PORT = 3306,
  USERNAME = 'root',
  PASSWORD = '',
  DBNAME = 'historical_data'
)
# 
# #### LOCAL ENVIRONMENT ####
# init.local.data <- function() {
#   assign('LOCAL.DATA', NULL, envir = METAQUOTE.ANALYSTIC)
# }
# 
# get.local.data <- function() {
#   get('LOCAL.DATA', envir = METAQUOTE.ANALYSTIC)
# }
# 
# set.local.data <- function(local.data) {
#   METAQUOTE.ANALYSTIC$LOCAL.DATA <- local.data
# }
# 
# get.local.data.table <- function(symbol, timeframe) {
#   get('LOCAL.DATA', envir = METAQUOTE.ANALYSTIC) %>%
#     extract2(symbol) %>%
#     extract2(timeframe)
# }
# 
# set.local.data.table <- function(symbol, timeframe, data) {
#   symbols <-
#     get.local.data() %>%
#     names
#   if (!symbol %in% symbols) {
#     METAQUOTE.ANALYSTIC$LOCAL.DATA[[symbol]] <- list()
#   }
#   METAQUOTE.ANALYSTIC$LOCAL.DATA[[symbol]][[timeframe]] <- data
# }
# 
# set.local.file.path <- function(file) {
#   METAQUOTE.ANALYSTIC$local.file <- file
# }
# 
# get.local.file.path <- function() {
#   METAQUOTE.ANALYSTIC$local.file
# }
# 
# #### LOCAL ####
# load.local.file <- function(file) {
#   set.local.file.path(file)
#   if (!file.exists(file)) {
#     file.create(file)
#     set.local.data(build.local.data.tree())
#     return(NULL)
#   }
#   tryCatch(
#     set.local.data(get(load(file))),
#     error = function(e) {
#       message('Local Data File ERROR')
#       NULL
#     }
#   )
# }
# 
# save.local.file = function(data, file) {
#   if (missing(file)) {
#     file <- get.local.file.path()
#   }
#   tryCatch(
#     save(data, file = file),
#     error = function(e)  {
#       message('Local Data File ERROR')
#       NULL
#     }
#   )
# }
# 
# build.local.data.tree <- function(support.symbols=SYMBOLS.SETTING[, SYMBOL], timeframes=TIMEFRAMES) {
#   timeframe.list <-
#     vector('list', length(timeframes)) %>%
#     set_names(timeframes)
#   symbol.list <-
#     vector('list', length(support.symbols)) %>%
#     set_names(support.symbols)
#   lapply(symbol.list, function(symbol) timeframe.list)
# } # FINISH
# 
# read.data.csv <- function(file.csv) {
#   csv <- fread(file.csv)
#   new.csv <-
#     data.table(
#       ymd_hm(paste(csv$V1, csv$V2), tz = 'GMT'),
#       csv[j = c(3:7)]
#     ) %>%
#     as.xts.data.table %>%
#     set_colnames(c('Open', 'High', 'Low', 'Close', 'Volume'))
# } # FINISH
# 
# import.csv = function(file.csv) {
#   if (is.data.frame(file.csv)) {
#     file.name <- file.csv$name
#   } else {
#     file.name <- basename(file.csv)
#   }
#   split.file.name <- strsplit(file.name, '_')[[1]]
#   symbol <- split.file.name[1]
#   timeframe <- split.file.name[2]
#   new.data <- read.data.csv(file.csv)
#   old.data <- get.local.data.table(symbol, timeframe)
#   merged.data <- rbind(old.data, new.data)
#   merged.data <- merged.data[!duplicated(index(merged.data))] 
#   set.local.data.table(symbol, timeframe, merged.data)
# } # FINISH

#### MYSQL ####
mysql.query <- function(sql, host=MYSQL.SETTING$HOST, port=MYSQL.SETTING$PORT,
                        username=MYSQL.SETTING$USERNAME, password=MYSQL.SETTING$PASSWORD, dbname=MYSQL.SETTING$DBNAME) {
  # ''' mysql query '''
  # 2017-01-22: Version 1.0
  if (length(sql) == 0) {
    return(NULL)
  }
  mysql.connect <- tryCatch(
    dbConnect(MySQL(), host = host, port = port, username = username, password = password, dbname = dbname),
    error = function(e) {
      message('MySQL Connect ERROR')
      NULL
    }
  )
  if (is.null(mysql.connect)) {
    return(NULL)
  }
  res <- lapply(sql, function(s) {
    dbGetQuery(mysql.connect, s)
  })
  dbDisconnect(mysql.connect)
  res
}

mysql.price.open <- function(symbol, time, timeframe='M1') {
  # ''' get open data from mysql database '''
  # 2017-01-22: Version 1.0
  table <-
    paste(symbol, timeframe, sep = '_') %>%
    tolower
  max.time <-
    max(time)
  min.time <-
    min(time)
  # time.posixct <- format.time.numeric.to.posixct(time)
  
  
  from <- gsub('-', '.', as.character(as.Date(min.time)))
  to <- gsub('-', '.', as.character(as.Date(max.time) + 1))
  sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
  sql <- sprintf(sql.string, table, from, to)
  query.result <- mysql.query(sql)[[1]]
  
  open.serie <- xts(query.result$open, as.POSIXct(strptime(with(query.result, time), '%Y.%m.%d %H:%M', tz = 'GMT')))
  time.string <- paste0('/', gsub('[.]', '-', as.character(time)))
  sapply(time.string, function(.time) {
    if (nchar(.time) == 11) {
      time <- paste(.time, '00:00')
    }
    res <- open.serie[.time]
    if (nrow(res) == 0) {
      return(NA)
    }
    res <- tail(res, 1)
    #### ToDo right now just use 7200 as threshold ####
    if (difftime(.format.time(gsub('/', '', .time)), index(res), units = 'secs') > 3600 * 2) {
      return(NA)
    }
    res
  })
}

# mysql.price.open <- function(symbol, time, timeframe='M1') {
#   # ''' get open data from mysql database '''
#   # 2017-01-22: Version 1.0
#   table <-
#     paste(symbol, timeframe, sep = '_') %>%
#     tolower
#   max.time <- max(time)
#   min.time <- min(time)
#   time.posixct <- format.time.numeric.to.posixct(time)
#   
#   
#   from <- gsub('-', '.', as.character(as.Date(min.time)))
#   to <- gsub('-', '.', as.character(as.Date(max.time) + 1))
#   sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
#   sql <- sprintf(sql.string, table, from, to)
#   query.result <- mysql.query(sql)[[1]]
#   
#   open.serie <- xts(query.result$open, as.POSIXct(strptime(with(query.result, time), '%Y.%m.%d %H:%M', tz = 'GMT')))
#   time.string <- paste0('/', gsub('[.]', '-', as.character(time)))
#   sapply(time.string, function(.time) {
#     if (nchar(.time) == 11) {
#       time <- paste(.time, '00:00')
#     }
#     res <- open.serie[.time]
#     if (nrow(res) == 0) {
#       return(NA)
#     }
#     res <- tail(res, 1)
#     #### ToDo right now just use 7200 as threshold ####
#     if (difftime(.format.time(gsub('/', '', .time)), index(res), units = 'secs') > 3600 * 2) {
#       return(NA)
#     }
#     res
#   })
# }

mysql.price.ohlc <- function(symbol, from, to, timeframe='M1') {
  # ''' get ohlc from mysql database '''
  # 2017-01-22: Version 1.0
  table <-
    paste(symbol, timeframe, sep = '_') %>%
    tolower
  
  
  from <- gsub('-', '.', as.character(as.Date(.format.time(from))))
  to <- gsub('-', '.', as.character(as.Date(.format.time(to)) + 1))
  sql.string <- "SELECT time, open, high, low, close FROM %s WHERE time BETWEEN '%s' AND '%s'"
  sql <- sprintf(sql.string, table, from, to)
  query.result <- mysql.query(sql)[[1]]
  time <- as.POSIXct(strptime(query.result$time, '%Y.%m.%d %H:%M', tz = 'GMT'))
  price <- xts(query.result[2:5], time)
  colnames(price) <- c('Open', 'High', 'Low', 'Close')
  price
}

