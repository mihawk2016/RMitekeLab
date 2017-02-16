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
      suppressWarnings(readLines(con8 <- file(mq.file, open = 'rt', encoding = 'UTF-8'), 4, ok = FALSE, warn = FALSE)),
      error = function(e) {
        lines <- readLines(con16 <- file(mq.file, open = 'rt', encoding = 'UTF-16'), 4, ok = FALSE, warn = FALSE)
        close(con16)
        lines
      },
      finally = close(con8)
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

