library(compiler)
library(R6)
compilePKGS(T)


#### @UPDATE IDEA@ ####
## 2017-02-22: 

#### @PATCH NOTE@ ####
## 2017-02-22: Version 0.1

MQ_ANALYSTIC <- R6Class(
  classname = 'MetaQuote Analystic',
  public = list(
    initialize = function() {
      private$SYMBOLS.SETTING <- SYMBOLS.SETTING
      private$MYSQL.SETTING <- MYSQL.SETTING
      private$DB.OPEN.FUN <- DB.O
      private$DB.OHLC.FUN <- DB.OHLC
    },
    #### ACTIONS ####
    add.files = function(files, parallel=private$PARALLEL.THRESHOLD.READ.FILES) {
      files.data <- read.mq.file(files, parallel)
      mismatch.index <-
        sapply(files.data, is.character) %>% which
      if (length(mismatch.index)) {
        self$append('mismatch', unlist(files.data[mismatch.index]))
        self$append('report', files.data[-mismatch.index])
      } else {
        self$append('report', files.data)
      }
    },
    clear.files = function() {
      private$selected.index = c()
      private$mismatch <- c()
      private$report <- list()
      private$merged.report <- NULL
    },
    build.focus = function() {
      
    },
    
    #### GETTER & SETTER ####
    get.report = function(member, index,
                          default.currency=private$DEFAULT.CURRENCY,
                          default.leverage=private$DEFAULT.LEVERAGE,
                          get.open.fun=private$DB.OPEN.FUN,
                          mysql.setting=private$MYSQL.SETTING,
                          timeframe=private$TIMEFRAME.TICKVALUE,
                          symbols.setting=private$SYMBOLS.SETTING,
                          parallel=private$PARALLEL.THRESHOLD.GENERATE.TICKETS) {
      if (missing(index)) {
        index <- 1:length(private$report)
      }
      if (missing(member)) {
        return(private$report[index])
      }
      if (all(member %in% CONTENT.PHASE1.INIT)) {
        return(private$get.report.simple(member, index))
      }
      if (any(member %in% CONTENT.PHASE2.TICKETS)) {
        null.sub.index <- sapply(private$report[index], function(r) is.null(r$CURRENCY)) %>% which
        if (length(null.sub.index)) {
          null.index <- index[null.sub.index]
          new.report <- generate.html.tickets2(private$report[null.index], null.index, default.currency, default.leverage,
                                               get.open.fun, mysql.setting, timeframe, symbols.setting, parallel)
          self$set.report(index = null.index, value = new.report)
        }
      }
      return(private$get.report.simple(member, index))
    },
    set.report = function(member, index, value) {
      if (missing(index)) {
        index <- 1:length(private$report)
      }
      if (missing(member)) {
        return(private$report[index] <- value)
      }
      private$report[index] <-
        mapply(function(r, m, v) {
          r %>% inset(m, v)
        }, r = private$report[index], v = value, MoreArgs = list(m = member))
    },
    get.focus = function(member) {
      if (missing) {
        private$focus.report
      } else {
        private$focus.report[[member]]
      }
    },
    set.focus = function(member, value) {
      if (missing) {
        private$focus.report
      } else {
        private$focus.report[[member]] <- value 
      }
    },
    get = function(member) {
      private[[member]]
    },
    set = function(member, value) {
      private[[member]] <- value
    },
    append = function(member, value) {
      private[[member]] %<>% c(value)
    }
  ),
  private = list(
    SYMBOLS.SETTING = NULL,
    PARALLEL.THRESHOLD.READ.FILES = 200,
    PARALLEL.THRESHOLD.GENERATE.TICKETS = 6,
    DEFAULT.LEVERAGE = 100,
    DEFAULT.CURRENCY = 'USD',
    TIMEFRAME.TICKVALUE = 'M1',
    TIMEFRAME.REPORT = 'H1',
    MYSQL.SETTING =NULL,
    DB.OPEN.FUN = NULL,
    DB.OHLC.FUN = NULL,
    
    selected.index = c(),
    mismatch = c(),
    report = list(),
    focus.report = NULL,
    
    #### GETTER & SETTER ####
    get.report.simple = function(member, index) {
      if (length(member) == 1) {
        lapply(private$report[index], function(r) r[[member]])
      } else {
        lapply(private$report[index], function(r) r[member])
      }
    }
  )
)