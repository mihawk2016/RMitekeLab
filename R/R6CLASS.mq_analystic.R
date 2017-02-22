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
      private$setting.parallel.threshold.read.files <- PARALLEL.THRESHOLD.READ.FILES
    },
    add.files = function(files, parallel=FALSE) {
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
      private$mismatch <- c()
      private$report <- list()
      private$merged.report <- NULL
    },
    
    
    #### GETTER & SETTER ####
    get.report = function(member, index) {
      if (missing(index)) {
        index <- 1:length(private$report)
      }
      if (missing(member)) {
        return(private$report[index])
      }
      if (length(member) == 1) {
        lapply(private$report[index], function(r) r[[member]])
      } else {
        lapply(private$report[index], function(r) r[member])
      }
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
    get.merged.data = function(member) {
      if (missing) {
        private$merged.data
      } else {
        private$merged.data[[member]]
      }
    },
    set.merged.data = function(member, value) {
      if (missing) {
        private$merged.data
      } else {
        private$merged.data[[member]] <- value 
      }
    },
    append = function(member, value) {
      private[[member]] %<>% c(value)
    }
  ),
  private = list(
    setting.parallel.threshold.read.files = NULL,
    mismatch = c(),
    report = list(),
    merged.report = NULL
  )
)