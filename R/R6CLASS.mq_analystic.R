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
        self$append('m.mismatch', unlist(files.data[mismatch.index]))
        self$append('m.infos', lapply(files.data[-mismatch.index], function(file.data) file.data$INFOS))
        self$append('m.html.parse', lapply(files.data[-mismatch.index], function(file.data) file.data$HTML.PARSE))
      } else {
        self$append('m.infos', lapply(files.data, function(file.data) file.data$INFOS))
        self$append('m.html.parse', lapply(files.data, function(file.data) file.data$HTML.PARSE))
      }
    },
    clear.files = function() {
      
    },
    
    
    #### GETTER & SETTER ####
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
    setting.parallel.threshold.read.files = NULL,
    m.mismatch = c(),
    m.infos = list(),
    m.html.parse = list()
  )
)