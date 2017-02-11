## 2017-02-10: Version 0.1

#### SYMBOLS SETTING ####
SYMBOLS.SETTING <-
  data.table(
    SYMBOL = c('AUDCAD', 'AUDCHF', 'AUDJPY', 'AUDNZD', 'AUDUSD', 'CADCHF', 'CADJPY', 'CHFJPY', 'EURAUD', 'EURCAD',
               'EURCHF', 'EURGBP', 'EURJPY', 'EURNZD', 'EURUSD', 'GBPAUD', 'GBPCAD', 'GBPCHF', 'GBPJPY', 'GBPNZD',
               'GBPUSD', 'NZDCAD', 'NZDCHF', 'NZDJPY', 'NZDUSD', 'USDCAD', 'USDCHF', 'USDJPY', 'XAGUSD', 'XAUUSD'),
    SPREAD = c(40,40,40,40,20,40,40,40,40,40,40,40,40,40,20,40,20,40,40,40,20,40,40,40,20,20,20,40,50,500),
    DIGITS = c(5,5,3,5,5,5,3,3,5,5,5,5,3,5,5,5,5,5,3,5,5,5,5,3,5,5,5,3,3,2),
    CONTRACT.SIZE = c(rep(100000, 28), 1000, 100)
  ) %>%
  extract(j = TICKVALUE.POINT := CONTRACT.SIZE * 10 ^ -DIGITS) %>%
  setkey(SYMBOL)

SUPPORT.SYMBOLS <- SYMBOLS.SETTING[, SYMBOL]

#### TICKETS.COLUMNS ####
TICKETS.COLUMNS <- list(
  UNIFORM = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
              'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'GROUP', 'COMMENT', 'EXIT'),
  MONEY = c('TICKET', 'OTIME', 'PROFIT'),
  CLOSED = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
             'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', 'EXIT'),
  OPEN = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
           'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT'),
  PENDING = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
              'CTIME', 'CPRICE'),
  WORKING = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP', 'CPRICE')
)

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
  table
} # FINISH

#### DEFAULT LEVERAGE ####
DEFAULT.LEVERAGE <- 100

#### DEFAULT CURRENCY ####
DEFAULT.CURRENCY <- 'USD'
