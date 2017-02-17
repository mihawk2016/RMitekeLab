library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####


#### @PATCH NOTE@ ####
## 2017-02-17: Version 0.1

## additional tickets column: PIP, NPROFIT, RESULT, LOT.PROFIT

tickets.edited <- function(tickets.editing, get.open.fun=DB.O, timeframe='M1', currency=DEFAULT.CURRENCY,
                           symbols.setting=SYMBOLS.SETTING) {
  tickets.editing %>%
    setkey(SYMBOL) %>%
    extract(j = c('PIP', 'PROFIT') := {
      symbol <- SYMBOL[1]
      pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
      tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, timeframe, currency, symbols.setting)
      profit <- cal.profits(VOLUME, tickvalue, pip)
      list(pip, profitt)
    }, by = SYMBOL) %>%
    extract(j = c('NPROFIT', 'LOT.PROFIT', 'RESULT', 'PERIOD') :=
              list(COMMISSION + TAXES + SWAP + PROFIT, PROFIT / VOLUME, ifelse(NPROFIT >= 0, 'PROFIT', 'LOSS'), CTIME - OTIME))
} # FINISH

tickets.period <- function(tickets.edited) {
  period.from <- min(tickets.edited[, OTIME], na.rm = TRUE)
  period.to <- max(tickets.edited[, CTIME], na.rm = TRUE)
  period.vector <-
    format.time.numeric.to.posixct(c(period.from, period.to)) %>%
    as.Date
  trade.days <-
    seq.Date(from = period.vector[1], to = period.vector[2], by = 'day') %>%
    wday %>%
    extract(!. %in% c(1,7)) %>%
    length
  nature.interval <- interval(period.vector[1], period.vector[2], tz = 'GMT')
  nature.period <- as.period(nature.interval)
  data.table(
    FROM = period.from,
    TO = period.to,
    NATURE.INTERVAL = nature.interval,
    NATURE.PERIOD = nature.period,
    TRADE.DAYS = trade.days
  )
} # FINISH

tickets.statistics <- function(tickets.edited) {
  
}

tickets.statistics.by.result <- function(tickets.edited) {
  tickets.edited[j = .(N = .N,
                       SUM = sum(NPROFIT),
                       MEAN = mean(NPROFIT),
                       MAX = ifelse(
                         # max(abs(NPROFIT)),
                       SUM.PIP = sum(PIP)
                       ),
                 by = RESULT]
}