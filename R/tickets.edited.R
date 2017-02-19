library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-17: 

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
                       MEAN = round(mean(NPROFIT), 2),
                       MAX = ifelse(sign(NPROFIT[1]) == -1, min(NPROFIT), max(NPROFIT)),
                       PIP.SUM = sum(PIP),
                       PIP.MEAN = round(mean(PIP), 2),
                       PIP.MAX = ifelse(sign(PIP[1]) == -1, min(PIP), max(PIP)),
                       VOL.SUM = sum(VOLUME),
                       VOL.MEAN = round(mean(VOLUME), 2),
                       VOL.MAX = ifelse(sign(VOLUME[1]) == -1, min(VOLUME), max(VOLUME)),
                       VOL.MIN = ifelse(sign(VOLUME[1]) == -1, max(VOLUME), min(VOLUME))
                       ),
                 by = RESULT] %>%
    setkey(RESULT) %>%
    extract(c('PROFIT', 'LOSS')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.by.exit <- function(tickets.edited) {
  tickets.edited[i = !is.na(EXIT),
                 j = .(N = .N,
                       NPROFIT = sum(NPROFIT)
                       ),
                 by = EXIT] %>%
    setkey(EXIT) %>%
    extract(c('SL', 'TP', 'SO')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.by.type <- function(tickets.edited) {
  tickets.edited[j = .(N = .N,
                       NVOLUME = sum(VOLUME)
                 ),
                 by = TYPE] %>%
    setkey(TYPE) %>%
    extract(c('BUY', 'SELL')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.summary <- function(statistics.result) {
  statistics.result[j = .('N.TRADE' = sum(N),
                          'NET.PROFIT' = sum(SUM),
                          'NET.PIP' = sum(PIP.SUM),
                          'SUM.VOLUME' = sum(VOL.SUM),
                          'WIN%' = round(N[1] / sum(N) * 100, 2),
                          'PROFIT.FACTOR' = round(SUM[1] / -SUM[2], 2),
                          'EXPECT' = round(sum(SUM) / sum(N), 2),
                          'VOL/TRADE' = round(sum(VOL.SUM) / sum(N), 2),
                          'PROFIT/TRADE' = round(sum(SUM) / sum(VOL.SUM), 2)
                          )]
}

tickets.statistics.continuous <- function(tickets.edited) {
  nprofit <-
    tickets.edited %>%
    setkey(CTIME) %>%
    extract(j = NPROFIT)
  col.name <- c('CON.N.MAX', 'CON.N.MEAN', 'CON.MAX', 'CON.MEAN')
  row.name <- c('PROFIT', 'LOSS')
  con.table <-
    data.table(RESULT = row.name, matrix(data = NA_real_, nrow = length(row.name), ncol = length(col.name),
                                         dimnames = list(NULL, col.name))) %>%
    setkey(RESULT)
  continuous <- cal.continuous(nprofit)
  if (length(continuous$UP.FROM)) {
    up.n <- with(continuous, UP.TO - UP.FROM) + 1
    up.pl <- mapply(function(from, to) {
      sum(nprofit[from:to])
    }, from = continuous$UP.FROM, to = continuous$UP.TO)
    con.table['PROFIT', (col.name) := list(max(up.n), mean(up.n), max(up.pl), mean(up.pl))]
  }
  if (length(continuous$DN.FROM)) {
    dn.n <- with(continuous, DN.TO - DN.FROM) + 1
    dn.pl <- mapply(function(from, to) {
      sum(nprofit[from:to])
    }, from = continuous$DN.FROM, to = continuous$DN.TO)
    con.table['LOSS', (col.name) := list(max(dn.n), mean(dn.n), max(dn.pl), mean(dn.pl))]
  }
  con.table[row.name]
} # FINISH



#### UTILS ####
cal.continuous <- function(x) {
  if (!(len <- length(x))) {
    return(NULL)
  }
  signs <- ifelse(x >= 0, 1, 0)
  turns <- diff(signs)
  dn.to <- which(turns == 1)
  up.from <- dn.to + 1
  up.to <- which(turns == -1)
  dn.from <- up.to + 1
  if (signs[1]) {
    up.from %<>% c(1, .)
  } else {
    dn.from %<>% c(1, .)
  }
  if (signs[len]) {
    up.to %<>% c(len)
  } else {
    dn.to %<>% c(len)
  }
  list(UP.FROM = up.from, UP.TO = up.to, DN.FROM = dn.from, DN.TO = dn.to)
} # FINISH

maxdrawdown <- function(x) {
  if (!(len <- length(x))) {
    return(NULL)
  }
  cum.drawdown <- cummax(x) - x
  mdd <- max(cum.drawdown)
  to <- which(mdd == cum.drawdown)
  cum.max <- which(cum.drawdown  == 0)
  from <- sapply(to, function(x) {
    cum.drawdown[sum(cum.drawdown < to)]
  })
  list(MDD = mdd, FROM = from, TO = to)
} # FINISH
