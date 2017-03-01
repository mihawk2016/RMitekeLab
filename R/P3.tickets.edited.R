library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-22:timeserie.account optimize, table / len 

#### @PATCH NOTE@ ####
## 2017-02-17: Version 0.1

## additional tickets column: PIP, NPROFIT, RESULT, LOT.PROFIT
reports.PHASE3 <- function(report.phase2,
                           set.init.money=NULL, include.middle=TRUE, default.money=DEFAULT.INIT.MONEY,
                           currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe.tickvalue='M1',
                           get.ohlc.fun=DB.OHLC, timeframe.report='H1', parallel.db=PARALLEL.THRESHOLD.DB.SYMBOLS,
                           margin.base=1500, symbols.setting=SYMBOLS.SETTING, parallel.tickets=PARALLEL.THRESHOLD.GENERATE.TICKETS,
                           mysql.setting=MYSQL.SETTING) {
  if (is.numeric(parallel.tickets)) {
    parallel.tickets <- length(report.phase2) >= parallel.tickets
  }
  if (!parallel.tickets) {
    phase3data <- mapply(report.PHASE3, report.phase2,
                         MoreArgs = list(set.init.money, include.middle, default.money, currency, get.open.fun, timeframe.tickvalue,
                                         get.ohlc.fun, timeframe.report, parallel.db, margin.base, symbols.setting, mysql.setting),
                         SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    cluster <- makeCluster(detectCores() - 1)
    phase3data <- clusterMap(cluster, report.PHASE3, report.phase2,
                             MoreArgs = list(set.init.money, include.middle, default.money, currency, get.open.fun, timeframe.tickvalue,
                                             get.ohlc.fun, timeframe.report, parallel.db, margin.base, symbols.setting, mysql.setting),
                             SIMPLIFY = FALSE, USE.NAMES = FALSE)
    stopCluster(cluster)
  }
  phase3data
}

report.PHASE3 <- function(report.phase2,
                          set.init.money=NULL, include.middle=TRUE, default.money=DEFAULT.INIT.MONEY,
                          currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe.tickvalue='M1',
                          get.ohlc.fun=DB.OHLC, timeframe.report='H1', parallel=PARALLEL.THRESHOLD.DB.SYMBOLS,
                          margin.base=1500, symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  within(report.phase2, {
    TICKETS.EDITED <- tickets.edited(TICKETS.EDITING, CURRENCY, get.open.fun, timeframe.tickvalue, symbols.setting)
    TICKETS.MONEY <- tickets.money(TICKETS.SUPPORTED, set.init.money, include.middle, default.money)# %T>% print
    PERIOD <- tickets.period(TICKETS.EDITED)# %T>% print
    PRICE <- price.data(TICKETS.EDITED, PERIOD, get.ohlc.fun, timeframe.report, mysql.setting, parallel)# %T>% print
    TIMESERIE.TICKETS <- timeseries.tickets(TICKETS.EDITED, PRICE %>% price.data.with.tickvalue(
      get.open.fun, timeframe.tickvalue, currency, mysql.setting, symbols.setting), symbols.setting)# %T>% print
    TIMESERIE.SYMBOLS <- timeseries.symbols(TIMESERIE.TICKETS, TICKETS.MONEY)# %T>% print
    .timeserie.account <- timeseries.account(TIMESERIE.SYMBOLS, TICKETS.MONEY, margin.base)# %T>% print
    TIMESERIE.ACCOUNT <- .timeserie.account$timeseries.symbols# %T>% print
    SYMBOLS.PROFIT_VOLUME <- .timeserie.account$symbols.profit_volume
    .timeserie.account <- NULL
    PHASE <- 3
  })
}

statistics.and.timeseries <- function(tickets.editing, currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe.tickvalue='M1',
                                      get.ohlc.fun=DB.OHLC, timeframe.report='H1', parallel=PARALLEL.THRESHOLD.DB.SYMBOLS,
                                      symbols.setting=SYMBOLS.SETTING, mysql.setting=MYSQL.SETTING) {
  within(list(), {
    TICKETS.EDITED <- tickets.edited(tickets.editing, currency, get.open.fun, mysql.setting, symbols.setting, mysql.setting)
    PERIOD <- tickets.period(TICKETS.EDITED)
    PRICE <- price.data(TICKETS.EDITED, PERIOD, get.ohlc.fun, timeframe.report, parallel)
    TIMESERIE.TICKETS <- timeseries.tickets(tickets.edited, PRICE %>% price.data.with.tickvalue(
      get.open.fun, timeframe.tickvalue, currency, symbols.setting
    ), symbols.setting)
    TIMESERIE.SYMBOLS <- timeseries.symbols(TIMESERIE.TICKETS)
    TIMESERIE.ACCOUNT <- timeseries.account(TIMESERIE.SYMBOLS)
  })
}
  
tickets.edited <- function(tickets.editing, currency=DEFAULT.CURRENCY, get.open.fun=DB.O, timeframe='M1',
                           symbols.setting=SYMBOLS.SETTING, mysql.setting) {
  tickets.editing %>%
    copy %>%
    setkey(SYMBOL) %>%
    extract(j = c('PIP', 'PROFIT', 'NPROFIT') := {
      symbol <- SYMBOL[1]
      pip <- cal.pips(TYPE, OPRICE, CPRICE, symbols.setting[symbol, DIGITS])
      tickvalue <- cal.tick.value(symbol, CTIME, get.open.fun, mysql.setting, timeframe, currency, symbols.setting)
      profit <- cal.profit(VOLUME, tickvalue, pip)
      list(pip, profit, COMMISSION + TAXES + SWAP + PROFIT)
    }, by = SYMBOL) %>%
    extract(j = c('LOT.PROFIT', 'RESULT', 'PERIOD') :=
              list(PROFIT / VOLUME, ifelse(NPROFIT >= 0, 'PROFIT', 'LOSS'), CTIME - OTIME))
} # FINISH

tickets.period <- function(tickets.edited) {
  period.from <- min(tickets.edited[, OTIME], na.rm = TRUE)
  period.to <- max(tickets.edited[, CTIME], na.rm = TRUE)
  period.vector <-
    time.numeric.to.posixct(c(period.from, period.to)) %>%
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

#### STATISTICS ####
tickets.statistics <- function(tickets.edited) {
  within(list(), {
    STATISTIC.RESULT <- tickets.statistics.by.result(tickets.edited)
    STATISTIC.EXIT <- tickets.statistics.by.exit(tickets.edited)
    STATISTIC.TYPE <- tickets.statistics.by.type(tickets.edited)
    STATISTIC.SUMMARY <- tickets.statistics.by.result(STATISTIC.RESULT)
    STATISTIC.CONTINUOUS <- tickets.statistics.continuous(tickets.edited)
  })
}

tickets.statistics.by.result <- function(tickets.edited) {
  tickets.edited %>%
    setkey(RESULT) %>%
    extract(
      j = .(N = .N,
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
    by = RESULT) %>%
    extract(c('PROFIT', 'LOSS')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.by.exit <- function(tickets.edited) {
  tickets.edited %>%
    setkey(EXIT) %>%
    extract(
      i = !is.na(EXIT),
      j = .(N = .N,
            NPROFIT = sum(NPROFIT)),
      by = EXIT) %>%
    extract(c('SL', 'TP', 'SO')) %>%
    extract(is.na(N), 2:ncol(.) := list(0))
} # FINISH

tickets.statistics.by.type <- function(tickets.edited) {
  tickets.edited %>%
    setkey(TYPE) %>%
    extract(
      j = .(N = .N,
            NVOLUME = sum(VOLUME)
      ),
      by = TYPE) %>%
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
} # FINISH

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

#### PRICE DATA ####
price.data <- function(tickets.edited, tickets.period, get.ohlc.fun=DB.OHLC, timeframe='H1',
                       mysql.setting=MYSQL.SETTING, parallel=PARALLEL.THRESHOLD.DB.SYMBOLS) {
  symbols <-
    tickets.edited[, SYMBOL] %>%
    unique
  time.period <- tickets.period$NATURE.INTERVAL
  get.ohlc.fun(symbols, int_start(time.period), int_end(time.period), timeframe, mysql.setting, parallel)
}

price.data.with.tickvalue <- function(price.data,
                                      get.open.fun=DB.O, tickvalue.timeframe='M1', currency=DEFAULT.CURRENCY,
                                      mysql.setting=MYSQL.SETTING, symbols.setting=SYMBOLS.SETTING) {
  price.data %<>% copy
  serie.columns <- c('PROFIT', 'FLOATING', 'PL.VOLUME', 'VOLUME', 'MAX.FLOATING', 'MIN.FLOATING')
  symbols <- names(price.data)
  lapply(symbols, function(symbol) {
    price.data[[symbol]] %>%
      extract(j = TICKVALUE := cal.tick.value(symbol, TIME, get.open.fun, mysql.setting, tickvalue.timeframe,
                                               currency, symbols.setting)) %>%
      extract(j = (serie.columns) := 0)
  }) %>%
    set_names(symbols)
}

#### TIMESERIES ####
timeseries <- function(tickets.edited, price.data.for.timeseries, symbols.setting=SYMBOLS.SETTING) {
  tickets.serie <- timeseries.tickets(tickets.edited, price.data.for.timeseries, symbols.setting)
  symbols.serie <- timeseries.symbols(tickets.serie)
  account.serie <- timeseries.account(symbols.serie)
  list(
    TICKETS = tickets.serie,
    SYMBOLS = symbols.serie,
    ACCOUNT = account.serie
  )
} # FINISH

timeseries.tickets <- function(tickets.edited, price.data, symbols.setting=SYMBOLS.SETTING) {
  fun.env <- environment()
  env.tickets.series <- list()
  symbols <-
    tickets.edited %>%
    setkey(SYMBOL) %>%
    extract(
      j = {
        symbol <- SYMBOL[1]
        ticket.timeserie <-
          mapply(timeseries.one.ticket, OTIME, CTIME, NPROFIT, TYPE, VOLUME, OPRICE,
                 MoreArgs = list(symbols.setting[symbol, DIGITS], symbols.setting[symbol, SPREAD],
                                 price.data[[symbol]]),
                 SIMPLIFY = FALSE) %>%
          set_names(paste('T', TICKET, sep = '_')) %>%
          list %>%
          append(env.tickets.series, .) %>%
          assign('env.tickets.series', ., envir = fun.env)
        NA
      },
      by = SYMBOL
    ) %>%
    extract(j = SYMBOL)
  setNames(env.tickets.series, symbols)
} # FINISH

timeseries.symbols <- function(timeserie.tickets, money.tickets) {
  fun.env <- environment()
  env.symbols.series <- list()
  lapply(timeserie.tickets, function(symbol.list) {
    
    symbol.table <- 0
    symbol.timeserie <-
      lapply(symbol.list, function(ticket.serie) {
        symbol.table <<- symbol.table +
          ticket.serie[, .(BALANCE.DELTA = PROFIT + FLOATING, NET.VOLUME = PL.VOLUME, SUM.VOLUME = VOLUME)]
      })
    symbol.table %>%
      extract(j = c('TIME', 'MONEY', 'EQUITY', 'RETURN') := {
        serie.time <- symbol.list[[1]][, TIME]
        money.delta <- money.delta(money.tickets, serie.time)
        equity <- BALANCE.DELTA + cumsum(money.delta)
        returns <- c(0, diff(BALANCE.DELTA) / equity[-length(equity)])
        list(serie.time, money.delta, equity, returns)
      }) %>%
      setkey(TIME) %>%
      list %>%
      append(env.symbols.series, .) %>%
      assign('env.symbols.series', ., envir = fun.env)
  })
  setNames(env.symbols.series, names(timeserie.tickets))
} # FINISH

timeseries.account <- function(timeseries.symbols, money.tickets, margin.base=1500) {
  fun.env <- environment()
  symbols.profit_volume <- list()
  symbol.names <- names(timeseries.symbols)
  account.table <- 0
  len <- length(timeseries.symbols)
  intersection.time <-
    lapply(timeseries.symbols, function(ts) {
      ts[, TIME]
    }) %>%
    do.call(c, .) %>%
    table %>%
    extract(. == len) %>%
    names %>%
    as.numeric
  mapply(function(symbol.serie, symbol) {
    fun.env$account.table %<>% add(symbol.serie[.(intersection.time),
                                                .(BALANCE.DELTA = BALANCE.DELTA, NET.VOLUME = abs(NET.VOLUME), SUM.VOLUME = SUM.VOLUME)])
    fun.env$symbols.profit_volume %<>% append(list(symbol.serie[.(intersection.time),
                                                                .(TIME = TIME, SYMBOL = symbol, BALANCE.DELTA = BALANCE.DELTA, NET.VOLUME = NET.VOLUME)]))
  }, timeseries.symbols, symbol.names)
  symbols.profit_volume %<>%
    do.call(rbind, .) %>%
    extract(
      j = TIME := time.numeric.to.posixct(TIME)
    )
  account.table %>%
    extract(j = c('TIME', 'MONEY', 'EQUITY', 'RETURN', 'MARGIN.USED', 'MARGIN.FREE') := {
      money.delta <- money.delta(money.tickets, intersection.time)
      equity <- BALANCE.DELTA + cumsum(money.delta)
      returns <- c(0, diff(BALANCE.DELTA) / equity[-length(equity)])
      margin.used <- NET.VOLUME * margin.base
      list(intersection.time, money.delta, equity, returns, margin.used, equity - margin.used)
    }) %>%
    extract(
      j = TIME := time.numeric.to.posixct(TIME)
    ) %>%
    setkey(TIME) %>%
    list(
      timeseries.symbols = .,
      symbols.profit_volume = symbols.profit_volume
    )
} # FINISH

timeseries.one.ticket <- function(otime, ctime, nprofit, type, volume,
                                     oprice, digit, spread, symbol.price.data) {
  copy(symbol.price.data) %>%
    setkey(TIME) %>%
    extract(TIME >= ctime, PROFIT := nprofit) %>%
    extract(
      i = TIME >= otime & TIME < ctime,
      j = c('FLOATING', 'PL.VOLUME', 'VOLUME', 'MAX.FLOATING', 'MIN.FLOATING') := {
        digit.factor <- 10 ^ digit
        if (type == 'BUY') {
          pl.volume <- volume
          floating.pip <- (OPEN - oprice) * digit.factor
          max.floating.pip <- (HIGH - oprice) * digit.factor
          min.floating.pip <- (LOW - oprice) * digit.factor
        } else {
          pl.volume <- -volume
          floating.pip <- (oprice - OPEN) * digit.factor - spread
          max.floating.pip <- (oprice - LOW) * digit.factor - spread
          min.floating.pip <- (oprice - HIGH) * digit.factor - spread
        }
        list(cal.profit(volume, TICKVALUE, floating.pip), pl.volume, volume,
             cal.profit(volume, TICKVALUE, max.floating.pip),
             cal.profit(volume, TICKVALUE, min.floating.pip))
      }
    ) %>%
    extract(
      j = c('OPEN', 'HIGH', 'LOW', 'CLOSE', 'TICKVALUE') := NULL
    ) %>%
    setkey(TIME)
} # FINISH

#### MONEY ####
tickets.money <- function(tickets.supported, set.init.money=NULL, include.middle=TRUE, default.money=DEFAULT.INIT.MONEY) {
  first.trade.time <-
    tickets.supported %>%
    setkey(GROUP) %>%
    extract(!'MONEY', min(OTIME))
  init.part <-
    (if (!is.null(set.init.money)) {
      data.table(
        TICKET = 0,
        OTIME = first.trade.time - 1,
        PROFIT = set.init.money
      ) %>%
        build.tickets('MONEY')
    } else {
      tickets.money.raw <-
        tickets.supported['MONEY'] %>%
        extract(OTIME < first.trade.time, nomatch = 0)
      if (nrow(tickets.money.raw)) {
        tickets.money.raw
      } else {
        data.table(
          TICKET = 0,
          OTIME = first.trade.time - 1,
          PROFIT = default.money
        ) %>%
          build.tickets('MONEY')
      }
    })
  if (include.middle) {
    return(rbind(init.part, tickets.supported[GROUP == 'MONEY' & OTIME > first.trade.time, nomatch = 0], fill = TRUE))
  }
  init.part
}

money.delta <- function(tickets.money, time.vector) {
  serie <- vector('numeric', length(time.vector))
  mapply(function(time, value) {
    serie[which(time.vector > time)[1]] <<- value
  }, tickets.money[, 'OTIME'], tickets.money[, 'PROFIT'])
  serie
}

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
