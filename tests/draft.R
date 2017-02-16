# '' ' report functions' ''
# 2016-08-17: Create

#### Phase 1: Initialize ####

initialize.report <- function(report, default.currency = 'USD', default.leverage = 100, capital.initialize.setting, capital.hypothesis, capital.leverage, support.symbols.table = SYMBOLS, timeframe = 'M1', format.digit = 2, db = c('mysql.old', 'mysql.new', 'influxdb'),
                              score.return.table, score.maxdd.table, score.weight, score.level.table) {
  # ''' initialize report '''
  # 2016-08-17: TESTING
  within(report, {
    support.symbols <- rownames(support.symbols.table)
    Currency <- report.info.currency(Info, default.currency)
    Leverage <- report.info.leverage(Info, default.leverage)
    Item_Symbol.Mapping <- report.item_symbol.mapping(report.tickets.items(Tickets), support.symbols)
    Symbols.Table <- report.symbols.table(Item_Symbol.Mapping, default.currency, default.leverage, support.symbols.table)
    Tickets <- report.tickets.initialize(Tickets, Item_Symbol.Mapping, Symbols.Table, timeframe, format.digit, db)
    Trades <- report.trades(Tickets$Support)
    Parameter <- report.parameter(capital.initialize.setting, capital.hypothesis, capital.leverage, timeframe, format.digit, db, score.return.table, score.maxdd.table, score.weight, score.level.table)
    support.symbols <- NULL
  })
} # 2016-08-17: TESTING


report.trades <- function(support.tickets) {
  # ''' create trades (incl. open & closed tickets) '''
  # 2016-08-19: DONE
  list(
    Money = report.tickets.choose.group(support.tickets, 'MONEY'),
    # Before.Hypothesis = report.tickets.choose.group(support.tickets, 'CLOSED|OPEN')
    Before.Hypothesis = report.tickets.choose.group(support.tickets, 'CLOSED')
  )
} # 2016-08-19: DONE



#### Phase 2: Hypothesis ####


#### Phase 3: Without Price ####

report.trades.after.hypothesis <- function(hypothesis.trades, money.tickets, capital.hypothesis, capital.leverage, timeframe, symbols.table, format.digit = 2, db = c('mysql.old', 'mysql.new', 'influxdb')) {
  # ''' reform trades after hypothesis '''
  # 2016-08-19: Done
  ## JUST FOR MERGE 14 in 20161114 <
  # hypothesis.trades <- hypothesis.trades[hypothesis.trades$OTime >= as.POSIXct('2015-09-01', format = '%Y-%m-%d', tz='GMT') & hypothesis.trades$CTime <= as.POSIXct('2016-09-01', format = '%Y-%m-%d', tz='GMT'), ]
  ## JUST FOR MERGE 14 in 20161114 >
  trades.after.hypothesis <- within(hypothesis.trades, {
    DiffPrice <- ifelse(Type == 'BUY', CPrice - OPrice, OPrice - CPrice)
    DiffTime <- difftime(CTime, OTime, units = 'secs')
    ProfitP <- DiffPrice * 10 ^ symbols.table[Symbol, 'Digit'] 
    Profit <- round(calculate.tickvalue(Symbol, CTime, timeframe, symbols.table, db) * ProfitP * Volume, format.digit)
    NetProfit <- Commission + Taxes + Swap + Profit
    ProfitPerLot <- Profit / Volume
    Result <- ifelse(NetProfit >= 0, 'PROFIT', 'LOSS')
  })
  sorted.trades.after.hypothesis <- sort.dataframe(trades.after.hypothesis, 'CTime')
  real.return <- report.trades.after.hypothesis.return.real(sorted.trades.after.hypothesis, money.tickets)
  hypothesis.return <- report.trades.after.hypothesis.return.hypothesis(sorted.trades.after.hypothesis, capital.hypothesis)
  leverage.return <- report.trades.after.hypothesis.return.leverage(sorted.trades.after.hypothesis, capital.leverage)
  cbind(sorted.trades.after.hypothesis, ReturnR = real.return, ReturnH = hypothesis.return, ReturnL = leverage.return )
} # 2016-08-19: Done

report.trades.after.hypothesis.return.real <- function(trades.after.hypothesis, money.tickets) {
  # ''' calculate real return for trades after hypothesis '''
  # 2016-08-19: TESTING
  reformed.money.tickets <- report.trades.reform.money.tickets.for.calculate.returns(money.tickets)
  if (is.null(reformed.money.tickets)) return(NULL)
  bind.tickets <- rbind(reformed.money.tickets, subset(trades.after.hypothesis, select = c(CTime, NetProfit, Group)))
  ordered.bind.tickets <- sort.dataframe(bind.tickets, 'CTime')
  trade.index <- grep('CLOSED|OPEN', ordered.bind.tickets$Group)
  balance.serie <- cumsum(ordered.bind.tickets$NetProfit)
  simple.return.percents <- simple.return.percent(balance.serie)
  simple.return.percents[trade.index]
} # 2016-08-19: TESTING

report.trades.after.hypothesis.add.return.real <- function(trades.after.hypothesis, money.tickets) {
  # ''' add real returns to trades after hypothesis '''
  # 2016-08-20: Done
  trades.after.hypothesis <- sort.dataframe(trades.after.hypothesis, 'CTime')
  real.returns <- report.trades.after.hypothesis.return.real(trades.after.hypothesis, money.tickets)
  trades.after.hypothesis$ReturnR <- real.returns
  trades.after.hypothesis
} # 2016-08-20: Done

report.trades.after.hypothesis.return.hypothesis <- function(trades.after.hypothesis, capital.hypothesis) {
  # ''' calculate hypothesis return for trades after hypothesis '''
  # 2016-08-19: TESTING
  balance.serie <- cumsum(c(capital.hypothesis, trades.after.hypothesis$NetProfit))
  simple.returns <- simple.return.percent(balance.serie)
  simple.returns[-1]
} # 2016-08-19: TESTING

report.trades.after.hypothesis.add.return.hypothesis <- function(trades.after.hypothesis, capital.hypothesis) {
  # ''' add hypothesis returns to trades after hypothesis '''
  # 2016-08-20: Done
  trades.after.hypothesis <- sort.dataframe(trades.after.hypothesis, 'CTime')
  hypothesis.returns <- report.trades.after.hypothesis.return.hypothesis(trades.after.hypothesis, capital.hypothesis)
  trades.after.hypothesis$ReturnH <- hypothesis.returns
  trades.after.hypothesis
} # 2016-08-20: Done

report.trades.after.hypothesis.return.leverage <- function(trades.after.hypothesis, capital.leverage) {
  # ''' calculate leverage return for trades after hypothesis '''
  # 2016-08-19: TESTING
  with(trades.after.hypothesis, {
    DiffPrice * capital.leverage / OPrice
  })
} # 2016-08-19: TESTING

report.trades.after.hypothesis.add.return.leverage <- function(trades.after.hypothesis, capital.leverage) {
  # ''' add leverage returns to trades after hypothesis '''
  # 2016-08-20: Done
  print(trades.after.hypothesis)
  trades.after.hypothesis <- sort.dataframe(trades.after.hypothesis, 'CTime')
  leverage.returns <- report.trades.after.hypothesis.return.leverage(trades.after.hypothesis, capital.leverage)
  trades.after.hypothesis$ReturnL <- leverage.returns
  trades.after.hypothesis
} # 2016-08-20: Done

report.trades.reform.money.tickets.for.calculate.returns <- function(money.tickets) {
  # ''' reform money tickets, for calculate returns '''
  # 2016-08-19: TESTING
  if (is.null(money.tickets)) return(NULL)
  sub.money <- subset(money.tickets, select = c(OTime, Profit, Group))
  colnames(sub.money)[1:2] <- c('CTime', 'NetProfit')
  sub.money
} # 2016-08-19: TESTING



report.parameter.capital.initialize <- function(period, trades.money) {
  # ''' get initialize capital for mdd calculate '''
  # 2016-08-25: TESTING
  begin <- period$From
  capital <- sum(trades.money$Profit[trades.money$OTime < begin])
} # 2016-08-25: TESTING

report.statistics <- function(trades, money.tickets, capital.hypothesis, capital.leverage) {
  # ''' get statistics for trades '''
  # 2016-08-21: Working
  list(
    Portfolio = trades.statistics(trades, money.tickets, capital.hypothesis, capital.leverage),
    Symbol = lapply(split(trades, trades$Symbol), trades.statistics, money.tickets, capital.hypothesis, capital.leverage)
  )
} # 2016-08-21: Working

report.statistics.add.return_sharpe.hypotheis <- function(statistics, trades, capital.hypothesis) {
  # ''' add hypothesis return & sharpe to statistics '''
  # 2016-08-22: TESTING
  within(statistics, {
    Portfolio <- trades.statistics.add.return_sharpe.hypotheis(Portfolio, trades, capital.hypothesis)
    Symbol <- lapply(Symbol, trades.statistics.add.return_sharpe.hypotheis, trades, capital.hypothesis)
  })
} # 2016-08-22: TESTING

report.statistics.add.return_sharpe.leverage <- function(statistics, trades, capital.leverage) {
  # ''' add leverage return & sharpe to statistics '''
  # 2016-08-22: TESTING
  within(statistics, {
    Portfolio <- trades.statistics.add.return_sharpe.leverage(Portfolio, trades, capital.leverage)
    Symbol <- lapply(Symbol, trades.statistics.add.return_sharpe.hypotheis, trades, capital.leverage)
  })
} # 2016-08-22: TESTING

trades.statistics <- function(trades, money.tickets, capital.hypothesis, capital.leverage) {
  # ''' get statistics for trades '''
  # 2016-08-22: Working
  result <- trades.statistics.result(trades)
  summary <- trades.statistics.summary(result)
  list(
    Result = result,
    Type = trades.statistics.type(trades),
    Exit = trades.statistics.exit(trades),
    Return_Sharpe = cbind(trades.statistics.return_sharpe.real(trades, money.tickets),
                          trades.statistics.return_sharpe.hypothesis(trades, capital.hypothesis),
                          trades.statistics.return_sharpe.leverage(trades, capital.leverage)),
    Summary = summary
  )
} # 2016-08-22: Working

trades.statistics.summary <- function(result) {
  # ''' get statistics summary '''
  # 2016-08-25
  trades.total <- sum(result[, 1])
  profit.total <- sum(result[, 2])
  volume.total <- sum(result[, 8])
  data.frame(
    stringsAsFactors = F,
    row.names = 'Summary',
    TradesTotal = trades.total,
    ProfitTotal = profit.total,
    ProfitPTotal = sum(result[, 5]),
    VolumeTotal = volume.total,
    WinPercent = result[1, 1] / trades.total * 100,
    ProfitFactor = result[1, 2] / -result[2, 2],
    Expect = profit.total / trades.total,
    VolumePerTrade = volume.total / trades.total,
    ProfitPerLot = profit.total / volume.total
  )
} # 2016-08-25

trades.statistics.add.return_sharpe.hypotheis <- function(statistics, trades, capital.hypothesis) {
  # ''' add hypothesis return & sharpe to statistics '''
  # 2016-08-22: TESTING
  within(statistics, {
    Return_Sharpe[, c('ReturnH', 'SharpeH')] <- trades.statistics.return_sharpe.hypothesis(trades, capital.hypothesis)
  })
} # 2016-08-22: TESTING

trades.statistics.add.return_sharpe.leverage <- function(statistics, trades, capital.leverage) {
  # ''' add leverage return & sharpe to statistics '''
  # 2016-08-22: TESTING
  within(statistics, {
    Return_Sharpe[, c('ReturnL', 'SharpeL')] <- trades.statistics.return_sharpe.leverage(trades, capital.leverage)
  })
} # 2016-08-22: TESTING

trades.statistics.type <- function(trades) {
  # ''' split type, trades statistics '''
  # 2016-08-22: TESTING
  type <- trades$Type
  all.types <- c('BUY', 'SELL')
  volume.list <- tapply(trades$Volume, type, function(group.type) {
    c(TradesTotal = length(group.type), VolumeTotal = sum(group.type))
  })
  other.type.names <- setdiff(all.types, names(volume.list))
  if (length(other.type.names) > 0) {
    volume.list[other.type.names] <- 0
  }
  as.data.frame(do.call(rbind, volume.list)[all.types, ])
} # 2016-08-22: TESTING

trades.statistics.exit <- function(trades) {
  # ''' split type, trades statistics '''
  # 2016-08-22: TESTING
  exit <- trades$Comment
  all.types <- c('SO', 'SL', 'TP')
  exit.list <- tapply(trades$NetProfit, exit, function(group.type) c(TradesTotal = length(group.type), ProfitTotal = sum(group.type)))
  other.type.names <- setdiff(all.types, names(exit.list))
  if (length(other.type.names) > 0) {
    exit.list[other.type.names] <- 0
  }
  do.call(rbind, exit.list)[all.types, ]
} # 2016-08-22: TESTING

trades.statistics.result <- function(trades) {
  # ''' split Result, trades statistics '''
  # 2016-08-22: TESTING
  result <- trades$Result
  all.types <- c('PROFIT', 'LOSS')
  profit.list <- tapply(trades$Profit, result, function(group.result) {
    c(TradesTotal = length(group.result), Total = sum(group.result), Mean = mean(group.result), Max = max(abs(group.result)))
  })
  profitp.list <- tapply(trades$ProfitP, result, function(group.result) {
    c(Total = sum(group.result), Mean = mean(group.result), Max = max(abs(group.result)))
  })
  volume.list <- tapply(trades$Volume, result, function(group.result) {
    c(Total = sum(group.result), Mean = mean(group.result), Max = max(group.result), Min = min(group.result))
  })
  other.type.names <- setdiff(all.types, names(profit.list))
  if (length(other.type.names) > 0) {
    profit.list[other.type.names] <- 0
    profitp.list[other.type.names] <- 0
    volume.list[other.type.names] <- 0
  }  
  profit.data.frame <- do.call(rbind, profit.list)
  profit.data.frame['LOSS', 'Max'] <- -profit.data.frame['LOSS', 'Max']
  profitp.data.frame <- do.call(rbind, profitp.list)
  profitp.data.frame['LOSS', 'Max'] <- -profitp.data.frame['LOSS', 'Max']
  volume.data.frame <- do.call(rbind, volume.list)
  net.profits <- trades$NetProfit
  continuous <- continuous.profit_loss(net.profits)
  continuous.data.frame <- with(continuous, {
    data.frame(
      row.names = all.types,
      MaxTrades = c(max(UpCon), max(DownCon)),
      MeanTrades = c(mean(UpCon), mean(DownCon)),
      MaxProfit = c(max(mapply(function(begin.index, end.index) sum(net.profits[begin.index:end.index]), UpFrom, UpFrom + UpCon - 1)),
                    min(mapply(function(begin.index, end.index) sum(net.profits[begin.index:end.index]), DownFrom, DownFrom + DownCon - 1)))
    )
  })
  all.data.frame <- list(
    Profit = profit.data.frame[all.types, ],
    ProfitP = profitp.data.frame[all.types, ],
    Volume = volume.data.frame[all.types, ],
    Continuous = continuous.data.frame[all.types, ]
  )
  do.call(cbind, all.data.frame)
} # 2016-08-22: TESTING

trades.statistics.return.real <- function(trades, money.tickets) {
  # ''' trades real return '''
  # 2016-08-22: TESTING
  reformed.money.tickets <- report.trades.reform.money.tickets.for.calculate.returns(money.tickets)
  if (is.null(reformed.money.tickets)) return(NULL)
  bind.tickets <- rbind(reformed.money.tickets, subset(trades, select = c(CTime, NetProfit, Group)))
  ordered.bind.tickets <- sort.dataframe(bind.tickets, 'CTime')
  balance.serie <- cumsum(ordered.bind.tickets$NetProfit)
  sign.groups <- c(sign(ordered.bind.tickets$Group == 'MONEY'), 1)
  signs <- diff(sign.groups)
  last.money.index <- which(signs == -1)
  last.trade.index <- which(signs == 1)
  returns <- balance.serie[last.trade.index] / balance.serie[last.money.index]
  prod(returns) - 1
} # 2016-08-22: TESTING

trades.statistics.return_sharpe.real <- function(trades, money.tickets) {
  # ''' trades real return & sharpe '''
  # 2016-08-22: TESTING
  data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    ReturnR = trades.statistics.return.real(trades, money.tickets),
    SharpeR = sharpe.ratio(report.trades.after.hypothesis.return.real(trades, money.tickets))
  )
} # 2016-08-22: TESTING

trades.statistics.return.hypothesis <- function(trades, capital.hypothesis) {
  # ''' trades hypothesis return '''
  # 2016-08-22: TESTING
  gain <- sum(trades$NetProfit)
  gain / capital.hypothesis
} # 2016-08-22: TESTING

trades.statistics.return_sharpe.hypothesis <- function(trades, capital.hypothesis) {
  # ''' trades hypothesis return & sharpe '''
  # 2016-08-22: TESTING
  data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    ReturnH = trades.statistics.return.hypothesis(trades, capital.hypothesis),
    SharpeH = sharpe.ratio(report.trades.after.hypothesis.return.hypothesis(trades, capital.hypothesis))
  )
} # 2016-08-22: TESTING

trades.statistics.return.leverage <- function(trades, capital.leverage) {
  # ''' trades leverage return '''
  # 2016-08-22: TESTING
  sum(trades$ReturnL)
} # 2016-08-22: TESTING

trades.statistics.return_sharpe.leverage <- function(trades, capital.leverage) {
  # ''' trades leverage return & sharpe '''
  # 2016-08-22: TESTING
  data.frame(
    stringsAsFactors = F,
    row.names = NULL,
    ReturnL = trades.statistics.return.leverage(trades, capital.leverage),
    SharpeL = sharpe.ratio(trades$ReturnL)
  )
} # 2016-08-22: TESTING

#### Phase 4: With Price ####

timeseries.from.trades.after.hypothesis <- function(trades, prices, timeframe, support.symbols.table, db) {
  # ''' get all need timeseries from trades after hypothesis '''
  # 2016-08-24: Working
  split.by.symbol <- split(trades, trades$Symbol)
  timeseries.symbols_trades <- lapply(split.by.symbol, function(symbol.trades) {
    price <- prices[[symbol.trades$Symbol[1]]]
    trades <- timeseries.trades.from.symbol.trades(symbol.trades, price, timeframe, support.symbols.table, db)
    symbol <- timeseries.symbols.from.timeseries.trades(trades)
    list(
      Symbol = symbol,
      Trades = trades
    )
  })
  timeseries.portfolio <- timeseries.portfolio.from.timeseries.symbols_trades(timeseries.symbols_trades)
  list(
    Portfolio = timeseries.portfolio,
    Symbol = timeseries.symbols_trades
  )
} # 2016-08-24: Working

timeseries.portfolio.from.timeseries.symbols_trades <- function(timeseries.symbols_trades) {
  # ''' get report timeseries '''
  # 2016-08-24: TESTING
  portfolio.serie.table <- 0
  lapply(timeseries.symbols_trades, function(symbols_trades) {
    portfolio.serie.table <<- portfolio.serie.table + symbols_trades$Symbol
  })
  portfolio.serie.table
} # 2016-08-24: TESTING

timeseries.symbols.from.timeseries.trades <- function(timeseries.trades) {
  # ''' get symbol series from trades series '''
  # 2016-08-24: TESTING
  symbol.serie.table <- 0
  lapply(timeseries.trades, function(serie.trade) {
    symbol.serie.table <<- symbol.serie.table + serie.trade
  })
  symbol.serie.table$Profit <- cumsum(symbol.serie.table$Profit)
  colnames(symbol.serie.table)[1:3] <- c('Balance.Delta', 'NetVolume', 'TotalVolume')
  symbol.serie.table
} # 2016-08-24: TESTING

timeseries.trades.from.symbol.trades <- function(symbol.trades, price, timeframe, support.symbols.table, db) {
  # ''' get all need timeseries from symbol trades '''
  # 2016-08-24: TESTING
  require(xts)
  from.string <- as.character(reform.time(min(symbol.trades$OTime)))
  to.string <- as.character(reform.time(max(symbol.trades$CTime)))
  symbol <- symbol.trades$Symbol[1]
  price.with.tickvalue <- timeseries.tickvalues.for.symbol.trades(symbol.trades, price, from.string, to.string, timeframe, support.symbols.table, db)
  timeseries <- xts(matrix(0, nrow = nrow(price), ncol = 6, 
                           dimnames = list(NULL, c('Profit', 'D.Volume', 'Volume', 'Floating', 'Max.Floating', 'Min.Floating'))),
                    index(price))
  trades.timeseries <- lapply(1:nrow(symbol.trades), function(symbol.trade.index) {
    timeseries.from.symbol.trade(symbol.trades[symbol.trade.index, ], timeseries, price.with.tickvalue, support.symbols.table)
  })
  names(trades.timeseries) <- paste0('T', symbol.trades$Ticket)
  trades.timeseries
} # 2016-08-24: TESTING

timeseries.tickvalues.for.symbol.trades <- function(symbol.trades, price, from.string, to.string, timeframe, support.symbols.table, db) {
  # ''' get tickvalues for symbol trades '''
  # 2016-08-24: TESTING
  require(xts)
  price.with.tickvalues.xts <- cbind(price, TickValue = NA)
  period.string <- paste(from.string, to.string, sep = '/')
  tickvalues <- calculate.tickvalue(symbols = symbol.trades$Symbol[1],
                                    time = index(price[period.string]),
                                    timeframe = timeframe,
                                    support.symbols.table = support.symbols.table,
                                    db = db)
  price.with.tickvalues.xts$TickValue[period.string] <- tickvalues
  price.with.tickvalues.xts
} # 2016-08-24: TESTING

timeseries.from.symbol.trade <- function(symbol.trade, timeseries, price.with.tickvalue, support.symbols.table) {
  # ''' get all need timeseries from one symbol trade '''
  # 2016-08-24: Working
  from.string <- as.character(symbol.trade$OTime)
  to.string <- as.character(symbol.trade$CTime)
  index.last.string <- paste0(to.string, '/')
  index.floating.string <- paste(from.string, to.string, sep = '/')
  timeseries[index.last.string]$Profit[1] <- symbol.trade$NetProfit
  floating.period <- timeseries[index.floating.string]
  if (nrow(floating.period) > 0) {
    symbol <- symbol.trade$Symbol
    volume <- symbol.trade$Volume
    type <- symbol.trade$Type
    open.price <- symbol.trade$OPrice
    spread <- support.symbols.table[symbol, 'Spread']
    digit.factor <- 10 ^ support.symbols.table[symbol, 'Digit']
    tickvalue.serie <- price.with.tickvalue$TickValue[index.floating.string]
    if (type == 'BUY') {
      direction.volume <- volume
      floating.pips <- (price.with.tickvalue$Open[index.floating.string] - open.price) * digit.factor
      max.floating.pips <- (price.with.tickvalue$High[index.floating.string] - open.price) * digit.factor
      min.floating.pips <- (price.with.tickvalue$Low[index.floating.string] - open.price) * digit.factor
    } else {
      direction.volume <- -volume
      floating.pips <- (open.price - price.with.tickvalue$Open[index.floating.string]) * digit.factor - spread
      max.floating.pips <- (open.price - price.with.tickvalue$Low[index.floating.string]) * digit.factor - spread
      min.floating.pips <- (open.price - price.with.tickvalue$High[index.floating.string]) * digit.factor - spread
    }
    timeseries$D.Volume[index.floating.string] <- direction.volume
    timeseries$Volume[index.floating.string] <- volume
    # print(tickvalue.serie)
    # print(floating.pips)
    # if (length(floating.pips) == 0)
    #   floating.pips <- 0
    timeseries$Floating[index.floating.string] <- floating.pips * volume * tickvalue.serie
    timeseries$Max.Floating[index.floating.string] <- max.floating.pips * volume * tickvalue.serie
    timeseries$Min.Floating[index.floating.string] <- min.floating.pips * volume * tickvalue.serie
  }
  timeseries
} # 2016-08-24: Working

#### Symbol Utils ####

#### TickValue ####

check.overnight.for.swap <- function(close.time, open.time) {
  # ''' check or over night '''
  # 2016-08-15: Done
  over.one.day <- difftime(close.time, open.time, units = 'days') >= 1
  not.one.day <- as.POSIXlt(close.time)$mday != as.POSIXlt(open.time)$mday
  check <- over.one.day | not.one.day
  check[is.na(close.time)] <- F
  check
} # 2016-08-15: Done

#### Calculate Utils ####

continuous.profit_loss <- function(x) {
  # ''' calculate continuous profits and losses '''
  # 2016-08-19: Optimize
  len <- length(x)
  if(len == 0) return(list(UpFrom = 0, UpCon = 0, DownFrom = 0, DownCon = 0))
  signs <- sign(x)
  if(all(signs == 1)) return(list(UpFrom = 1, UpCon = len, DownFrom = 0, DownCon = 0))
  if(all(signs == -1)) return(list(UpFrom = 0, UpCon = 0, DownFrom = 1, DownCon = len))
  diff.signs <- diff(signs)
  turns <- c(signs[1], diff.signs)
  down.begin <- which(turns < 0)
  up.begin <- which(turns > 0)
  suppressWarnings(
    if(down.begin[1] > up.begin[1]) {
      up.con <- down.begin - up.begin
      down.con <- up.begin[-1] - down.begin
    } else {
      down.con <- up.begin - down.begin
      up.con <- down.begin[-1] - up.begin
    }
  )
  last.con <- len - tail(which(turns != 0), 1) + 1
  if(length(up.con) == 0) up.con <- last.con
  if(length(down.con) == 0) down.con <- last.con
  up.con[up.con < 0] <- last.con
  down.con[down.con < 0] <- last.con
  list(
    UpFrom = up.begin,
    UpCon = up.con,
    DownFrom = down.begin,
    DownCon = down.con
  )
} # 2016-08-19: Optimize

sharpe.ratio <- function(x) {
  # ''' calculate sharpe ratio '''
  # 2016-08-19: Done
  mean(x) / sd(x)
} # 2016-08-19: Done

simple.return.percent <- function(x) {
  # ''' calculate simple return in percent '''
  # 2016-08-19: Done
  c(0, diff(x) / x[-length(x)] * 100)
} # 2016-08-19: Done

maxdrawdown <- function(x) {
  # ''' calculate max drawdown '''
  # 2016-08-19: Optimize
  x <- na.fill(x, 'extend')
  max <- max(x)
  min <- min(x)
  max.index <- which.max(x)
  min.index <- which.min(x)
  diff <- max - min
  if (min.index > max.index) return(data.frame(MDD = diff, MDDP = diff / max * 100, MaxIndex = max.index, MinIndex = min.index))
  x.diff <- diff(x)
  sign <- sign(x.diff)
  sign <- c(-sign[1], sign)
  topbottons <- -diff(sign)
  topbottons <- c(topbottons, tail(sign, 1)) ## all turns
  ## top
  tops.index <- which(topbottons > 0)
  tops <- x[tops.index]
  top.df <- data.frame(Top = tops, Index = tops.index)
  top.order <- order(top.df$Top, decreasing = TRUE)
  top.df <- top.df[top.order, ]
  ## bottom
  bottoms.index <- which(topbottons < 0)
  bottoms <- x[bottoms.index]
  bottom.df <- data.frame(Bottom = bottoms, Index = bottoms.index)
  bottom.order <- order(bottom.df$Bottom)
  bottom.df <- bottom.df[bottom.order, ]
  max.dd <- 0
  top <- NA
  while (nrow(top.df) > 0 & nrow(bottom.df) > 0) {
    topfirst <- top.df$Index[1]
    bestbottom <- which(bottom.df$Index > topfirst)[1]
    if(!is.na(bestbottom)) {
      dd <- top.df$Top[1] - bottom.df$Bottom[bestbottom]
      if(dd > max.dd) {
        max.dd <- dd
        top <- top.df[1, ]
        bottom <- bottom.df[bestbottom, ]
      }
      bottom.df <- bottom.df[1:(bestbottom - 1), ]
    }
    bottomfirst <- bottom.df$Index[1]
    besttop <- which(top.df$Index < bottomfirst)[1]
    if(!is.na(besttop)) {
      dd <- top.df$Top[besttop] - bottom.df$Bottom[1]
      if(dd > max.dd) {
        max.dd <- dd
        top <- top.df[besttop, ]
        bottom <- bottom.df[1, ]
      }
      top.df <- top.df[1:(besttop - 1), ]
    }
    if(is.na(top[1])) return(data.frame(MDD = diff, MDDP = Inf, MaxIndex = 1, MinIndex = 1))
    top.df <- top.df[-1, ]
    bottom.df <- bottom.df[-1, ]
  }
  if (is.na(top)) return(data.frame(MDD = 0, MDDP = 0, MaxIndex = max.index, MinIndex = min.index))
  max <- top$Top
  max.index <- top$Index
  min.index <- bottom$Index
  data.frame(MDD = max.dd, MDDP = max.dd / max * 100, MaxIndex = max.index, MinIndex = min.index)
} # 2016-08-19: Optimize

#### Other Utils ####

format.difftime <- function(x) {
  if (length(x) > 1) {
    return(sapply(x, format.difftime))
  }
  if(is.na(x) | x <0) return('')
  str <- as.character(as.POSIXlt(x, origin = '1970-01-01', tz = 'GMT'))
  str <- substr(str, 12, 20)
  if(x >= 86400) str <- paste0(x%/%86400, 'D ', str)
  str
}