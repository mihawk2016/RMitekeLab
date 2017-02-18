




report.statistics <- function(trades, money.tickets, capital.hypothesis, capital.leverage) {
  # ''' get statistics for trades '''
  # 2016-08-21: Working
  list(
    Portfolio = trades.statistics(trades, money.tickets, capital.hypothesis, capital.leverage),
    Symbol = lapply(split(trades, trades$Symbol), trades.statistics, money.tickets, capital.hypothesis, capital.leverage)
  )
} # 2016-08-21: Working


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