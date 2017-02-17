
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