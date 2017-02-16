library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####


#### @PATCH NOTE@ ####
## 2017-02-16: Version 0.1


obtain.tickets.edited <- function(tickets.hypothesis) {
  tickets.hypothesis[]
} # ToDo ####

tickets.period <- function(tickets.edited) {
  period.from <- min(tickets.edited[, OTIME])
  period.to <- max(tickets.edited[, CTIME])
  period.vector <-
    format.time.numeric.to.posixct(c(period.from, period.to)) %>%
    as.Date
  trade.days <-
    seq.Date(from = period.vector[1], to = period.vector[2], by = 'day') %>%
    wday %>%
    setdiff(c(1, 7)) %>%
    length
  nature.interval <- interval(period.vector[1], period.vector[2])
  nature.period <- as.period(nature.interval)
  data.table(
    FROM = period.from,
    TO = period.to,
    NATURE.INTERVAL = nature.interval,
    NATURE.PERIOD = nature.period,
    TRADE.DAYS = trade.days
  )
}