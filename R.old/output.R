library(compiler)
compilePKGS(T)

#### @UPDATE IDEA@ ####
## 2017-02-17: 

#### @PATCH NOTE@ ####
## 2017-02-21: Version 0.1


#### REPORT ####
output.report <- function(infos, file.type='HTML') {
  
}

output.report.html <- function(markdown, infos) {
  windowsFonts(CON = windowsFont("Consolas"))
  
  
  output.report.html.render(markdown, output.file.name(infos, 'REPORT'), environment())
}

output.report.html.render <- function(markdown, file.name, envir) {
  # ''' report output file '''
  # 2017-01-30: Version 1.0
  # markdown <- ifelse(comment, './Markdowns/Output.Comment.Rmd', './Markdowns/Output.Rmd')
  # file.name <- paste0(.output.report.name(infos), '.html')
  Sys.setlocale(locale = 'us')
  render(markdown, html_document(), output_file = file.name, quiet = T, envir = envir)
  Sys.setlocale(locale = 'Chinese')
} # FINISH




#### + REPORT INFOS ####
output.report.html.infos <- function(infos) {
  # ''' infos for output '''
  # 2016-08-11: Version 1.0
  infos[, TIME := time.numeric.to.posixct(TIME)] %>%
    htmlTable(css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
              rnames = F)
} # FINISH






#### TICKETS ####
output.tickets <- function(tickets, groups, columns, file.name) {
  tickets %>%
    setkey(GROUP) %>%
    extract(
      j = c('OTIME', 'CTIME') := list(time.numeric.to.posixct(OTIME), time.numeric.to.posixct(CTIME))
    ) %>%
    extract(
      i = groups,
      j = c('TICKET', 'OTIME', 'TYPE', 'VOLUME', 'ITEM', 'OPRICE', 'SL', 'TP',
            'CTIME', 'CPRICE', 'COMMISSION', 'TAXES', 'SWAP', 'PROFIT', columns)
    ) %>%
    write.csv(file = file.name)
} #


#### UTILS ####
output.file.name <- function(infos, type=c('REPORT', 'TICKETS')) {
  if (nrow(infos) == 1) {
    infos[, FILE]
  } else {
    'FileSet'
  } %>%
    sprintf('%s-%s', substr(type, 1, 1), .)
}

