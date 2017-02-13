






mapping <-
  1:10 %>%
  set_names(LETTERS[1:10])


vec <- c(rep('B', 5), rep('C', 5))

func <- function(x) {
  print('h')
  mapping[x]
}

print(func(vec))
