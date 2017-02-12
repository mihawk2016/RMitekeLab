
aaa <- new.env()

test.set <- function() {
  assign('x', list(1,2,3), envir = aaa)
}

test.assign <- function(value) {
  assign('x', value, envir = aaa)
}

test.get <- function() {
  get('x', envir = aaa)
}