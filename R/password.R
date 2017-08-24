

generatePassword <- function(nchars = 10) {
  sym <- ",./;'[]<>?:{}!@#$%^&*()"
  sym <- unlist(strsplit(sym, split = ""))
  # Make sure we get at least one of each...
  chars <- c(sample(letters, 1), sample(LETTERS, 1), sample(sym, 1))
  moreChars <- sample(c(letters, LETTERS, sym), nchars - 3)
  pwd <- c(chars, moreChars)
  pwd <- sample(pwd)
  paste(pwd, collapse = "")
}
generatePassword(10)
