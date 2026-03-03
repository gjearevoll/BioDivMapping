
removeAccents <- function(x) {
  x1 <- gsub("æ", "ae", x)
  x2 <- gsub("å", "aa", x1)
  x3 <- gsub("ø", "oe", x2)
  return(x3)
}
