
removeAccents <- function(x) {
  x1 <- gsub("æ", "ae", x)
  x2 <- gsub("å", "aa", x1)
  x3 <- gsub("ø", "oe", x2)
  x4 <- gsub("Å", "Aa", x3)
  x5 <- gsub("Ø", "Oe", x4)
  x6 <- gsub("Æ", "Ae", x5)
  return(x6)
}
