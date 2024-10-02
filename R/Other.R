# Function to update progress messages in place
updateProgress <- function(message, maxLength = 80) {
  if (nchar(message) > maxLength) {
    # Truncate the message
    message <- substr(message, 1, maxLength)
  }
  paddedMessage <- sprintf("\r%-*s", maxLength, message)
  cat(paddedMessage)
  flush.console()
}


formatCountPercent <- function(count, percent, percentDigits = 1) {
  return(paste0(
    formatIntegerWithComma(count),
    " (",
    formatPercent(percent, digits = percentDigits),
    ")"
  ))
}

formatIntegerWithComma <- function(number) {
  return(formatC(number, format = "d", big.mark = ","))
}

formatPercent <- function(x,
                          digits = 2,
                          format = "f",
                          ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
