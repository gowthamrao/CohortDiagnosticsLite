# Function to update progress messages in place
updateProgress <- function(message, maxLength = 80) {
  paddedMessage <- sprintf("\r%-*s", maxLength, message)
  cat(paddedMessage)
  flush.console()
}
