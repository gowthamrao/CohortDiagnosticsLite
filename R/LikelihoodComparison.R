#' Compare Likelihoods in a Data Frame
#'
#' This function compares the likelihoods of observed and expected counts in a data frame and calculates the log-likelihood ratio.
#'
#' @param data A data frame that contains the columns 'observed' and 'expected'.
#' @param maxRatio The maximum ratio for the likelihood comparison. If NULL, the default is 1.25.
#' @param alpha The significance level for the likelihood ratio test. If NULL, the default is 0.05.
#' @return A data frame with the ratio, p-value, and stability indicator.
likelihoodComparison <- function(data,
                                 maxRatio = 1.25,
                                 alpha = 0.05) {
  observed <- data$observed
  expected <- data$cyclopsExpected

  # From https://cdsmithus.medium.com/the-logarithm-of-a-sum-69dd76199790
  smoothMax <- function(x, y) {
    smoothMax <- ifelse(
      test = abs(x - y) > 100,
      yes = pmax(x, y),
      no = x + log(1 + exp(y - x))
    )
    return(smoothMax)
  }

  logLikelihood <- function(x) {
    xTerm <- stats::dpois(observed, expected * x, log = TRUE)
    yTerm <- stats::dpois(observed, expected / x, log = TRUE)
    return(-sum(smoothMax(x = xTerm, y = yTerm)))
  }

  likelihood <- function(x) {
    return(exp(-logLikelihood(x)))
  }

  vectorLikelihood <- function(x) {
    # Handle cases where likelihood might become extremely small or infinite
    likelihood_values <- sapply(x, likelihood)
    likelihood_values[is.infinite(likelihood_values) |
      likelihood_values < .Machine$double.eps] <- .Machine$double.eps
    return(likelihood_values)
  }

  x <- seq(1, 10, by = 0.1)
  ll <- sapply(x, logLikelihood)

  # Handle the case when indices is empty by setting to NA
  maxX <- NA
  minX <- NA

  indices <- which(!is.na(ll) & !is.infinite(ll))
  if (length(indices) > 0) {
    maxX <- x[max(indices)]
    minX <- x[min(indices)]
  }

  xHat <- stats::optim(
    1.5,
    logLikelihood,
    lower = minX,
    upper = maxX,
    method = "L-BFGS-B"
  )$par

  # Handle potential divergent integrals by introducing lower and upper bounds on the integrand
  l0 <- tryCatch(
    {
      stats::integrate(vectorLikelihood, lower = 1, upper = maxRatio)$value
    },
    error = function(...) {
      NA
    }
  ) # Return NA if the integral fails

  l1 <- tryCatch(
    {
      stats::integrate(vectorLikelihood, lower = maxRatio, upper = Inf)$value
    },
    error = function(...) {
      NA
    }
  ) # Return NA if the integral fails

  if (is.na(l0) || is.na(l1)) {
    llr <- NA
    p <- NA
  } else {
    llr <- 2 * (log(l1) - log(l0))
    p <- ifelse(
      is.nan(llr),
      ifelse(xHat > maxRatio, 0, 1),
      stats::pchisq(llr, 1, lower.tail = FALSE)
    )
  }

  result <- data |>
    tidyr::crossing(dplyr::tibble(
      ratio = xHat,
      p = p,
      stable = p > alpha
    ))

  return(result)
}
