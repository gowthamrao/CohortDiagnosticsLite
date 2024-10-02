#' Retrieve Reference Population Demographics
#'
#' This function returns the age group and gender stratified demographic distribution
#' for various geographic regions. It provides the proportion of the population
#' in each age group, broken down by gender, for the selected region. The current
#' implementation includes data for the US, UK, Canada, Europe, Japan, China, and
#' a worldwide population.
#'
#' @param region A character string specifying the geographic region for which the
#' reference population should be returned. Valid options include "US", "UK",
#' "Canada", "Europe", "Japan", "China", and "Worldwide". The default is "US".
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{ageGroup}{Age group stratification (e.g., "0-9", "10-19").}
#'   \item{gender}{Gender stratification ("Female" or "Male").}
#'   \item{proportion}{Proportion of the population in that age group and gender.}
#' }
#'
#' @details
#' The proportions are approximations based on available demographic data for the
#' selected region. For more accurate and updated data, refer to official statistics
#' agencies such as the US Census Bureau, Eurostat, Statistics Canada, Japan's
#' Statistics Bureau, and China's National Bureau of Statistics.
#'
#' @export
getReferencePopulation <- function(region = "US") {
  if (region == "US") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.030,
        0.032,
        0.035,
        0.035,
        0.031,
        0.035,
        0.033,
        0.025,
        0.011,
        0.004,
        0.002,
        0.030,
        0.032,
        0.035,
        0.034,
        0.031,
        0.035,
        0.033,
        0.024,
        0.010,
        0.002,
        0.001
      )
    ) |> dplyr::tibble()
  } else if (region == "UK") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.028,
        0.030,
        0.034,
        0.033,
        0.030,
        0.034,
        0.031,
        0.026,
        0.012,
        0.005,
        0.001,
        0.029,
        0.031,
        0.034,
        0.033,
        0.030,
        0.034,
        0.031,
        0.024,
        0.012,
        0.004,
        0.001
      )
    ) |> dplyr::tibble()
  } else if (region == "Canada") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.029,
        0.031,
        0.033,
        0.032,
        0.029,
        0.033,
        0.030,
        0.027,
        0.013,
        0.006,
        0.002,
        0.030,
        0.032,
        0.033,
        0.032,
        0.030,
        0.033,
        0.030,
        0.025,
        0.013,
        0.005,
        0.001
      )
    ) |> dplyr::tibble()
  } else if (region == "Europe") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.025,
        0.027,
        0.032,
        0.032,
        0.029,
        0.032,
        0.030,
        0.028,
        0.015,
        0.007,
        0.002,
        0.026,
        0.028,
        0.032,
        0.032,
        0.029,
        0.032,
        0.030,
        0.027,
        0.014,
        0.006,
        0.002
      )
    ) |> dplyr::tibble()
  } else if (region == "Japan") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.019,
        0.022,
        0.027,
        0.026,
        0.024,
        0.029,
        0.030,
        0.031,
        0.018,
        0.007,
        0.002,
        0.020,
        0.023,
        0.028,
        0.026,
        0.024,
        0.028,
        0.030,
        0.030,
        0.017,
        0.006,
        0.001
      )
    ) |> dplyr::tibble()
  } else if (region == "China") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.025,
        0.027,
        0.032,
        0.032,
        0.031,
        0.033,
        0.030,
        0.020,
        0.010,
        0.004,
        0.001,
        0.025,
        0.027,
        0.032,
        0.031,
        0.031,
        0.033,
        0.030,
        0.019,
        0.009,
        0.003,
        0.001
      )
    ) |> dplyr::tibble()
  } else if (region == "Worldwide") {
    referencePopulation <- data.frame(
      ageGroup = c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80-89",
        "90-99",
        "100-109"
      ),
      gender = rep(c("Female", "Male"), each = 11),
      proportion = c(
        0.180,
        0.170,
        0.160,
        0.150,
        0.120,
        0.090,
        0.070,
        0.050,
        0.020,
        0.005,
        0.002,
        0.190,
        0.180,
        0.160,
        0.140,
        0.120,
        0.090,
        0.070,
        0.040,
        0.020,
        0.004,
        0.001
      )
    ) |> dplyr::tibble()
  } else {
    stop("Region not supported.")
  }

  return(referencePopulation)
}
