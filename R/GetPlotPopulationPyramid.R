#' Generate a Population Pyramid Plot
#'
#' This function generates a population pyramid plot based on the provided demographic data.
#' It displays the proportion of males and females across different age groups, with male
#' proportions displayed on the left (as negative values) and female proportions on the right.
#' The plot is flipped to create the pyramid style.
#'
#' @param data A tibble or data frame containing demographic information. It must have three
#' columns:
#' \describe{
#'   \item{ageGroup}{Character vector representing the age group stratification (e.g., "0-9", "10-19").}
#'   \item{gender}{Character vector representing the gender stratification ("Female" or "Male").}
#'   \item{proportion}{Numeric vector representing the proportion of the population in each age group and gender.}
#' }
#'
#' @return A ggplot object displaying the population pyramid. The x-axis represents the age groups,
#' and the y-axis represents the proportion of the population (with male proportions shown as negative
#' values to the left and female proportions as positive values to the right). The fill color differentiates
#' between genders, where males are represented in blue and females in pink.
#'
#' @details
#' The population pyramid visually represents the age and gender distribution of a population. It’s
#' a useful tool for understanding demographic structures. The function adjusts the male proportions
#' to be negative so that they appear on the left side of the plot, and it flips the coordinates to
#' achieve the pyramid-like shape.
#'
#' The function requires the `ggplot2` package to create the plot. If `ggplot2` is not installed, the
#' function will stop and prompt the user to install it.
#'
#' @importFrom ggplot2 ggplot geom_bar aes coord_flip scale_y_continuous labs scale_fill_manual theme_minimal
#' @export
getPlotPopulationPyramid <- function(data) {
  # Ensure ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the ggplot2 package to use this function.")
  }
  
  # Extract the start of the age range for sorting
  data$ageStart <- as.numeric(sub("-.*", "", data$ageGroup))
  
  # Order the data by the numeric start of the age group
  data <- data[order(data$ageStart), ]
  
  # Adjust proportions for plotting (make male proportions negative)
  data$proportion <- ifelse(data$gender == 'Male', -data$proportion, data$proportion)
  
  # Create the plot
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = stats::reorder(.data$ageGroup, .data$ageStart),
      y = .data$proportion,
      fill = .data$gender
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +  # Flip coordinates for pyramid style
    ggplot2::scale_y_continuous(labels = abs) +  # Show positive y-axis labels
    ggplot2::labs(title = "Population Pyramid", x = "Age Group", y = "Proportion of Population") +
    ggplot2::scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    ggplot2::theme_minimal()
  
  # Display the plot
  return(p)
}
