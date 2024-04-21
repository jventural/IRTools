\name{value_test_information}
\alias{value_test_information}
\title{Calculate and Plot Test Information for a GPCM Model}

\description{
This function calculates test information for a Generalized Partial Credit Model (GPCM) across a specified range of theta values. It plots both the test information and its corresponding standard error, providing a visual representation of information efficiency across the theta continuum. Additionally, the function identifies and returns the maximum test information value and the corresponding data point.
}

\usage{
value_test_information(gpcm_03)
}

\arguments{
  \item{gpcm_03}{A GPCM model object from which test information will be computed.}
}

\details{
The function performs the following steps:
1. Computes the test information for theta values ranging from -3 to 3.
2. Calculates the standard error as the inverse of the square root of the test information.
3. Merges these values into a data frame and adjusts the column names.
4. Plots the test information and standard error using `ggplot2`.
5. Extracts and returns the maximum test information value along with its detailed data frame row.

This process facilitates a thorough understanding of the model's precision and efficiency across different levels of the trait being measured.
}

\value{
A list containing two elements:
1. `max_test_info_value` - the maximum value of test information.
2. `filtered_data` - a data frame with the row containing the maximum test information value, including theta and standard error.

This output is useful for identifying the range of theta with the highest information precision, assisting in the optimization of test design.
}

\examples{
# Assuming 'gpcm_03' is a fitted GPCM model object
info_results <- value_test_information(gpcm_03)
print(info_results$plot)  # To display the plot of test information
print(info_results$max_test_info_value)  # To see the maximum test information value
}

\author{
Dr. José Ventura-León
}
