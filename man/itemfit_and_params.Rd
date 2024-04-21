\name{itemfit_and_params}
\alias{itemfit_and_params}
\title{Combine Item Fit Statistics and Parameter Estimates for IRT Models}

\description{
This function merges item fit statistics with parameter estimates from a generalized partial credit model (GPCM). It cleans and restructures the combined data by removing and relocating specific columns to create a more concise and informative tibble. This allows for a streamlined examination of both fit and parameter data, facilitating a clearer understanding of the model's performance.
}

\usage{
itemfit_and_params(itemfit_gpcm_03, gpcm_params_03)
}

\arguments{
  \item{itemfit_gpcm_03}{A dataframe containing item fit statistics for a GPCM model.}
  \item{gpcm_params_03}{A dataframe containing parameter estimates for the same GPCM model.}
}

\details{
The function utilizes the `dplyr` and `tibble` packages to perform data manipulation tasks. It first binds the two provided dataframes column-wise, then converts the result into a tibble for easier manipulation. It removes certain less relevant columns such as item fit diagnostics like Zh, S_X2, and df.S_X2, and relocates important RMSEA and p-values to a more prominent position immediately after the main parameter estimates. This restructured output is more intuitive and emphasizes key metrics for model evaluation.
}

\value{
A tibble that integrates cleaned and restructured item fit statistics with parameter estimates, making key information readily accessible and enhancing the ease of analysis.
}

\examples{
# Assuming 'itemfit_gpcm_03' contains item fit statistics and 'gpcm_params_03' contains parameter estimates for the same model
combined_results_df <- itemfit_and_params(itemfit_gpcm_03, gpcm_params_03)
print(combined_results_df)
}

\author{
Dr. José Ventura-León
}
