\name{fit_and_reliability_IRT}
\alias{fit_and_reliability_IRT}
\title{Fit and Evaluate Reliability of IRT Models}

\description{
This function evaluates both the fit and empirical reliability of a list of Item Response Theory (IRT) models. It integrates functionality to assess model fit via log-likelihood, AIC, and BIC metrics, and calculates empirical reliability coefficients for each model using expected a posteriori (EAP) scores. The function is structured to provide a comprehensive output combining both fit and reliability metrics for each model.
}

\usage{
fit_and_reliability_IRT(model_list, model_names, modelfit_list)
}

\arguments{
  \item{model_list}{A list of IRT model objects to be evaluated.}
  \item{model_names}{A vector of names corresponding to the models in `model_list`.}
  \item{modelfit_list}{A list containing model fit data for each model in `model_list`.}
}

\details{
The function operates in two main steps: it first computes fit metrics for each model, and then calculates the empirical reliability coefficient (Rxx) for each. The initial step involves creating a summary table of model fit statistics using internal functions, followed by another function that computes the reliability based on EAP scores. The results from both functions are then merged to provide a final dataframe containing comprehensive information about each model's performance.
}

\value{
A dataframe that combines the model fit statistics and empirical reliability coefficients for each model in the input lists. This output facilitates a detailed assessment of each model's effectiveness and reliability.
}

\examples{
modelfit_list <- list(modelfit_gpcm_01, modelfit_gpcm_02, modelfit_gpcm_03)
model_list <- list(gpcm_01, gpcm_02, gpcm_03)
model_names <- c("M1", "M2", "M3")

final_results_df <- fit_and_reliability_IRT(model_list, model_names, modelfit_list)
print(final_results_df)
}

\author{
Dr. José Ventura-León
}
