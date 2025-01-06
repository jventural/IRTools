\name{fit_and_reliability_IRT}
\alias{fit_and_reliability_IRT}
\title{Fit and Evaluate Reliability of IRT Models}

\description{
This function evaluates both the fit and empirical reliability of a list of Item Response Theory (IRT) models. It integrates functionality to assess model fit via log-likelihood, AIC, and BIC metrics, and calculates empirical reliability coefficients for each model using expected a posteriori (EAP) scores. Additionally, the function allows optional renaming of factor labels in bifactor or multidimensional models, making it highly customizable for different model structures. The function provides a comprehensive output combining both fit and reliability metrics for each model.
}

\usage{
fit_and_reliability_IRT(model_list, model_names, modelfit_list, rename_factors = NULL)
}

\arguments{
  \item{model_list}{A list of IRT model objects to be evaluated.}
  \item{model_names}{A vector of names corresponding to the models in `model_list`.}
  \item{modelfit_list}{A list containing model fit data for each model in `model_list`.}
  \item{rename_factors}{An optional named vector for renaming factor labels in models. For example, `c(G = "General", S1 = "Factor1", S2 = "Factor2")`. If `NULL`, no renaming is applied. This argument is particularly useful for bifactor or multidimensional models.}
}

\details{
The function operates in three main steps:
\itemize{
  \item First, it computes fit metrics (log-likelihood, AIC, BIC) for each model using an internal function.
  \item Second, it calculates the empirical reliability coefficient (Rxx) for each model based on EAP scores. If the `rename_factors` argument is provided, the factor labels (e.g., G, S1, S2) are renamed accordingly.
  \item Finally, the results from both steps are merged into a single dataframe containing comprehensive information about each model's performance.
}

The `rename_factors` argument allows for flexible renaming of factor labels in the output. For example, in a bifactor model, you can rename `G` (general factor) to "Dependencia" and `S1`, `S2`, etc., to specific labels like "Necesidad", "Autonomia", or "Abandono". This makes the output more interpretable and tailored to the user's specific research context.
}

\value{
A dataframe that combines the model fit statistics and empirical reliability coefficients for each model in the input lists. The factor labels in the reliability section of the output are adjusted if `rename_factors` is provided. This output facilitates a detailed assessment of each model's effectiveness and reliability.
}

\examples{
modelfit_list <- list(modelfit_gpcm_01, modelfit_gpcm_02, modelfit_gpcm_03)
model_list <- list(gpcm_01, gpcm_02, gpcm_03)
model_names <- c("M1", "M2", "M3")

# Without renaming factors
final_results_df <- fit_and_reliability_IRT(model_list, model_names, modelfit_list)
print(final_results_df)

# With renaming factors
rename_factors <- c(G = "Dependencia", S1 = "Necesidad", S2 = "Autonomia", S3 = "Abandono")
final_results_df <- fit_and_reliability_IRT(model_list, model_names, modelfit_list, rename_factors = rename_factors)
print(final_results_df)
}

\author{
Dr. José Ventura-León
}
