\name{evaluate_local_independence}
\alias{evaluate_local_independence}
\title{Evaluate Local Independence in Item Response Models}

\description{
This function assesses local independence within an item response model, specifically designed for generalized partial credit models (GPCM). It computes the Q3 statistics to detect possible dependencies among items. The function calculates the difference between the maximum and the mean of non-diagonal Q3 residuals, determining whether this value exceeds a specified threshold, indicating potential violations of local independence.
}

\usage{
evaluate_local_independence(gpcm_01)
}

\arguments{
  \item{gpcm_01}{A GPCM model object from which the local independence will be evaluated.}
}

\details{
The function first calculates factor scores (theta scores) for the provided GPCM model. It then calculates Q3 residuals, using these theta scores, which represent the pairwise dependency statistics for each item pair. Non-diagonal elements of the Q3 matrix are extracted and assessed to produce a single statistic, Q3*. The function checks if Q3* is greater than 0.20, providing an indication of whether the local independence assumption holds or is violated in the model.
}

\value{
A list containing the value of Q3* and a message indicating whether the local independence principle is met or violated based on the threshold of 0.20. This output assists in diagnosing the fit and appropriateness of the model.
}

\examples{
# Assuming 'model_gpcm' is a fitted GPCM model
independence_result <- evaluate_local_independence(model_gpcm)
print(independence_result)
}

\author{
Dr. José Ventura-León
}
