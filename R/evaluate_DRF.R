evaluate_DRF <- function(data, items, balance = TRUE, model_spec, group_col, itemtype = "graded") {
  library(dplyr)
  library(PsyMetricTools) # Ensure PsyMetricTools is installed for smote_multiclass
  library(mirt)           # Ensure mirt is installed for modeling

  # Function to format DIF results dynamically
  format_dif_results <- function(data, dif_type) {
    # Identify the key columns dynamically
    dif_col <- grep("DIF", names(data), value = TRUE)
    ci_lower_col <- grep("CI_2.5", names(data), value = TRUE)
    ci_upper_col <- grep("CI_97.5", names(data), value = TRUE)

    # Check if the necessary columns are present
    if (length(dif_col) == 0 || length(ci_lower_col) == 0 || length(ci_upper_col) == 0) {
      stop("Data must contain columns for DIF and its 95% CI (CI_2.5, CI_97.5)")
    }

    # Creating the new CI column
    data$DIF_CI <- sprintf("%.2f [%.2f, %.2f]", data[[dif_col]], data[[ci_lower_col]], data[[ci_upper_col]])

    # Check for group and item columns
    if (!("groups" %in% names(data)) || !("item" %in% names(data))) {
      stop("Data must contain 'groups' and 'item' columns")
    }

    # Selecting and renaming the columns
    result <- data[, c("groups", "item", "DIF_CI")]
    names(result) <- c("groups", "item", paste0(dif_type, " [IC 95%]"))

    return(result)
  }

  # Check if group_col exists in the data
  if (!group_col %in% names(data)) {
    stop(paste("The dataset doesn't have a variable named", group_col))
  }

  # Select the necessary items and group column before balancing
  data <- data %>%
    select(all_of(c(group_col, items)))

  # Balance the data if specified
  if (balance) {
    data <- PsyMetricTools::smote_multiclass(data,
                                             outcome = group_col,
                                             perc_maj = 100,
                                             k = 5)
  }

  # Define a group variable from data
  group <- data %>% select(!!sym(group_col)) %>% pull()

  # Calculate a multigroup model
  model_group <- mirt::multipleGroup(data = select(data, items),
                                     model = model_spec,
                                     group = group,
                                     SE = TRUE,
                                     itemtype = itemtype,
                                     verbose = FALSE,
                                     type = "G2")

  # DIF analysis via DRF
  DRF_model <- mirt::DRF(model_group, draws = 1000, DIF = FALSE, plot = FALSE,
                         theta_lim = c(-6, 6), DIF.cats = TRUE)

  # Extract and format DIF results
  results <- list(
    dDIF = format_dif_results(DRF_model$dDIF, "dDIF"),
    sDIF = format_dif_results(DRF_model$sDIF, "sDIF"),
    uDIF = format_dif_results(DRF_model$uDIF, "uDIF")
  )

  return(results)
}
