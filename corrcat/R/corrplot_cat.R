#' @title Categorical Correlation Matrix
#'
#' @description
#' Generates correlation matrix from cramersV matrix
#'
#' @param df dataset
#'
#' @return Correlation Matrix
#' @author Lily Zhou


# Generate correlation matrix for categorical variables
corrplot_cat <- function(df) {
  cramer_matrix <- calculate_cramers_v(df)
  cramer_matrix

  corrplot(cramer_matrix, method = "color", addCoef.col = "black", is.corr = FALSE, tl.col = "black")
}
