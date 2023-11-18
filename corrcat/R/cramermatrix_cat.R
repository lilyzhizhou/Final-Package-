#' @title Correlation Matrix for Categorical Variables
#'
#' @description
#' Calculates CramersV score for categorical variables and produces correlation matrix
#'
#' @param df dataset
#'
#' @return CramersV score and Correlation matrix
#' @author Lily Zhou
#' @export
#'
#' @examples
#'
#' cramermatrix_cat(birds)
#' cramermatrix_cat(birds[1:4])


cramermatrix_cat <- function(df) {
  mat <- calculate_cramers_v(df)
  corrplot_cat(df)

  return(mat)
}

