#' @title Calculate Cramers V
#'
#' @description
#' Calculates CramersV score for categorical variables
#'
#' @param df dataset
#'
#' @return CramersV score
#' @author Lily Zhou


calculate_cramers_v <- function(df) {
  ncol <- ncol(df)
  mat <- matrix(1, ncol, ncol) # create matrix to store cramerv values, dimensions are same a # of columns of df

  for (i in 1:(ncol - 1)) {                  # start at column 1 and go up till the 2nd last column
    for (j in (i + 1):ncol) {                # start at column 2 and go up till the last column

      result <- cramerV(df[, i], df[, j])    # calculate craverV scores for each column iteration
      mat[i, j] <- result                    # input the score into the indicated row/column in the matrix
      mat[j, i] <- result                    # input the same score into the indicated row/column in the matrix
    }
  }

  rownames(mat) <- colnames(df)             # set row names in matrix to match dataset names
  colnames(mat) <- colnames(df)             # set column names in matrix to match dataset names

  return(mat)

}
