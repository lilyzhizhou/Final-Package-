######################
### sample dataset ###
######################
library(tidyverse)

# Define the possible categories for each column
colors <- c("blue", "white", "brown")
countries <- c("japan", "canada", "peru")
diets <- c("fish", "bug", "fruit")
can_fly<- c(0, 1)
wingspan <- runif(30, min = 3, max = 150)

# Create a random selection of categories for each column
random_colors <- sample(colors, 30, replace = TRUE)
random_countries <- sample(countries, 30, replace = TRUE)
random_diets <- sample(diets, 30, replace = TRUE)
random_fly <- sample(can_fly, 30, replace = TRUE)

# Create the dataframe
birds <- data.frame(color = random_colors, country = random_countries, diet = random_diets, can_fly = random_fly, wingspan = wingspan) %>%
  mutate(color = ifelse(country == 'canada' & runif(1) < 0.5, 'white', color),
         diet = ifelse(can_fly == 'fly', sample(c("bug", "fish"), n(), replace = TRUE), diet))
  

# View the first few rows of the dataframe
head(birds)

# export data
write.csv(birds, file = "data/birds.csv", row.names = FALSE)

#################
### Function ####
#################
library(corrplot) # to generate corrplot 
library(rcompanion) # to use cramers V


# Calculate cramersV scores for overall variable correlation 
############################################################

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

# Generate correlation matrix for categorical variables 
corrplot_cat <- function(df) {
  cramer_matrix <- calculate_cramers_v(df)
  cramer_matrix
  
  corrplot(cramer_matrix, method = "color", addCoef.col = "black", is.corr = FALSE, tl.col = "black")
}

calculate_cramers_v(birds)
corrplot_cat(birds)

corrplot_cat2(birds[1:4])
