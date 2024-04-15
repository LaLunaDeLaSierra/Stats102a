install.packages("tidyverse")
library(tidyverse)
messy_impute <- function(messy_table, center = "Mean", margin, ...){
  #This function will impute values from the data frame by using the mean or      
  #median of the numerical values
  #Args:
  #messy_table: Data frame made of rows and columns of any length
  #center: Character vector with value "mean" or "median"
  #margin: integer vector with value 1 or 2
  #Return:
  #Data frame with the same number of rows and columns as the input data frame
  center <- tolower(center)
  if(class(messy_table) != "data.frame"){
    stop("The input must be a data frame")
  }
  if(margin == 1){
    for(i in 1:ncol(messy_table)){
      col <- messy_table[,i]
      missing_i <- which(is.na(col))
      if(length(missing_i) > 0){
        if(center == "mean"){
          col_mean <- mean(col, na.rm = TRUE)
          col[missing_i] <- col_mean
        }else if(center == "median"){
          col_median <- median(col, na.rm = TRUE)
          col[missing_i] <- col_median
        }
        messy_table[,i] <- col
      }
    }
  }else if(margin == 2){
    for(i in 1:nrow(messy_table)){
      my_rows <- as.numeric(messy_table[i,])
      missing_i <- which(is.na(my_rows))
      if(length(missing_i) > 0){
        if(center == "mean"){
          row_mean <- mean(my_rows, na.rm = TRUE)
          my_rows[missing_i] <- row_mean
        }else if(center == "median"){
          row_median <- median(my_rows, na.rm = TRUE)
          my_rows[missing_i] <- row_median
        }
        messy_table[i,] <- my_rows
      }
    }
  }else{
    stop("Your margin value is invalid")
  }
  messy_table
}

tidy_impute <- function(tidy_table, center = "Mean", margin, ... ){
  #This function will take a tidy data table and impute the NA values within it, returning
  #a tibble of tidy data with no NA values
  #Args:
  #tidy_table: tibble of any dimension
  #center: character vector of value "mean" or "median"
  #margin: integer value of 1 or 2
  #Return:
  #Tidy tibble with same dimensions as the input tibble
  center <- tolower(center)
  if(center == "mean"){
    for(i in 1:max(length(ncol(tidy_table)))){
      if(is.na(tidy_table$Score[i]) == TRUE){
        missing_i <- tidy_table[i]
        tidy_mean <- mean(missing_i, na.rm = TRUE)
        tidy_table[missing_i] <- tidy_mean
      }
      tidy_table[i] <- tidy_mean
    }
  }else if(center == "median"){
    for(i in 1:max(length(ncol(tidy_table)))){
      if(is.na(tidy_table$Score[i]) == TRUE){
        missing_i <- tidy_table[i]
        tidy_median <- median(missing_i, na.rm = TRUE)
        tidy_table[missing_i] <- tidy_median
      }
      tidy_table[i] <- tidy_median
    }
  }else{
    stop("Your margin value is invalid")
  }
  tidy_table
}
