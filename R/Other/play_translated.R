# Libraries ----
library(dplyr)

# which(myV>7)[1]
idx_greater_than <- function(value, list){
  #Find the first index of vector 'list' that has a corresponding value greater than 'value'
  for(i in 1:length(list)){
    if(list[i] > value){
      return(i)
    }
  }
}

# MAIN LOGIC STARTS HERE ----

df <- data.frame(colour = c("red", "green", "red", "blue", "blue", NA, "blue", NA),
                 coolest = c("braun", "kinz", "braun", NA, NA, "skittles", "skittles", "kinz"))

for(name in names(df)){
  column_vector <- pull(df, name)
  
  # Get index of nans
  nan_idxs <- which(is.na(column_vector))
  
  # If no nans, don't worry
  if(length(nan_idxs)==0){
    next
  }
  
  srs_notnull <- column_vector[!is.na(column_vector)]
  
  # Get unique labels and counts for the non-nan features
  unique_frequency_df <- as.data.frame(table(srs_notnull))
  labels <- as.character(unique_frequency_df$srs_notnull)
  counts <- unique_frequency_df$Freq
  cum_counts <- cumsum(counts)
  
  # Generate random numbers of size len(nan_idxs)
  rand_vals <- floor(runif(length(nan_idxs), min=0, max=length(srs_notnull)))
  
  new_vals <- c()
  for(x in rand_vals){
    #Find out the largest number in cum_counts that each rand_val is less than
    larger_value_index <- idx_greater_than(x, cum_counts)
    # Get values corresponding to above index
    new_vals <- append(new_vals, labels[larger_value_index])
  }
  
  # Update the df with the new vals
  df[nan_idxs, name] = new_vals
}
