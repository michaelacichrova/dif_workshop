summary(item_data)

dat <- item_data   # copy

for (col in names(dat)) {
  
  # Try numeric conversion
  suppressWarnings(num_version <- as.numeric(dat[[col]]))
  
  # If >95% of rows convert to numeric → treat as numeric
  if (mean(!is.na(num_version)) > 0.95) {
    
    dat[[col]] <- num_version
    
  } else {
    # Otherwise convert to factor (no characters allowed)
    dat[[col]] <- as.factor(dat[[col]])
  }
}

summary(dat)


is_binary <- function(x) {
  if (is.numeric(x)) {
    u <- unique(na.omit(x))
    return(all(u %in% c(0, 1)))
  }
  if (is.factor(x)) {
    return(nlevels(x) == 2)
  }
  FALSE
}

binary_items <- names(dat)[ sapply(dat, is_binary) ]
binary_items <- 
