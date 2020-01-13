####FUNCTION to generate random dataset based on data_dict and codebook inputs

makeRandomDF <- function(vars = data_dict, 
                         codes = codebook, 
                         num_rows = n,
                         yn_cols = yn_cols,
                         num_cols = num_cols,
                         num_ranges = num_ranges) {
    
    #create empty dataframe with column names
    randomDF <- data.frame(matrix(ncol = length(vars$auto_var), nrow = num_rows))
    names(randomDF) <- vars$auto_var
    
    #populate columns with randomized options from codebook
    ##TODO lapply this
    for(i in 1:ncol(randomDF)) {
        # check if there are codes
        if(names(randomDF)[i] %in% codes$auto_var == TRUE) {
          x <- codes[which(codes$auto_var == names(randomDF)[i]), ]
          randomDF[,i] <- sample(x$source_code, size = n, replace = T)
        } 
      }
    
    #populate date column
    randomDF$date <- sample(seq(as.Date('2019/09/01'), as.Date('2019/11/21'), by="day"), replace = T, size = n)
    
    #populate id column
    randomDF$id <- seq(1, n)
    
    #populate y/n columns
    for(i in 1:ncol(randomDF)) {
      # check if there are codes
      if(names(randomDF)[i] %in% yn_cols) {
        randomDF[,i] <- sample(c("yes", "no"), size = num_rows, replace = T)
      } 
    }
    
    #populate numerical columns
    for(i in 1:ncol(randomDF)) {
      # check if there are codes
      if(names(randomDF)[i] %in% num_cols) {
        j <- num_ranges[[which(num_cols == names(randomDF)[i])]]
        randomDF[,i] <- sample(j, size = num_rows, replace = T)
      } 
    }
    
    return(randomDF)
  }