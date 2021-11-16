# Megan Cater
# Exercise 9

# clears the environment
rm(list=ls())

# reads from a directory and determines the coefficient of variation of each file inside the directory
# if files have a header, user must specify the header
# if the files do not have a header, user can specify column # or leave blank
read_data<-function(dir, column=NULL, sep_type=",", override=FALSE) {
  # lists the files in the directory
  dir_files = list.files(path=dir)
  
  # vector to hold the coefficients of variation
  coeff_vars = numeric(length = length(dir_files))
  
  if_header = TRUE
  
  # if no header, default column is 1
  if (is.null(column)) {
    if_header = FALSE
    column = 1
  } else if (is.numeric(column)) {
    if_header = FALSE
  }
  
  # index value
  i = 1
  
  # for each file in the directory, reads data and calculates coefficient of variation
  for (file in dir_files) {
    # calculate remains TRUE if there is enough data or override is TRUE
    calculate = TRUE
    
    # creates file path
    file_path = paste(dir, file, sep="")
    
    # reads data from files in input directory
    data<-read.table(file_path, header=if_header, sep=sep_type, stringsAsFactors=FALSE)
    
    # if there is not enough data, print error if no override or warning if override
    if (nrow(data) < 50) {
      if (!override) {
        print(sprintf('Error: %s must have at least 50 observations.', file))
        calculate = FALSE
      } else {
        print(sprintf('Warning: %s does not have 50 observations.', file))
      }
    }
      
    # calculates the coefficient of variation if no error
    if (calculate) {
      coeff_vars[i] = sd(data[,column])/mean(data[,column])
    } else {
      coeff_vars[i] = NA
    }
      
    # increments index
    i = i + 1
  }
  
  return(coeff_vars)
}

# Example of usage
# read_data('test/', 'column', sep_type=',', override=TRUE)