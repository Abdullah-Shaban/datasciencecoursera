pollutantmean <- function(directory, pollutant, id = 1:332) 
{
  
  ## =================================================
  ##  Trying to make sense of Ahmed Tadde's solutions!
  ## =================================================
  
  ## Ahmed setting the directory
    ## 'directory' is a character vector of length 1 indicating
    ##  the location of the CSV files
  
     setwd(file.path(getwd(), directory)) 
     total = 0                            
     observations = 0                     
  
      # First run failed; says: "cannot change working directory"  
  
     # cleaning data of missing values and setting id to 3 digits  
     for (i in id)
     {
           if (i <10) { 
               data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
               header = T, 
               na.strings=c("NA","NaN", " ")
                                )
                      }
      
            else if (i>=10 & i<100) { 
               data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
               header = T, 
               na.strings=c("NA","NaN", " ") 
                                )
                                     }
                     
             else       { 
                 data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                 header = T, 
                 na.strings=c("NA","NaN", " ") 
                                 ) 
                         }
  
        data = na.omit(data)                          ## to exclude rows containing missing values
       
        observations = observations + nrow(data)      ##  cumulative addition of the complete observations
 
         if (pollutant == "sulfate") {total = total + sum(data$sulfate)}     ## aggregate the observed values
         else {total = total + sum(data$nitrate)}

      }
   
    ## resetting directory path and returning the means of the pollutant values
       setwd("..")
       return (total/observations)

}

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
 
