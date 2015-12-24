
complete<- function(directory,  idvec) {
  id <- c(NULL)
  nobs <-c(NULL)
  countDataFrame <- data.frame(id,nobs)
  for (idnum in idvec) {
    # put proper number of zeros in front of idnum to open the .csv files in directory
    if (idnum < 10) idchar <- paste("00",as.character(idnum), sep="")
    if (idnum>=10 & idnum<100) idchar<- paste("0", as.character(idnum), sep="")
    if (idnum>=100) idchar <- as.character(idnum)
    
    # compute the individual .csv file names and open them
    csvname = paste(directory, "/", idchar, ".csv", sep="")
    csvData = read.csv(csvname)
    
    # check if either "nitrate" or "sulfate" = NA
    csvCount <- subset(csvData, !is.na(csvData$nitrate) & !is.na(csvData$sulfate) )
    
    id <- c(id, idnum)
    nobs <- c(nobs, dim(csvCount)[1])
    
                  }  
  countDataFrame <- data.frame(id, nobs)
        }



