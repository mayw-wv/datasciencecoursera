
corr<- function(directory,  thres=0) {
   
  result <- NULL
  for (idnum in 1:332) {
    # put proper number of zeros in front of idnum to open the .csv files in directory
    if (idnum < 10) idchar <- paste("00",as.character(idnum), sep="")
    if (idnum>=10 & idnum<100) idchar<- paste("0", as.character(idnum), sep="")
    if (idnum>=100) idchar <- as.character(idnum)
    
    # compute the individual .csv file names and open them
    csvname = paste(directory, "/", idchar, ".csv", sep="")
    csvData = read.csv(csvname)
    
    # check if either "nitrate" or "sulfate" = NA
    csvCount <- subset(csvData, !is.na(csvData$nitrate) & !is.na(csvData$sulfate) )
    
    # if( dim(csvCount)[1] >= thres)  print (c(idnum, cor(csvCount$sulfate, csvCount$nitrate)))
    if (dim(csvCount)[1]<thres) corrNum <- 0
                      else corrNum <- cor(csvCount$sulfate, csvCount$nitrate)
    if( dim(csvCount)[1] >= thres)  result <- c(result, corrNum )
    
  }
    
    result
}

