

pollutantmean<- function(directory, pollutant, id=0) {
      csvCompute <- NULL
      
        for (idnum in id) {
            # put proper number of zeros in front of idnum to open the .csv files in directory
            if (idnum < 10) idchar <- paste("00",as.character(idnum), sep="")
            if (idnum>=10 & idnum<100) idchar<- paste("0", as.character(idnum), sep="")
            if (idnum>=100) idchar <- as.character(idnum)
              
            # compute the individual .csv file names and open them
            csvname = paste(directory, "/", idchar, ".csv", sep="")
            csvData = read.csv(csvname)
            csvCompute <- rbind(csvCompute, csvData)
            
      
                          } 
        # do the assignment computations
        
            if (pollutant=="sulfate") result <- mean(csvCompute$sulfate, na.rm=TRUE)
            if (pollutant=="nitrate") result <- mean(csvCompute$nitrate, na.rm=TRUE)
            result
              
      }
     
  


