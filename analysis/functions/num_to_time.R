#Function that takes Marport sensor time and converts to Posix

library(stringr)
library(readr)

#start_time must be in ISO format 
NumToTime<-function(x,start_date){
  y<-str_replace(x,"(\\d{1})(\\d{2})(\\d{2})$","\\1:\\2:\\3")
  y<-parse_time(y)
  z<- as.POSIXct(paste(as.Date(start_date),y),format="%Y-%m-%d %H:%M:%S")  
  return(y)
  
}


