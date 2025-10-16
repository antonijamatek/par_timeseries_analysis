source("C:/Users/... put your path here.../paketi_ucitavanje.R")

Sys.setlocale("LC_TIME", "C")

#CHECK DIRECTORY
getwd()

#SET DIRECTORY
setwd()

#IMPORT DATA
#PAR10
par10 <- readr::read_delim("par10_22.csv", delim = ",", show_col_types = FALSE)
par10[4] <- NULL #removing the last column 
#PAR40
par40 <- readr::read_delim("par40_22.csv", delim = ",", show_col_types = FALSE)
par40[4] <- NULL #removing the last column

#set tibble
par10 <- as_tibble(par10)
par40 <- as_tibble(par40)

#see data in your console
par10
par40

#we can see that PAR column is a  "double" format which is good, however the Timestamp column is 
#a character vector and we need to transform it into date and time vector indicated by <dttm>

#EDIT DATA

#function to define timestamp format <dttm>: convert to 24-hour (remove AM/PM) and define format as %Y-%m-%d %H:%M:%S
timestamp_format <- function(x) {
  #timestamp structure we are converting (must be the same as in our data frame)
  timestamp <- ymd_hms(x)
  #convert timestamp to the desired format
  formatted_timestamp <- format(timestamp, "%Y-%m-%d %H:%M:%S")
  return(formatted_timestamp)
}

#set format using function timestamp_format():
par10$TimeStamp <- sapply(par10$TimeStamp, timestamp_format)
par40$TimeStamp <- sapply(par40$TimeStamp, timestamp_format)

#define as date and time vector <dttm> using POSIXct(): 
par10$TimeStamp <- POSIXct(par10$TimeStamp)
par40$TimeStamp <- POSIXct(par40$TimeStamp)

#convert timestamp from UTC +02:00 to UTC +01:00 using with_tz():
par10$TimeStamp <- with_tz(par10$TimeStamp, tzone = "Europe/London")
par40$TimeStamp <- with_tz(par40$TimeStamp, tzone = "Europe/London")

#change column name 
colnames(par10)[1] <- "Timestamp [UTC+1]"
colnames(par40)[1] <- "Timestamp [UTC+1]"

#check the format of the vector and name
par10
par40

#plot
plot_ly(data = par10, 
              x = ~`Timestamp [UTC+1]`, 
              y = ~`PAR [umol/(m^2s)]`,  
              type = 'scatter', 
              mode = 'lines')%>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]"),
         title = "10m - raw data 5 min interval")

plot_ly(data = par40, 
              x = ~`Timestamp [UTC+1]`, 
              y = ~`PAR [umol/(m^2s)]`,  
              type = 'scatter', 
              mode = 'lines')%>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]"),
         title = "40m - raw data 5 min interval")


