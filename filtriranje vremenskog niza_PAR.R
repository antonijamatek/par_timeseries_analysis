source("C:/Users/... put your path here.../paketi_ucitavanje.R")

Sys.setlocale("LC_TIME", "C")

#**time-indexing** is preformed so we can apply the moving average function in order to filter out high frequency variability in the time-series and check if there is noise in low-frequency variability
#PAR10
par10ts <- zoo(n_par10$`PAR [umol/(m^2s)]`, order.by = n_par10$`Timestamp [UTC+1]`)
par10ts
#PAR40
par40ts <- zoo(n_par40$`PAR [umol/(m^2s)]`, order.by = n_par40$`Timestamp [UTC+1]`)
par40ts


#low-frequency filtering of data using moving average that is applied within window size 
#on time series data (ts_l0, ts_l8, ts_l8m)
#**window size determines the magnitude of low frequency filtering**
#5 min to 30 min - 6, 1 h - 12, 3 h - 36, 6 h - 72

#**10 meters*
#**30 min**
par10_0.5h <- rollapply(par10ts, 
                      width = 6, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA)  #applying the moving average over the time series 
time <- index(par10ts)
start_time <- min(time)
end_time <- max(time)
x <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
par10_0.5h <- aggregate(par10_0.5h, as.POSIXct(cut(time, breaks = x)), FUN = mean)

#**1 hour**
par10_1h <- rollapply(par10ts, 
                      width = 12, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA)  #applying the moving average over the time series 
x <- seq.POSIXt(from = start_time, to = end_time, by = "1 hour")
par10_1h <- aggregate(par10_1h, as.POSIXct(cut(time, breaks = x)), FUN = mean)

#**3 hours**
par10_3h <- rollapply(par10ts, 
                      width = 36, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA)  
x <- seq.POSIXt(from = start_time, to = end_time, by = "3 hours")
par10_3h <- aggregate(par10_3h, as.POSIXct(cut(time, breaks = x)), FUN = mean)

#**6 hours**
par10_6h <- rollapply(par10ts, 
                         width = 72, 
                         FUN = mean, 
                         align = "center", 
                         fill = NA)  
x <- seq.POSIXt(from = start_time, to = end_time, by = "6 hours")
par10_6h <- aggregate(par10_6h, as.POSIXct(cut(time, breaks = x)), FUN = mean)


#**in order to plot, we set time series into tibble**
par10_0.5h <- tidy(par10_0.5h) #tidy function creates a data frame 
colnames(par10_0.5h)[1] <- "Timestamp [UTC+1]"
colnames(par10_0.5h)[2] <- "PAR [umol/(m^2s)]"

par10_1h <- tidy(par10_1h) #tidy function creates a data frame 
colnames(par10_1h)[1] <- "Timestamp [UTC+1]"
colnames(par10_1h)[2] <- "PAR [umol/(m^2s)]"

par10_3h <- tidy(par10_3h)  
colnames(par10_3h)[1] <- "Timestamp [UTC+1]"
colnames(par10_3h)[2] <- "PAR [umol/(m^2s)]"

par10_6h <- tidy(par10_6h)  
colnames(par10_6h)[1] <- "Timestamp [UTC+1]"
colnames(par10_6h)[2] <- "PAR [umol/(m^2s)]"

#**plot together**
#dash: Sets the line type. It can be one of several options such as 'solid', 'dot', 'dash', 'longdash', 'dashdot', or 'longdashdot'.

plot_ly(data = n_par10, 
        x = ~`Timestamp [UTC+1]`, 
        y = ~`PAR [umol/(m^2s)]`,  
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'gray', width = 2, dash = 'solid'),
        name = "5 min interval")%>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]",
                      tickformat = ".1e"),
         title = "10 m")%>% 
  add_lines(data = par10_0.5h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'red', width = 2, dash = 'solid'),
            name = "30 min interval")%>%
  add_lines(data = par10_1h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'blue', width = 2, dash = 'solid'),
            name = "1 hour interval")%>%
  add_lines(data = par10_3h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'green', width = 2, dash = 'solid'),
            name = "3 hour interval")%>%
  add_lines(data = par10_6h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'orange', width = 2, dash = 'solid'),
            name = "6 hour interval")


#**40 meters**

#**30 min**
par40_0.5h <- rollapply(par40ts, 
                      width = 6, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA) 
time <- index(par40ts)
start_time <- min(time)
end_time <- max(time)
x <- seq.POSIXt(from = start_time, to = end_time, by = "30 min")
par40_0.5h <- aggregate(par40_0.5h, as.POSIXct(cut(time, breaks = x)), FUN = mean)

#**1 hour**
par40_1h <- rollapply(par40ts, 
                      width = 12, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA) 
x <- seq.POSIXt(from = start_time, to = end_time, by = "hour")
par40_1h <- aggregate(par40_1h, as.POSIXct(cut(time, breaks = x)), FUN = mean)

#**3 hours**
par40_3h <- rollapply(par40ts, 
                      width = 36, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA)  
x <- seq.POSIXt(from = start_time, to = end_time, by = "3 hours")
par40_3h <- aggregate(par40_3h, as.POSIXct(cut(time, breaks = x)), FUN = mean)

#**6 hours**
par40_6h <- rollapply(par40ts, 
                      width = 72, 
                      FUN = mean, 
                      align = "center", 
                      fill = NA)  
x <- seq.POSIXt(from = start_time, to = end_time, by = "6 hours")
par40_6h <- aggregate(par40_6h, as.POSIXct(cut(time, breaks = "6 hours")), FUN = mean)


#**set tibble**
par40_0.5h <- tidy(par40_0.5h) 
colnames(par40_0.5h)[1] <- "Timestamp [UTC+1]"
colnames(par40_0.5h)[2] <- "PAR [umol/(m^2s)]"

par40_1h <- tidy(par40_1h) 
colnames(par40_1h)[1] <- "Timestamp [UTC+1]"
colnames(par40_1h)[2] <- "PAR [umol/(m^2s)]"

par40_3h <- tidy(par40_3h)  
colnames(par40_3h)[1] <- "Timestamp [UTC+1]"
colnames(par40_3h)[2] <- "PAR [umol/(m^2s)]"

par40_6h <- tidy(par40_6h)  
colnames(par40_6h)[1] <- "Timestamp [UTC+1]"
colnames(par40_6h)[2] <- "PAR [umol/(m^2s)]"

#**plot together**
plot_ly(data = n_par40, 
        x = ~`Timestamp [UTC+1]`, 
        y = ~`PAR [umol/(m^2s)]`,  
        type = 'scatter', 
        mode = 'lines',
        line = list(color = 'gray', width = 2, dash = 'solid'),
        name = "5 min interval")%>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]",
                      tickformat = ".1e"),
         title = "40 m")%>% 
  add_lines(data = par40_0.5h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'red', width = 2, dash = 'solid'),
            name = "30 min interval")%>%
  add_lines(data = par40_1h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'blue', width = 2, dash = 'solid'),
            name = "1 hour interval")%>%
  add_lines(data = par40_3h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'green', width = 2, dash = 'solid'),
            name = "3 hour interval")%>%
  add_lines(data = par40_6h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            line = list(color = 'orange', width = 2, dash = 'solid'),
            name = "6 hour interval")

#**Compare light at 10 m and 40 m using 1 hour interval time-series data**
plot_ly(data = par10_1h, 
        x = ~`Timestamp [UTC+1]`, 
        y = ~`PAR [umol/(m^2s)]`,  
        type = 'scatter', 
        mode = 'lines',
        name = "10 m")%>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]",
                      tickformat = ".1e"),
         title = "1 hour interval")%>% 
  add_lines(data = par40_1h,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            name = "40 m")


#**Use dplyr filter function to filter out the data for certain period and than plot and look into more detail**
#example: filtering 1 hour interval data into week between 20th and 30th August
aug_par10 <- par10_1h %>%
  filter(`Timestamp [UTC+1]` >= "2022-08-20 06:00:00" &
           `Timestamp [UTC+1]` <= "2022-08-30 21:00:00")
range(aug_par10$`PAR [umol/(m^2s)]`)

aug_par40 <- par40_1h %>%
  filter(`Timestamp [UTC+1]` >= "2022-08-20 06:00:00" &
           `Timestamp [UTC+1]` <= "2022-08-30 21:00:00")


#plot together
plot_ly(data = aug_par10, 
        x = ~`Timestamp [UTC+1]`, 
        y = ~`PAR [umol/(m^2s)]`,  
        type = 'scatter', 
        mode = 'lines',
        name = "10 m")%>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]",
                      tickformat = ".1e"),
         title = "1 hour interval")%>% 
  add_lines(data = aug_par40,
            x = ~ `Timestamp [UTC+1]`, 
            y = ~ `PAR [umol/(m^2s)]`,  
            type = 'scatter', 
            mode = 'lines', 
            name = "40 m")


#**Save data**
  
#save data from environment to R.data for further use in another project
save(n_par10, n_par40, file = "edited_par10_par40.RData")
#save data from environment as .csv file for further use in another program 
write.csv(aug_par10, "aug_par10.csv")
