source("C:/Users/... put your path here.../paketi_ucitavanje.R")

Sys.setlocale("LC_TIME", "C")


#edit data
#PAR10
#omitting values after 5th October
n_par10 <- par10 %>% filter(par10$`Timestamp [UTC+1]` < "2022-09-30 23:55:00") 
n_par40 <- par40 %>% filter(par40$`Timestamp [UTC+1]` < "2022-09-30 23:55:00") 

#remove max value from 40 m
max(n_par40$`PAR [umol/(m^2s)]`)
n_par40 <- n_par40%>% filter(n_par40$`PAR [umol/(m^2s)]` != 1816.8)
n_par40 <- n_par40%>% filter(n_par40$`PAR [umol/(m^2s)]` != 814.7)

#plot
plot_ly(data = n_par10, 
        x = ~ `Timestamp [UTC+1]`, 
        y = ~ `PAR [umol/(m^2s)]`,  
        type = 'scatter', 
        mode = 'lines') %>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]"))

plot_ly(data = n_par40, 
        x = ~ `Timestamp [UTC+1]`, 
        y = ~ `PAR [umol/(m^2s)]`,    
        type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(title = "Date in 2022 (UTC+1)"), 
         yaxis = list(title = "PAR [μmol/(m<sup>2</sup>s)]"))



