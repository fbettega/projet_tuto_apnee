library("tidyverse")
library("lubridate")
pre_tt_PA <- function(x){
PA <- read.csv(x)  
PA$Time <- dmy_hms(as.character(PA$Time))
PA_without_zero <- PA  %>% filter(SBP != 0,DBP != 0)


return(PA_without_zero)
}




