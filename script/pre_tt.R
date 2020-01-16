library("tidyverse")
library("lubridate")
pre_tt_PA <- function(x){
PA <- read.csv(x)  
PA$Time <- dmy_hms(as.character(PA$Time))
PA_without_zero <- PA  %>% filter(SBP != 0,DBP != 0) %>% mutate(MonDay=replace(MonDay, MonDay==-364, 1)) %>% mutate(MonDay=replace(MonDay, MonDay==-363, 2))

return(PA_without_zero)
}




