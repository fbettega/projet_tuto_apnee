
pre_tt_PA <- function(x){
PA <- read.csv(x)  
PA_SBP_zero<- PA %>% group_by(UNID) %>% summarise(n=n(),sum(SBP == 0))%>% ungroup()

return(PA_SBP_zero)
}




