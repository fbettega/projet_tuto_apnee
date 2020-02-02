library("tidyverse")
library("lubridate")
pre_tt_PA <- function(x){
PA <- read.csv(x)  
PA$Time <- dmy_hms(as.character(PA$Time))
PA_without_zero <- PA  %>% filter(SBP != 0,DBP != 0) %>% mutate(MonDay=replace(MonDay, MonDay==-364, 1)) %>% mutate(MonDay=replace(MonDay, MonDay==-363, 2))

return(PA_without_zero)
}





longue_data_heures <- function(x){
    df <- pre_tt_PA(x)
    Systo <- df %>% mutate(heure_mesure = hour(df$Time)) %>% select(UNID,SBP,heure_mesure) 
    large_systo <- Systo %>% 
      pivot_wider(names_from = heure_mesure, values_from = SBP,    values_fn = list(SBP = mean)) 
    df_res <- large_systo %>%
      select(-UNID)  %>%
      t %>%
      as.data.frame %>%
      fill(names(.), .direction ="downup") %>%
      t %>%
      as_tibble %>%
      mutate(UNID = large_systo$UNID)
  return(df_res)
}
