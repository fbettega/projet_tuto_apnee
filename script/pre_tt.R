library("tidyverse")
library("lubridate")
select <- dplyr::select

pre_tt_PA <- function(x){
PA <- read.csv(x)  
PA$Time <- dmy_hms(as.character(PA$Time))
PA_without_zero <- PA  %>% filter(SBP != 0,DBP != 0) %>% mutate(MonDay=replace(MonDay, MonDay==-364, 1)) %>% mutate(MonDay=replace(MonDay, MonDay==-363, 2))

return(PA_without_zero)
}


'%!in%' <- function(x,y)!('%in%'(x,y))


longue_data_heures <- function(x){
    df <- pre_tt_PA(x)
    Systo <- df %>% mutate(heure_mesure = hour(df$Time)) %>%  select(UNID,SBP,heure_mesure) 
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


sort_names <- function(data) {
  name  <- names(data)
  chars <- str_subset(name,"[:upper:]")
  nums  <- name %>% 
    as.numeric() %>% 
    sort() %>% 
    sprintf("`%s`", .)
  
  select_(data, .dots = c(chars, nums))
}

longue_data_minutes <- function(x){
df <- pre_tt_PA(x)

df_over_24_hours <- df %>% group_by(UNID) %>% filter(Time < (min(Time) + days(1))) 

Systo_min <- df_over_24_hours  %>% mutate(heure_mesure = hour(Time),minute_mesure = cut(minute(Time), breaks = seq(0,60,10), include.lowest = TRUE)) %>% 
    group_by(UNID,heure_mesure,minute_mesure) %>%
    mutate(SBP_c = max(SBP),DBP_c = max(DBP)) %>% 
    distinct(UNID,heure_mesure,minute_mesure,SBP_c,DBP_c) %>% 
    ungroup() %>% 
    mutate(mesure_time = heure_mesure + as.numeric(minute_mesure)/10 ) %>%  select(-heure_mesure,-minute_mesure)






large_systo <- Systo_min %>% 
   select(-DBP_c) %>% 
  pivot_wider(names_from = mesure_time, values_from = SBP_c)   %>% 
  {suppressWarnings(sort_names(.))}
large_diasto <- Systo_min %>% 
   select(-SBP_c) %>% 
  pivot_wider(names_from = mesure_time, values_from = DBP_c)  %>% 
  {suppressWarnings(sort_names(.))}    

df_res_systo <- large_systo %>%
   select(-UNID)   %>%
  t %>%
  as.data.frame %>%
  fill(names(.), .direction ="downup") %>%
  t %>% 
  as_tibble %>%     rename_all(funs( c(colnames(large_systo[2:ncol(large_systo)])))) %>% 
  mutate(UNID = large_systo$UNID)

df_res_diasto <- large_diasto %>%
   select(-UNID)  %>%
  t %>%
  as.data.frame %>%
  fill(names(.), .direction ="downup") %>%
  t %>%
  as_tibble %>% 
  rename_all(funs( c(colnames(large_diasto[2:ncol(large_diasto)])))) %>% 
  mutate(UNID = large_diasto$UNID) 
return(list(systo = df_res_systo,diasto = df_res_diasto))
}
