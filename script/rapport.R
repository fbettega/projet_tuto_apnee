
library(fda.usc)
library(cluster)
library(factoextra)
library(tidyverse)
library(ROSE)
source(file = "pre_tt.R")
#df <- pre_tt_PA("data/Grenoble_raw.csv")

to_indice <- function(X) {
  x <- seq_along(unique(X))
  names(x) <- unique(X)
  x[as.character(X)]
}

#large_systo <- longue_data_heures("data/Grenoble_raw.csv")
#conflict_prefer("lag", "dplyr")

blood_pressure <- longue_data_minutes("data/Grenoble_raw.csv")

# enlève tete et queu de la dérivé
functional_data_heas_tail_deriv <- function(data){
  data[,1] <- 0
  data[,ncol(data)] <- 0
  return(data)
  
}


# rapport 

PA <- read.csv("data/Grenoble_raw.csv")

PA$Time <- dmy_hms(as.character(PA$Time))

PA %>% group_by(UNID) %>% summarise(min = min(Time), max = max(Time)) %>% mutate(diff = max - min) %>%  view()


# exemple de complétion des données 
library(xtable)


df <- pre_tt_PA("data/Grenoble_raw.csv")

df_over_24_hours <- df %>% group_by(UNID) %>% filter(Time < (min(Time) + days(1))) 

Systo_min <- df_over_24_hours  %>% mutate(heure_mesure = hour(Time),minute_mesure = cut(minute(Time), breaks = seq(0,60,10), include.lowest = TRUE)) %>% group_by(UNID,heure_mesure,minute_mesure) %>% mutate(SBP_c = max(SBP),DBP_c = max(DBP)) %>% distinct(UNID,heure_mesure,minute_mesure,SBP_c,DBP_c) %>% ungroup() %>% mutate(mesure_time = heure_mesure + as.numeric(minute_mesure)/10 ) %>% select(-heure_mesure,-minute_mesure)


large_systo <- Systo_min %>% 
  select(-DBP_c) %>% 
  pivot_wider(names_from = mesure_time, values_from = SBP_c)   %>% 
  {suppressWarnings(sort_names(.))}


df_res_systo <- large_systo %>%
  select(-UNID)   %>%
  t %>%
  as.data.frame %>%
  fill(names(.), .direction ="downup") %>%
  t %>% 
  as_tibble %>%     rename_all(funs( c(colnames(large_systo[2:ncol(large_systo)])))) %>% 
  mutate(UNID = large_systo$UNID)

xtable(large_systo[1:2,1:10],rnames = paste(c("patient 1", "patient 2")),caption="Exemple de données pour deux patients avec les mesure manquante")


# histogramme de la distannce des mesures 
df <- pre_tt_PA("data/Grenoble_raw.csv")

df_over_24_hours <- df %>% group_by(UNID) %>% filter(Time < (min(Time) + days(1))) 

ecart_mesure <- df_over_24_hours %>% group_by(UNID) %>% arrange(Time) %>%  mutate(ecart_mesure = as.integer(Time - dplyr::lag(Time, default = Time[1]))/60 ) %>% filter(ecart_mesure != 0)


quantile(ecart_mesure$ecart_mesure,seq(1,100)/100)

ggplot(data = ecart_mesure ) +
  geom_histogram(aes(ecart_mesure),binwidth = 10,color="darkblue", fill="lightblue") +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 180, 10)) +
  labs(title="Histogramme de l'écart entre les mesures",x="Temps(min)", y = "Log du Nombre de mesure")


# courbes + moyenne


df <- blood_pressure$systo %>% select(-UNID) %>% as.matrix()

heure <-   blood_pressure$systo %>% dplyr::select(-UNID) %>% colnames %>% str_replace_all("\\.","h") %>% paste0(.,"0") %>% .[{str_detect(.,"h10|h30|h50") ->> pos }  ] 

fonctionnaldata <- fdata(df,names = list(main = "Pression systolique", xlab = "heure de la mesure" , ylab = "Systolique blood Pressure"))
plot.fdata(fonctionnaldata) 
plot(fonctionnaldata,col = "grey",axes=FALSE)
lines(func.mean(fonctionnaldata), col = 2, lty = 2)
axis(1, at=which(pos==TRUE), labels=heure)
axis(2, at=c(50,100,150,200), labels=as.character(c(50,100,150,200)))





# Dérivée 
fonctionnaldata <- fdata(df_fonc,names = list(main = "Pression systolique", xlab = "heure de la mesure" , ylab = "SBP"))

fonctionnaldata_deriv <- fdata.deriv(fonctionnaldata, nderiv = 1)
fonctionnaldata_deriv$data <- functional_data_heas_tail_deriv(fonctionnaldata_deriv$data)

heure <-   blood_pressure$systo %>% dplyr::select(-UNID) %>% colnames %>% str_replace_all("\\.","h") %>% paste0(.,"0") %>% .[{str_detect(.,"h10|h30|h50") ->> pos }  ] 
palette(c("blue","grey"))
plot(fonctionnaldata_deriv[1:10],col = carac$VAR45,axes=FALSE,main = "Pression systolique coloré en fonction \n de la distance a la AHI",ylab = "Dérivée la SPB en fonction du temps")
legend(1, 15, legend=c("Apnée légère", "Apnée Sévère"),
       col=c("grey", "blue"), lty=c(1,1), cex=0.8)
axis(1, at=which(pos==TRUE), labels=heure)
axis(2, at=seq(-20,20,10), labels=as.character(seq(-20,20,10)))