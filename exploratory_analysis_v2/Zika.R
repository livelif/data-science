# Mapeando a ocorrência do vírus Zika

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_exploratoria_dos_dados")
getwd()

# http://combateaedes.saude.gov.br/pt/situacao-epidemiologica

# Carregando os pacotes
# devtools::install_github("wch/webshot")
library(dplyr)
library(ggplot2)


temp_files <- list.files(pattern = ".csv")
temp_files

myfiles <- lapply(temp_files, read.csv, stringsAsFactors = FALSE) 


str(myfiles, 1)
lapply(myfiles, names)[1]
lapply(myfiles, head,2)[1:2]


brazil <- do.call(rbind, myfiles)
View(brazil)
brazil <- brazil %>% 
  mutate(report_date = as.Date(report_date))


glimpse(brazil)


brazil <- brazil %>% select(-(6:7)) 


brazil %>% slice (1:20) 


brazil %>% filter(location_type == "region")
brazil %>% filter(location_type == "region") %>% 
  ggplot(aes(x = report_date, y = value, group = location, color = location)) + 
  geom_line() +  
  geom_point() +
  ggtitle("Casos de Zika por Região do Brasil")



region <- brazil %>% 
  filter(location_type == "region")

region %>% 
  ggplot(aes(x =location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")


region %>% 
  slice(1:length(unique(region$location)))

region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value))


region %>% 
  slice(1:length(unique(region$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location,levels=location,ordered=TRUE)) %>% 
  glimpse()


brazil_totals <- brazil %>% filter(location=="Brazil") 
region_totals <- brazil %>% filter(location_type=="region") %>%
  group_by(report_date,location) %>%  
  summarize(tot = sum(value)) 


regvec <- vector()  
length(regvec) <- nrow(brazil)
for (ii in 1:nrow(brazil)) {
  if (brazil[ii,]$location_type != "region")  {
    regvec[ii] <- newlab
  } else {
    newlab <- brazil[ii,]$location
    regvec[ii] <- newlab
  }
}


statedf <- cbind(brazil,regvec)


statedf <- statedf %>% filter(location != "Brazil") 
statedf <- statedf %>% filter(location_type != "region") 


statedf %>% group_by(report_date,regvec) %>% 
  summarize(tot=sum(value)) -> totals


library(ggmap)
longlat <- geocode(unique(statedf$location)) %>% 
  mutate(loc = unique(statedf$location)) 


statedf %>% filter(as.character(report_date) == "2016-06-11") %>% 
  group_by(location) %>% summarize(cases = sum(value)) %>% 
  inner_join(longlat, by = c("location" = "loc")) %>% 
  mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping


head(formapping) 


num_of_times_to_repeat <- formapping$cases
long_formapping <- formapping[rep(seq_len(nrow(formapping)),
                                  num_of_times_to_repeat),]


head(long_formapping)


install.packages("leaflet")
library(leaflet)


leaflet(long_formapping) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())
        
             


