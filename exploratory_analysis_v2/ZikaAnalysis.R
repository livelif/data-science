setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_exploratoria_dos_dados")
getwd()

library(dplyr)
library(ggplot2)

fileNames <- list.files(pattern = ".csv")

dataOfArchives <- lapply(fileNames, read.csv, stringsAsFactors = FALSE)

?do.call
# do Call constroi e executa uma funcao com uma lista de parametros
?rbind

unionDataOfArchivesEmUm <- function(dataOfArchives) {
  dataFrames <- dataOfArchives[[1]]
  for (i in 2:length(dataOfArchives)) {
    dataFrames <- rbind(dataFrames, dataOfArchives[[i]])
  }
  return(dataFrames)
}

allDatas <- unionDataOfArchivesEmUm(dataOfArchives = dataOfArchives)

allDatas <- allDatas %>% mutate(report_date, as.Date(report_date))

allDatas %>% filter(location_type == "region") %>% ggplot(aes(x=report_date, y = value, group = location, color = location)) +
  geom_point() +
  geom_line() +
  ggtitle("Zika Virus")


regions <- allDatas %>% filter(location_type == "region")

regions %>% ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") + 
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")

?slice

View(regions)

regions %>% 
  slice(1:length(unique(regions$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") +
  ylab("Número de Casos Reportados") + xlab("Region") + 
  ggtitle("Casos de Zika Reportados no Brasil")


unicRegions <- regions %>% slice(1:length(unique(regions$location)))

unicRegions <- unicRegions %>% arrange(desc(value))

allCasesRegions <- regions %>% filter(location_type == "region") %>% group_by(report_date, location) %>% summarize(total = sum(value))

View(allCasesRegions)

unicRegions[,1]

vectorOfRegion <- vector()  

for (i in 1:nrow(allDatas)) {
  if (allDatas[i,]$location_type != "region")  {
    vectorOfRegion[i] <- newlab
  } else {
    newlab <- allDatas[i,]$location
    vectorOfRegion[i] <- newlab
  }
}


dataFrameState <- cbind(allDatas, vectorOfRegion)

View(dataFrameState)

dataFrameState <- dataFrameState %>% filter(location != "Brazil") 
dataFrameState <- dataFrameState %>% filter(location_type != "region") 

dataFrameState %>% group_by(report_date,vectorOfRegion) %>% 
  summarize(tot=sum(value)) -> total

sumRegion <- total %>% select(vectorOfRegion, tot) %>% group_by(vectorOfRegion) %>% summarise(total = sum(tot))

View(total)
View(sumRegion)

sumRegion %>% arrange(desc(total)) %>% mutate(vectorOfRegion = factor(vectorOfRegion, levels = vectorOfRegion, ordered = TRUE)) %>%
  ggplot(aes(x = vectorOfRegion, y = total)) + geom_bar(stat = "identity")
