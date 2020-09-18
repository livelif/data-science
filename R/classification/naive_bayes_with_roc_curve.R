setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_classificação")

library(e1071)
library(ggplot2)
library(dplyr)
library(pROC)

titanicDataFrame <- as.data.frame(Titanic)

View(titanicDataFrame)

mansNoSurvived <- titanicDataFrame %>% filter(Sex == "Male" & Survived == "No")

ggplot(mansNoSurvived, aes(x = Class, y = Freq)) + geom_bar(stat = "identity")

ggplot(mansNoSurvived, aes(x = Age, y = Freq)) + geom_bar(stat = "identity")

howManyChildHave <- titanicDataFrame %>% filter(Age == "Child") %>% summarise(sum(Freq))
howManyChildNoSurvived <- mansNoSurvived %>% filter(Age == "Child") %>% summarise(sum(Freq))

howManyAdultHave <- titanicDataFrame %>% filter(Age == "Adult") %>% summarise(sum(Freq))
howManyAdultNoSurvived <- mansNoSurvived %>% filter(Age == "Adult") %>% summarise(sum(Freq))

percentDeathChild <- (howManyChildNoSurvived / howManyChildHave) * 100
percentDethAdult <- (howManyAdultNoSurvived / howManyAdultHave) * 100


dataDeaths <- as.data.frame(percentDethAdult)
dataDeaths$New = "Adult"
colnames(dataDeaths) <- c("DeathsPercent", "Type")

dataDeaths[nrow(dataDeaths) + 1, ] <- percentDeathChild
dataDeaths[2,2] <- "Child"

ggplot(dataDeaths, aes(y = DeathsPercent, x = Type)) + geom_histogram(stat = "identity")


#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(titanicDataFrame)), titanicDataFrame$Freq) #This will repeat each combination equal to the frequency of each combination

#Create the dataset by row repetition created
Titanic_dataset=titanicDataFrame[repeating_sequence,]

#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)


#What does the model say? Print the model summary
summary(Naive_Bayes_Model)

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)

#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)


# ROC

?glm

fit1 <- glm(Survived ~ Class, data = Titanic_dataset, family = binomial)

summary(fit1)

prob <- predict(fit1, type=c("response"))

fit1$prob <- prob

roc1 <- roc(Survived ~ prob, data = Titanic_dataset, plot = T)

roc1
