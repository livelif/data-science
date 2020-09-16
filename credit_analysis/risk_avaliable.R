setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_credito")

credit.df <- read.csv("credit_dataset.csv", header = TRUE, sep = ",")
head(credit.df)


convertVariablesToFactors <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  
  return(df)
}

scaleData <- function(df, numberVariables) {
  for (variable in numberVariables) {
    df[[variable]] <- scale(df[[variable]], center = TRUE, scale = TRUE)
  }
  
  return(df)
}

categoricalVariables <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')

credit.df <- convertVariablesToFactors(credit.df, categoricalVariables)

numericVariables <- c("credit.duration.months", "age", "credit.amount")
credit.df <- scaleData(credit.df, numericVariables)


?sample

numberLines <- nrow(credit.df)
numberLines

sizeTrainDataSet <- floor(0.8 * nrow(credit.df))

indexTrainDataset <- sample(seq_len(numberLines), size = sizeTrainDataSet)

treinoDataset <- credit.df[indexTrainDataset, ]

testeDataset <- credit.df[-indexTrainDataset, ]

library(caret) 
library(randomForest) 

?rfeControl

selectVariable <- function(variaveisDoDataset, variaveisDeClassificacao) {
  numeroIteracoes=20
  quantidadeDeVariaveis <- 1:10;
  control <- rfeControl(functions = rfFuncs, method = "cv", 
                        verbose = FALSE, returnResamp = "all", 
                        number = numeroIteracoes)
  
  resultados <- rfe(x = variaveisDoDataset, y = variaveisDeClassificacao, 
                     sizes = quantidadeDeVariaveis, 
                     rfeControl = control)
  
  return(resultados)
}

removeDotColunmNames <- function(df) {
  nomeDasColunas <- colnames(treinoDataset)
  for(nome in 1:length(nomeDasColunas)) {
    nomeDasColunas[[nome]] <- gsub("\\.", "", nomeDasColunas[[nome]])
  }
  
  return(nomeDasColunas)
}

colunmNamesWithoutDot <- removeDotColunmNames(treinoDataset)
colnames(treinoDataset) <- colunmNamesWithoutDot

importantVariable <- selectVariable(variaveisDoDataset = treinoDataset[,-1], variaveisDeClassificacao = treinoDataset[,1])
importantVariable
varImp(importantVariable)

 
library(ROCR) 

source("plot_utils.R") 

View(treinoDataset)

formula <- "creditrating ~ accountbalance + creditdurationmonths + previouscreditpaymentstatus + creditamount"
formula <- as.formula(formula)

model <- glm(formula = formula, data = treinoDataset, family = "binomial")

summary(model)


colunmNamesWithoutDot <- removeDotColunmNames(testeDataset)
colnames(testeDataset) <- colunmNamesWithoutDot

predict <- predict(model, testeDataset, type = "response")
predict <- round(predict)

testVariablesClassification <- testeDataset[,1]
filterVariables <- testeDataset[,-1]

confusionMatrix(table(data = predict, reference = testVariablesClassification), positive = '1')


values <- predict(model, filterVariables, type = "response")
predict <- prediction(values, variaveisDeClassificacao)
par(mfrow = c(1,2))
plot.roc.curve(predict, title.text = "Curva ROC")
plot.pr.curve(predict, title.text = "Curva Precision/Recall")
