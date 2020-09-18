setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_exploratoria_dos_dados_aprendendo")

getwd()

library(ISLR)
library(dplyr)
str(Carseats)


carseats <- ISLR::Carseats

View(carseats)

set.seed(123)
carseats[sample(seq(NROW(carseats)), 10), "Urban"] <- NA

library(dlookr)
describe(carseats)

carseats %>%
  describe(Sales, CompPrice, Income)

carseats %>%
  describe() %>%
  select(variable, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

normality(carseats, Sales, CompPrice, Income)

plot_normality(carseats, Sales, CompPrice)

plot_correlate(carseats)

carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  plot_correlate(Sales)


#categorical
categ <- target_by(carseats, US)
categ
cat_num <- relate(categ, Sales)
cat_num
summary(cat_num)

plot(cat_num)

cat_cat <- relate(categ, ShelveLoc)
cat_cat
plot(cat_cat)


#numeric

num <- target_by(carseats, Sales)
num_num <- relate(num, Price)
num_num

lm(Sales ~ Price, data = carseats)
summary(num_num)

plot(num_num)

plot(num_num, hex_thres = 350)

# predict categorical
num_cat <- relate(num, ShelveLoc)
num_cat
plot(num_cat)
