###########################################################################
#
#   R code to reproduce the analysis reported in 
#   "Влияние семейной истории дислексии на развитие навыков фонологической обработки у детей 5-6 лет."
#   written by Tatiana Eremicheva
#   email: eremichevat22@gmail.com
#   Please email me if you see any errors or have any questions
#   last update: 19.03.2023
#
###########################################################################

library(tidyverse)
library(ggplot2)
library(effects)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)

rm(list = ls())
setwd("/Users/oksana/Downloads")


data = read.csv("ReadingPredictors_summary_data.csv", sep = ";")

data_norm <- data %>% 
  dplyr::select(N, ID, group, age, gender, RuTOPP_1_PhonDiscr:RuTOPP_10_ChangingSound) %>% 
  filter(group == "norm")

data_risk <- data %>% 
  dplyr::select(N, ID, group, age, gender, RuTOPP_1_PhonDiscr:RuTOPP_10_ChangingSound) %>% 
  filter(group == "risk")

##средний возраст 
mean(data$age) #76.375
sd(data$age) #4.604464

##средние баллы за каждый тест в группе нормы
tapply(X = data_norm$RuTOPP_1_PhonDiscr, INDEX = data_norm$gender, FUN = mean) 
#f - 0.8775000, m - 0.9158333
tapply(X = data_norm$RuTOPP_2_LexDec, INDEX = data_norm$gender, FUN = mean)  
#f - 0.9066667, m - 0.9316667
tapply(X = data_norm$RuTOPP_4_NWR, INDEX = data_norm$gender, FUN = mean) 
#f - 0.8033333, m - 0.8150000
tapply(X = data_norm$RuTOPP_7_PhonIsol, INDEX = data_norm$gender, FUN = mean, na.rm = TRUE) 
#f - 0.7318182, m - 0.6841667
tapply(X = data_norm$RuTOPP_8_PhonDet, INDEX = data_norm$gender, FUN = mean, na.rm = TRUE) 
#f - 0.8166667, m - 0.8241667
tapply(X = data_norm$RuTOPP_9_NumOfSounds, INDEX = data_norm$gender, FUN = mean, na.rm = TRUE) 
#f - 0.8833333, m - 0.8380000
tapply(X = data_norm$RuTOPP_10_ChangingSound, INDEX = data_norm$gender, FUN = mean, na.rm = TRUE)
#f - 0.93, m - 0.66

##стандартное отклонение за каждый тест в группе нормы
tapply(X = data_norm$RuTOPP_1_PhonDiscr, INDEX = data_norm$gender, FUN = sd) 
#f - 0.14162210, m - 0.07329372
tapply(X = data_norm$RuTOPP_2_LexDec, INDEX = data_norm$gender, FUN = sd)  
#f - 0.07679173, m - 0.04529365
tapply(X = data_norm$RuTOPP_4_NWR, INDEX = data_norm$gender, FUN = sd) 
#f - 0.1138047, m - 0.1118034
tapply(X = data_norm$RuTOPP_7_PhonIsol, INDEX = data_norm$gender, FUN = sd, na.rm = TRUE) 
#f - 0.1011749, m - 0.2189835
tapply(X = data_norm$RuTOPP_8_PhonDet, INDEX = data_norm$gender, FUN = sd, na.rm = TRUE) 
#f - 0.1488339, m - 0.1178179
tapply(X = data_norm$RuTOPP_9_NumOfSounds, INDEX = data_norm$gender, FUN = sd, na.rm = TRUE) 
#f - 0.04618802, m - 0.17195930
tapply(X = data_norm$RuTOPP_10_ChangingSound, INDEX = data_norm$gender, FUN = sd, na.rm = TRUE)
#f - 0.02828427, m - 0.16822604

mean(data_norm$age) #76.25
sd(data_norm$age) #4.513265
tapply(X = data_norm$age, INDEX = data_norm$gender, FUN = mean)
#f - 77.0, m - 75.5

##средние баллы за каждый тест в группе риска
tapply(X = data_risk$RuTOPP_1_PhonDiscr, INDEX = data_risk$gender, FUN = mean)
#f - 0.9125, m - 0.8700
tapply(X = data_risk$RuTOPP_2_LexDec, INDEX = data_risk$gender, FUN = mean)
#f - 0.955, m - 0.790
tapply(X = data_risk$RuTOPP_4_NWR, INDEX = data_risk$gender, FUN = mean)
#f - 0.705, m - 0.795
tapply(X = data_risk$RuTOPP_7_PhonIsol, INDEX = data_risk$gender, FUN = mean, na.rm = TRUE)
#f - 0.7675, 0.5975
tapply(X = data_risk$RuTOPP_8_PhonDet, INDEX = data_risk$gender, FUN = mean, na.rm = TRUE)
#f - 0.8275, m - 0.6950
tapply(X = data_risk$RuTOPP_9_NumOfSounds, INDEX = data_risk$gender, FUN = mean, na.rm = TRUE)
#f - 0.79, m - 0.83
tapply(X = data_risk$RuTOPP_10_ChangingSound, INDEX = data_risk$gender, FUN = mean, na.rm = TRUE)
#f - NA, m - NA

##стандартное отклонение за каждый тест в группе нормы
tapply(X = data_risk$RuTOPP_1_PhonDiscr, INDEX = data_risk$gender, FUN = sd) 
#f - 0.05377422, m - 0.06928203
tapply(X = data_risk$RuTOPP_2_LexDec, INDEX = data_risk$gender, FUN = sd)  
#f - 0.06137318, m - 0.19882991
tapply(X = data_risk$RuTOPP_4_NWR, INDEX = data_risk$gender, FUN = sd) 
#f - 0.22883764, m - 0.07325754
tapply(X = data_risk$RuTOPP_7_PhonIsol, INDEX = data_risk$gender, FUN = sd, na.rm = TRUE) 
#f - 0.05560276, m - 0.33270357
tapply(X = data_risk$RuTOPP_8_PhonDet, INDEX = data_risk$gender, FUN = sd, na.rm = TRUE) 
#f - 0.12230427, m - 0.09110434
tapply(X = data_risk$RuTOPP_9_NumOfSounds, INDEX = data_risk$gender, FUN = sd, na.rm = TRUE) 
#f - NA, m - NA
tapply(X = data_risk$RuTOPP_10_ChangingSound, INDEX = data_risk$gender, FUN = sd, na.rm = TRUE)
#f - NA, m - NA

mean(data_risk$age) #76.75
sd(data_risk$age) #4.891684
tapply(X = data_risk$age, INDEX = data_risk$gender, FUN = mean)
#f - 80.0, m - 73.5

#более простые субтесты (фонематическое восприятие)
hist(data_norm$RuTOPP_1_PhonDiscr)
shapiro.test(data_norm$RuTOPP_1_PhonDiscr) #не норм
hist(data_norm$RuTOPP_2_LexDec)
shapiro.test(data_norm$RuTOPP_2_LexDec) #не норм
hist(data_norm$RuTOPP_8_PhonDet)
shapiro.test(data_norm$RuTOPP_8_PhonDet) #не норм

#более сложные субтесты (фонологическая обработка и артикуляция)
hist(data_norm$RuTOPP_4_NWR)
shapiro.test(data_norm$RuTOPP_4_NWR) #не норм
hist(data_norm$RuTOPP_7_PhonIsol)
shapiro.test(data_norm$RuTOPP_7_PhonIsol) #не норм
hist(data_norm$RuTOPP_9_NumOfSounds)
shapiro.test(data_norm$RuTOPP_9_NumOfSounds) #норм
hist(data_norm$RuTOPP_10_ChangingSound)
shapiro.test(data_norm$RuTOPP_10_ChangingSound) #норм

#более простые субтесты (фонематическое восприятие)
hist(data_risk$RuTOPP_1_PhonDiscr)
shapiro.test(data_risk$RuTOPP_1_PhonDiscr) #норм
hist(data_risk$RuTOPP_2_LexDec)
shapiro.test(data_risk$RuTOPP_2_LexDec) #не норм
hist(data_risk$RuTOPP_8_PhonDet)
shapiro.test(data_risk$RuTOPP_8_PhonDet) #норм

#более сложные субтесты (фонологическая обработка и артикуляция)
hist(data_risk$RuTOPP_4_NWR)
shapiro.test(data_risk$RuTOPP_4_NWR) #не норм
hist(data_risk$RuTOPP_7_PhonIsol)
shapiro.test(data_risk$RuTOPP_7_PhonIsol) #норм
hist(data_risk$RuTOPP_9_NumOfSounds)
shapiro.test(data_risk$RuTOPP_9_NumOfSounds) #два результата
hist(data_risk$RuTOPP_10_ChangingSound) 
shapiro.test(data_risk$RuTOPP_10_ChangingSound) #NA

#дисперсии не равны
var.test(data_norm$RuTOPP_9_NumOfSounds, data_risk$RuTOPP_9_NumOfSounds)

#оценка результатов на наличие значимой разницы
wilcox.test(data_norm$RuTOPP_1_PhonDiscr,data_risk$RuTOPP_1_PhonDiscr, paired = F, exact = F, conf.int = TRUE)
wilcox.test(data_norm$RuTOPP_2_LexDec,data_risk$RuTOPP_2_LexDec, paired = F, exact = F, conf.int = TRUE)
wilcox.test(data_norm$RuTOPP_8_PhonDet,data_risk$RuTOPP_8_PhonDet, paired = F, exact = F, conf.int = TRUE)
#тесты не значимые 
#(нет статистически значимых различий между результатами группы нормы и группы наследственного риска)

wilcox.test(data_norm$RuTOPP_4_NWR,data_risk$RuTOPP_4_NWR, paired = F, exact = F, conf.int = TRUE)
wilcox.test(data_norm$RuTOPP_7_PhonIsol,data_risk$RuTOPP_7_PhonIsol, paired = F, exact = F, conf.int = TRUE)
t.test(data_norm$RuTOPP_9_NumOfSounds,data_risk$RuTOPP_9_NumOfSounds, var.equal = F)
#в последнем субтесте нет данных для группы риска, поэтому тест не удалось выполнить
#средние результаты не различаются значимо

#проверка результатов мальчиков-участников
d_norm <- data_norm %>% 
  filter(gender == "m")

d_risk <- data_risk %>% 
  filter(gender == "m")

shapiro.test(d_norm$RuTOPP_1_PhonDiscr) # не норм
shapiro.test(d_norm$RuTOPP_2_LexDec) #не норм
shapiro.test(d_norm$RuTOPP_8_PhonDet) #не норм
shapiro.test(d_norm$RuTOPP_4_NWR) #не норм
shapiro.test(d_norm$RuTOPP_7_PhonIsol) #норм
shapiro.test(d_norm$RuTOPP_9_NumOfSounds) #норм
shapiro.test(d_norm$RuTOPP_10_ChangingSound) #норм

shapiro.test(d_risk$RuTOPP_1_PhonDiscr) #норм
shapiro.test(d_risk$RuTOPP_2_LexDec) #норм
shapiro.test(d_risk$RuTOPP_8_PhonDet) #не норм
shapiro.test(d_risk$RuTOPP_4_NWR) #норм
shapiro.test(d_risk$RuTOPP_7_PhonIsol) #норм
shapiro.test(d_risk$RuTOPP_9_NumOfSounds) #недостаточно данных
shapiro.test(d_risk$RuTOPP_10_ChangingSound) #недостаточно данных

#оценка дисперсий
var.test(d_norm$RuTOPP_7_PhonIsol, d_risk$RuTOPP_7_PhonIsol) #не равны
var.test(d_norm$RuTOPP_9_NumOfSounds, d_risk$RuTOPP_9_NumOfSounds) #недостаточно наблюдений
var.test(d_norm$RuTOPP_10_ChangingSound, d_risk$RuTOPP_10_ChangingSound) #недостаточно наблюдений

wilcox.test(d_norm$RuTOPP_1_PhonDiscr,d_risk$RuTOPP_1_PhonDiscr, paired = F, exact = F, conf.int = TRUE)
wilcox.test(d_norm$RuTOPP_2_LexDec,d_risk$RuTOPP_2_LexDec, paired = F, exact = F, conf.int = TRUE)
wilcox.test(d_norm$RuTOPP_8_PhonDet,d_risk$RuTOPP_8_PhonDet, paired = F, exact = F, conf.int = TRUE)

wilcox.test(d_norm$RuTOPP_4_NWR,d_risk$RuTOPP_4_NWR, paired = F, exact = F, conf.int = TRUE)
t.test(d_norm$RuTOPP_7_PhonIsol,d_risk$RuTOPP_7_PhonIsol, var.equal = F)
#t-тесты для сравнения результатов последних двух субтестов провести не удалось,
#так как для этого недостаточно данных у участников группы риска

df1 <- data_norm %>% 
  dplyr::select("N":"RuTOPP_1_PhonDiscr", "RuTOPP_2_LexDec", "RuTOPP_4_NWR", "RuTOPP_7_PhonIsol", "RuTOPP_8_PhonDet", "RuTOPP_9_NumOfSounds", "RuTOPP_10_ChangingSound") %>% 
  pivot_longer(cols = c("RuTOPP_1_PhonDiscr", "RuTOPP_2_LexDec", "RuTOPP_4_NWR", "RuTOPP_7_PhonIsol", "RuTOPP_8_PhonDet", "RuTOPP_9_NumOfSounds", "RuTOPP_10_ChangingSound"),
               names_to = "Subtest",
               values_to = "Correct")

df2 <- data_risk %>% 
  dplyr::select("N":"RuTOPP_1_PhonDiscr", "RuTOPP_2_LexDec", "RuTOPP_4_NWR", "RuTOPP_7_PhonIsol", "RuTOPP_8_PhonDet", "RuTOPP_9_NumOfSounds", "RuTOPP_10_ChangingSound") %>% 
  pivot_longer(cols = c("RuTOPP_1_PhonDiscr", "RuTOPP_2_LexDec", "RuTOPP_4_NWR", "RuTOPP_7_PhonIsol", "RuTOPP_8_PhonDet", "RuTOPP_9_NumOfSounds", "RuTOPP_10_ChangingSound"),
               names_to = "Subtest",
               values_to = "Correct")

dfnew <- rbind(df1, df2)

ggplot(dfnew, aes(x = Subtest, y = Correct, color = group )) + 
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(labels=c("Дискриминация фонем","Замена звука в псевдослове","Лексическое решение","Повторение псевдослов", "Первый звук в слове", "Наличие звука в слове", "Количество звуков в слове")) +
  labs(title = "Рисунок 1. Разброс результатов по каждому субтесту в зависимости от группы участников",
       subtitle = "Красным отмечены результаты детей из группы нормы, синим - результаты детей из группы риска",
       x = "Название субтеста",
       y = "Результаты") +
  theme(plot.title = element_text(hjust = 0.7, size = 10),
        plot.subtitle = element_text(hjust = 0.95, size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


ggplot(dfnew, aes(x = Correct, y = age, color = group)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Subtest, scale = "free")
  


df <- rbind(data_norm, data_risk)


#регрессионный анализ
lm1 <- lm(formula = log(RuTOPP_1_PhonDiscr) ~ group, data = df)
#зависимая перемнная - баллы, независимая - группа норма или риск
#за ноль берется группа нормы
qqnorm(residuals(lm1))
summary(lm1)
#не значимая модель

lm2 <- lm(formula = log(RuTOPP_2_LexDec) ~ group, data = df)
#зависимая перемнная - баллы, независимая - группа норма или риск
#за ноль берется группа нормы
qqnorm(residuals(lm2))
summary(lm2)
#не значимая модель

lm3 <- lm(formula = log(RuTOPP_8_PhonDet) ~ group, data = df)
#зависимая перемнная - баллы, независимая - группа норма или риск
#за ноль берется группа нормы
qqnorm(residuals(lm3))
summary(lm3)
#не значимая модель

lm4 <- lm(formula = log(RuTOPP_7_PhonIsol) ~ group, data = df)
#зависимая перемнная - баллы, независимая - группа норма или риск
#за ноль берется группа нормы
qqnorm(residuals(lm4))
summary(lm4)
#не значимая модель

lm5 <- lm(formula = log(RuTOPP_4_NWR) ~ group, data = df)
#зависимая перемнная - баллы, независимая - группа норма или риск
#за ноль берется группа нормы
qqnorm(residuals(lm5))
summary(lm5)
#не значимая модель

lm6 <- lm(formula = log(RuTOPP_9_NumOfSounds) ~ group, data = df)
#зависимая перемнная - баллы, независимая - группа норма или риск
#за ноль берется группа нормы
qqnorm(residuals(lm6))
summary(lm6)
#не значимая модель



tab_model(lm1, transform = NULL, show.se = TRUE, show.ci = FALSE, show.stat = TRUE, auto.label = FALSE, file = "lm1.html")
tab_model(lm2, transform = NULL, show.se = TRUE, show.ci = FALSE, show.stat = TRUE, auto.label = FALSE, file = "lm2.html")
tab_model(lm3, transform = NULL, show.se = TRUE, show.ci = FALSE, show.stat = TRUE, auto.label = FALSE, file = "lm3.html")
tab_model(lm4, transform = NULL, show.se = TRUE, show.ci = FALSE, show.stat = TRUE, auto.label = FALSE, file = "lm4.html")
tab_model(lm5, transform = NULL, show.se = TRUE, show.ci = FALSE, show.stat = TRUE, auto.label = FALSE, file = "lm5.html")
tab_model(lm6, transform = NULL, show.se = TRUE, show.ci = FALSE, show.stat = TRUE, auto.label = FALSE, file = "lm6.html")


