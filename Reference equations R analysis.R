#Import dataset
install.packages("readr")
library(readr)

SEL2020 <- read_csv("~/Desktop/Valores referencia espirometria/Equacoes 2020/SEL2020.csv")

#View dataset

View(SEL2020)

#Installing and loading the necessary packages

install.packages("tidyverse")
library(tidyverse)

#Descriptive analysis for Derivation Cohort ARIA:

install.packages("dplyr")
library(dplyr)

#Selecting subset with only ARIA participants

ARIA <- subset(SEL2020, Project == 0)

# n and % for Gender ARIA:
table(ARIA$Gender)
ggplot(data=ARIA) + geom_bar(mapping = aes(x = Gender, fill = Gender))

#Males:
 n=267
(267/481)*100

#Females:
 n=214
(214/481)*100

# Age range:
summary(ARIA$Idade)

# Two-samples T-test gender comparissons in age, height, weight and BMI ARIA cohort:
install.packages("ggpubr")
ARIA_Boys <- subset(ARIA, Gender == "Male")
ARIA_Girls <- subset(ARIA, Gender == "Female")

#Age:
t.test(ARIA$Idade ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$Idade)
sd(ARIA_Boys$Idade)

mean(ARIA_Girls$Idade)
sd(ARIA_Girls$Idade)

#Height:
t.test(ARIA$Altura_cm ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$Altura_cm)
sd(ARIA_Boys$Altura_cm)

mean(ARIA_Girls$Altura_cm)
sd(ARIA_Girls$Altura_cm)

#Weight:
t.test(ARIA$Peso ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$Peso)
sd(ARIA_Boys$Peso)

mean(ARIA_Girls$Peso)
sd(ARIA_Girls$Peso)

#BMI:
t.test(ARIA$BMI ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$BMI)
sd(ARIA_Boys$BMI)

mean(ARIA_Girls$BMI)
sd(ARIA_Girls$BMI)

#Comparisson two-sample t-test spirometric parameters between Male and Female participants in ARIA:

#FVC_pre:
t.test(ARIA$FVC_pre ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$FVC_pre)
sd(ARIA_Boys$FVC_pre)

mean(ARIA_Girls$FVC_pre)
sd(ARIA_Girls$FVC_pre)

#FEV1_pre:
t.test(ARIA$FEV1_pre ~ ARIA$Gender, var.equal = TRUE)

mean(ARIA_Boys$FEV1_pre)
sd(ARIA_Boys$FEV1_pre)

mean(ARIA_Girls$FEV1_pre)
sd(ARIA_Girls$FEV1_pre)

#FEF2575_pre:
t.test(ARIA$FEF2575_pre ~ ARIA$Gender, var.equal = TRUE)
  #Mean and SD are computed together because no sig. differences were found between Male and Female:
mean(ARIA$FEF2575_pre)
sd(ARIA$FEF2575_pre)


#FEV1FVC_pre:
t.test(ARIA$FEV1FVC_pre ~ ARIA$Gender, var.equal = TRUE)
  #Mean and SD are computed together because no sig. differences were found between Male and Female:
mean(ARIA$FEV1FVC_pre)
sd(ARIA$FEV1FVC_pre)


#Descriptive analysis for Validation Cohort G21:
#Selecting subset with only G21 participants:
G21 <- subset(SEL2020, Project == 1)
G21_Girls <- subset(G21, Gender == "Female")
G21_Boys <- subset(G21, Gender == "Male")

# n and % for Gender G21:
table(G21$Gender)
ggplot(data=G21) + geom_bar(mapping = aes(x = Gender, fill = Gender))

#Males:
n=1538
(1538/2986)*100

#Females:
n=1448
(1448/2986)*100

# Age range:
summary(G21$Idade)

# Two-samples T-test gender comparissons in age, height, weight and BMI G21 cohort:
#Age:
t.test(G21$Idade ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$Idade)
sd(G21_Boys$Idade)

mean(G21_Girls$Idade)
sd(G21_Girls$Idade)

#Height:
t.test(G21$Altura_cm ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$Altura_cm)
sd(G21_Boys$Altura_cm)

mean(G21_Girls$Altura_cm)
sd(G21_Girls$Altura_cm)

#Weight:
t.test(G21$Peso ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$Peso)
sd(G21_Boys$Peso)

mean(G21_Girls$Peso)
sd(G21_Girls$Peso)

#BMI:
t.test(G21$BMI ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$BMI)
sd(G21_Boys$BMI)

mean(G21_Girls$BMI)
sd(G21_Girls$BMI)

#Comparisson two-sample t-test spirometric parameters between Male and Female participants in G21:

#FVC_pre:
t.test(G21$FVC_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FVC_pre)
sd(G21_Boys$FVC_pre)

mean(G21_Girls$FVC_pre)
sd(G21_Girls$FVC_pre)

#FEV1_pre:
t.test(G21$FEV1_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FEV1_pre)
sd(G21_Boys$FEV1_pre)

mean(G21_Girls$FEV1_pre)
sd(G21_Girls$FEV1_pre)

#FEF2575_pre:
t.test(G21$FEF2575_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FEF2575_pre)
sd(G21_Boys$FEF2575_pre)

mean(G21_Girls$FEF2575_pre)
sd(G21_Girls$FEF2575_pre)


#FEV1FVC_pre:
t.test(G21$FEV1FVC_pre ~ G21$Gender, var.equal = TRUE)

mean(G21_Boys$FEV1FVC_pre)
sd(G21_Boys$FEV1FVC_pre)

mean(G21_Girls$FEV1FVC_pre)
sd(G21_Girls$FEV1FVC_pre)

#Comparissons between Derivation and Validation cohorts:
#Two-sample t-test comparissons between ARIA and G21 participants:

#Age:
t.test(SEL2020$Idade ~ SEL2020$Project, var.equal = TRUE)

mean(G21$Idade)
sd(G21$Idade)

mean(ARIA$Idade)
sd(ARIA$Idade)

#Height:
t.test(SEL2020$Altura_cm ~ SEL2020$Project, var.equal = TRUE)

mean(G21$Altura_cm)
sd(G21$Altura_cm)

mean(ARIA$Altura_cm)
sd(ARIA$Altura_cm)

#Weight:
t.test(SEL2020$Peso ~ SEL2020$Project, var.equal = TRUE)

mean(G21$Peso)
sd(G21$Peso)

mean(ARIA$Peso)
sd(ARIA$Peso)

#BMI:
t.test(SEL2020$BMI ~ SEL2020$Project, var.equal = TRUE)

mean(G21$BMI)
sd(G21$BMI)

mean(ARIA$BMI)
sd(ARIA$BMI)

#FVC:
t.test(SEL2020$FVC_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FVC_pre)
sd(G21$FVC_pre)

mean(ARIA$FVC_pre)
sd(ARIA$FVC_pre)

#FEV1:
t.test(SEL2020$FEV1_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FEV1_pre)
sd(G21$FEV1_pre)

mean(ARIA$FEV1_pre)
sd(ARIA$FEV1_pre)

#FEF2575:
t.test(SEL2020$FEF2575_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FEF2575_pre)
sd(G21$FEF2575_pre)

mean(ARIA$FEF2575_pre)
sd(ARIA$FEF2575_pre)

#FEV1/FVC:
t.test(SEL2020$FEV1FVC_pre ~ SEL2020$Project, var.equal = TRUE)

mean(G21$FEV1FVC_pre)
sd(G21$FEV1FVC_pre)

mean(ARIA$FEV1FVC_pre)
sd(ARIA$FEV1FVC_pre)

#Levene's test of variance between ARIA and G21:
install.packages("car")
install.packages("reshape2")
install.packages("hms")

library(car)
library(reshape2)
library(hms)

#Age:
leveneTest(SEL2020$Idade, SEL2020$Project, center = mean)

#Height:
leveneTest(SEL2020$Altura_cm, SEL2020$Project, center = mean)

#Weight:
leveneTest(SEL2020$Peso, SEL2020$Project, center = mean)

#BMI:
leveneTest(SEL2020$BMI, SEL2020$Project, center = mean)

#FVC:
leveneTest(SEL2020$FVC_pre, SEL2020$Project, center = mean)

#FEV1:
leveneTest(SEL2020$FEV1_pre, SEL2020$Project, center = mean)

#FEF2575:
leveneTest(SEL2020$FEF2575_pre, SEL2020$Project, center = mean)

#FEV1FVC:
leveneTest(SEL2020$FEV1FVC_pre, SEL2020$Project, center = mean)

#Pearson's correlation coefficient ARIA cohort between antropometric and lung function parameters with graphs:
install.packages("ggpubr")
library(ggpubr)

#FVC and Age ARIA:
cor.test(ARIA$FVC_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)")

#FVC and Age ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)")

#FVC and Age ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Age (years)")

#FVC and Height ARIA:
cor.test(ARIA$FVC_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)", color = "Gender")

ggscatter(ARIA, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)")

#FVC and Height ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)")

#FVC and Height ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Height (cm)")

#FVC and Weight ARIA:
cor.test(ARIA$FVC_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Weight (kg)")

#FVC and Weight ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Weight (kg)")

#FVC and Height ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Weight (kg)")

#FVC and BMI ARIA:
cor.test(ARIA$FVC_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index")

#FVC and BMI ARIA_Boys:
cor.test(ARIA_Boys$FVC_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index")

#FVC and BMI ARIA_Girls:
cor.test(ARIA_Girls$FVC_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FVC (L)", ylab = "Body Mass Index")

#FEV1 and Age ARIA:
cor.test(ARIA$FEV1_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)")

#FEV1 and Age ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)")

#FEV1 and Age ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Age (years)")

#FEV1 and Height ARIA:
cor.test(ARIA$FEV1_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)")

#FEV1 and Height ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)")

#FEV1 and Height ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Height (cm)")

#FEV1 and Weight ARIA:
cor.test(ARIA$FEV1_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)")

#FEV1 and Weight ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)")

#FEV1 and weight ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Weight (kg)")

#FEV1 and BMI ARIA:
cor.test(ARIA$FEV1_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index")

#FEV1 and BMI ARIA_Boys:
cor.test(ARIA_Boys$FEV1_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index")

#FEV1 and BMI ARIA_Girls:
cor.test(ARIA_Girls$FEV1_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1 (L)", ylab = "Body Mass Index")

#FEF2575 and Age ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)")

#FEF2575 and Age ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)")

#FEF2575 and Age ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Age (years)")

#FEF2575 and Height ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)", color = "Gender")

ggscatter(ARIA, x = "FEV1_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)")

#FEF2575 and Height ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)")

#FEF2575 and Height ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Height (cm)")

#FEF2575 and Weight ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)")

#FEF2575 and Weight ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)")

#FEF2575 and Weight ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Weight (kg)")

#FEF2575 and BMI ARIA:
cor.test(ARIA$FEF2575_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index")

#FEF2575 and BMI ARIA_Boys:
cor.test(ARIA_Boys$FEF2575_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index")

#FEF2575 and BMI ARIA_Girls:
cor.test(ARIA_Girls$FEF2575_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FEF2575_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEF2575 (L/s)", ylab = "Body Mass Index")

#FEV1/FVC and Age ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$Idade, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)")

#FEV1/FVC and Age ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$Idade, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)")

#FEV1/FVC and Age ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$Idade, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "Idade", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Age (years)")

#FEV1/FVC and Height ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$Altura_cm, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Height (cm)", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Height (cm)")

#FEV1/FVC and Height ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$Altura_cm, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1FVC", ylab = "Height (cm)")

#FEV1/FVC and Height ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$Altura_cm, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "Altura_cm", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Height (cm)")

#FEV1/FVC and Weight ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$Peso, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)")

#FEV1/FVC and Weight ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$Peso, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)")

#FEV1/FVC and Weight ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$Peso, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "Peso", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Weight (kg)")

#FEV1/FVC and BMI ARIA:
cor.test(ARIA$FEV1FVC_pre, ARIA$BMI, method = "pearson")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index", color = "Gender")

ggscatter(ARIA, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index")

#FEV1/FVC and BMI ARIA_Boys:
cor.test(ARIA_Boys$FEV1FVC_pre, ARIA_Boys$BMI, method = "pearson")

ggscatter(ARIA_Boys, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index")

#FEV1/FVC and BMI ARIA_Girls:
cor.test(ARIA_Girls$FEV1FVC_pre, ARIA_Girls$BMI, method = "pearson")

ggscatter(ARIA_Girls, x = "FEV1FVC_pre", y = "BMI", add = "reg.line", 
          conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", 
          xlab = "FEV1/FVC", ylab = "Body Mass Index")

#Spirometry predictive models:

#Multiple Linear Regression FVC Boys: (FVC_bmodel_2 selected based on lower residuals!!)
  #FVC_bmodel 1 Age/Height/Weight/BMI
FVC_bmodel_1 <- lm(FVC_pre ~ Idade + Altura_cm + Peso + BMI, data = ARIA_Boys)
summary(FVC_bmodel_1)
  #CI for the variables in FVC_bmodel_1:
confint(FVC_bmodel_1)
  #Residuals for FVC_bmodel_1:
sigma(FVC_bmodel_1)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_1)/mean(G21_Boys$FVC_pre)

  #FVC_bmodel_2 Height/Weight:
FVC_bmodel_2 <- lm(FVC_pre ~ Altura_cm + Peso, data = ARIA_Boys)
summary(FVC_bmodel_2)
  #CI for the variables in FVC_bmodel_2:
confint(FVC_bmodel_2)
  #Residuals for FVC_bmodel_2:
sigma(FVC_bmodel_2)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_2)/mean(G21_Boys$FVC_pre)

  #FVC_bmodel_3 Height/Age:
FVC_bmodel_3 <- lm(FVC_pre ~ Altura_cm + Idade, data = ARIA_Boys)
summary(FVC_bmodel_3)
  #CI for the variables in FVC_bmodel_3:
confint(FVC_bmodel_3)
  #Residuals for FVC_bmodel_3:
sigma(FVC_bmodel_3)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_3)/mean(G21_Boys$FVC_pre)

  #FVC_bmodel_4 Height/BMI:
FVC_bmodel_4 <- lm(FVC_pre ~ Altura_cm + BMI, data = ARIA_Boys)
summary(FVC_bmodel_4)
  #CI for the variables in FVC_bmodel_2:
confint(FVC_bmodel_4)
  #Residuals for FVC_bmodel_4:
sigma(FVC_bmodel_4)/mean(ARIA_Boys$FVC_pre)
sigma(FVC_bmodel_4)/mean(G21_Boys$FVC_pre)

#Multiple Linear Regression FVC Girls: (FVC_gmodel_1 selected based on lower residuals!!)
  #FVC_gmodel_1 Age/Height/Weight/BMI
FVC_gmodel_1 <- lm(FVC_pre ~ Idade + Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FVC_gmodel_1)
  #CI for the variables in FVC_gmodel_1:
confint(FVC_gmodel_1)
  #Residuals for FVC_model_1:
sigma(FVC_gmodel_1)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_1)/mean(G21_Girls$FVC_pre)

  #FVC_gmodel_2 Age/Height/BMI
FVC_gmodel_2 <- lm(FVC_pre ~ Idade + Altura_cm + BMI, data = ARIA_Girls)
summary(FVC_gmodel_2)
  #CI for the variables in FVC_gmodel_2:
confint(FVC_gmodel_2)
  #Residuals for FVC_model_2:
sigma(FVC_gmodel_2)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_2)/mean(G21_Girls$FVC_pre)

  #FVC_gmodel_3 Height/Weight/BMI
FVC_gmodel_3 <- lm(FVC_pre ~ Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FVC_gmodel_3)
  #CI for the variables in FVC_gmodel_3:
confint(FVC_gmodel_3)
  #Residuals for FVC_model_3:
sigma(FVC_gmodel_3)/mean(ARIA_Girls$FVC_pre)
sigma(FVC_gmodel_3)/mean(G21_Girls$FVC_pre)

#Multiple Linear Regression FEV1 Boys: (FEV1_bmodel_2 selected based on lower residuals!!)
  #FEV1_bmodel 1 Age/Height/Weight/BMI
FEV1_bmodel_1 <- lm(FEV1_pre ~ Idade + Altura_cm + Peso + BMI, data = ARIA_Boys)
summary(FEV1_bmodel_1)
  #CI for the variables in FEV1_bmodel_1:
confint(FEV1_bmodel_1)
  #Residuals for FEV1_bmodel_1:
sigma(FEV1_bmodel_1)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_1)/mean(G21_Boys$FEV1_pre)

  #FEV1_bmodel_2 Height/Weight:
FEV1_bmodel_2 <- lm(FEV1_pre ~ Altura_cm + Peso, data = ARIA_Boys)
summary(FEV1_bmodel_2)
  #CI for the variables in FEV1_bmodel_2:
confint(FEV1_bmodel_2)
  #Residuals for FEV1_bmodel_2:
sigma(FEV1_bmodel_2)/mean(ARIA_Boys$FVC_pre)
sigma(FEV1_bmodel_2)/mean(G21_Boys$FVC_pre)

  #FEV1_bmodel_3 Height/Age:
FEV1_bmodel_3 <- lm(FEV1_pre ~ Altura_cm + Idade, data = ARIA_Boys)
summary(FEV1_bmodel_3)
  #CI for the variables in FEV1_bmodel_3:
confint(FEV1_bmodel_3)
  #Residuals for FEV1_bmodel_3:
sigma(FEV1_bmodel_3)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_3)/mean(G21_Boys$FEV1_pre)

  #FEV1_bmodel_4 Height/BMI:
FEV1_bmodel_4 <- lm(FEV1_pre ~ Altura_cm + BMI, data = ARIA_Boys)
summary(FEV1_bmodel_4)
  #CI for the variables in FEV1_bmodel_2:
confint(FEV1_bmodel_4)
  #Residuals for FEV1_bmodel_4:
sigma(FEV1_bmodel_4)/mean(ARIA_Boys$FEV1_pre)
sigma(FEV1_bmodel_4)/mean(G21_Boys$FEV1_pre)

#Multiple Linear Regression FEV1 Girls: (FEV1_gmodel_1 selected based on lower residuals!!)
  #FEV1_gmodel_1 Age/Height/Weight/BMI
FEV1_gmodel_1 <- lm(FEV1_pre ~ Idade + Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FEV1_gmodel_1)
  #CI for the variables in FEV1_gmodel_1:
confint(FEV1_gmodel_1)
  #Residuals for FEV1_model_1:
sigma(FEV1_gmodel_1)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_1)/mean(G21_Girls$FEV1_pre)

  #FEV1_gmodel_2 Age/Height/BMI
FEV1_gmodel_2 <- lm(FEV1_pre ~ Idade + Altura_cm + BMI, data = ARIA_Girls)
summary(FEV1_gmodel_2)
  #CI for the variables in FEV1_gmodel_2:
confint(FEV1_gmodel_2)
  #Residuals for FEV1_model_2:
sigma(FEV1_gmodel_2)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_2)/mean(G21_Girls$FEV1_pre)

  #FEV1_gmodel_3 Height/Weight/BMI
FEV1_gmodel_3 <- lm(FEV1_pre ~ Altura_cm + Peso + BMI, data = ARIA_Girls)
summary(FEV1_gmodel_3)
  #CI for the variables in FEV1_gmodel_3:
confint(FEV1_gmodel_3)
  #Residuals for FVC_model_3:
sigma(FEV1_gmodel_3)/mean(ARIA_Girls$FEV1_pre)
sigma(FEV1_gmodel_3)/mean(G21_Girls$FEV1_pre)

