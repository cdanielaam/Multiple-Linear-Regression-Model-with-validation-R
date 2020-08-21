#Import dataset

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

# n and % for Gender:
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

# Two-samples T-test gender comparissons in age, height and weight ARIA cohort:
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

#Selecting subset with only G21 participants:
G21 <- subset(SEL2020, Project == 1)

