#Import dataset

library(readr)

SEL2020 <- read_csv("~/Desktop/Valores referencia espirometria/Equacoes 2020/SEL2020.csv")

#View dataset

View(SEL2020)

#Loading the necessary packages

library(tidyverse)
library(dplyr)


#Descriptive analysis for Derivation Cohort ARIA:
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

# Two-samples T-test gender comparissons in age, height and weight G21 cohort:
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

#To be continued...
