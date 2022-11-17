# KPA-Analysis

---
title: "KPA Analysis"
author: "Hasitha Sampath"
date: "2022-11-15"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r Packages}
library(readxl) #read excel file_C1
library(ltm) #Cronbach alpha_C2
library(janitor) #to obtain counts and frequency_C3
library(ggplot2) #obtain pie charts_C5

library(dplyr)
library(extrafont) #extrafont families
library(corrplot) #correlation plot_C6
library(ggcorrplot) #Correlation plot ggplot

library(devtools)
library(hrbrthemes) #histogram theme_C8
library(tidyverse)
library(wesanderson)
library(PerformanceAnalytics) #Correlation plot
library(psych) #cor test

library(nnet)
library(reshape2)

library(readr) # Binary Logistic Regression
library(rcompanion)
library(broom)
library(margins)
library(aod)
library(car)
library(caret)
library(pROC)
library(InformationValue)

library(foreign) #CFA
library(lavaan)
library(tidyr)
library(knitr)
library(lavaanPlot)
library(semPlot)
library(kutils)
library(semTable)

library(caTools)
library(e1071)
library(party)
library(ROCR)

options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
   install_github('munoztd0/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

library(caret)
library(randomForest)
library(reprtree)
library(writexl)
library(webshot)
```

```{r Read dataset}
KPA <- read_excel("KPA.xlsx")
KPA
```

```{r Testing Relaibility}
KPA_Reliability <- KPA[9:49] #to omit all socio-demographics factor
cronbach.alpha(KPA_Reliability, na.rm = TRUE)

KPA_Reliability_1 <- KPA[,c(1,3:49)] #to omit VG
cronbach.alpha(KPA_Reliability_1, na.rm = TRUE)

K_Reliability <- KPA[9:28]
cronbach.alpha(K_Reliability, na.rm = TRUE)

P_Reliability <- KPA[29:42]
cronbach.alpha(P_Reliability, na.rm = TRUE)

A_Reliability <- KPA[43:49]
cronbach.alpha(A_Reliability, na.rm = TRUE)
```

```{r Composition of Demographics}
KPA_Demographic <- KPA[1:8] #to obtain socio-demographics factors

tabyl(KPA_Demographic$DT)
tabyl(KPA_Demographic$SX)
tabyl(KPA_Demographic$AG)
tabyl(KPA_Demographic$ED)
tabyl(KPA_Demographic$OC)
tabyl(KPA_Demographic$HI)
tabyl(KPA_Demographic$RG)

DT <- data.frame(Count = c(45, 16, 16, 3),
                 District = c("Kandy", "Gampaha", "Kurunegala", "Matara"))
SX <- data.frame(Count = c(42, 38),
                 Gender = c("Female", "Male"))
AG <- data.frame(Count = c(33, 41, 6),
                 Age = c("18-30", "31-60", "Over 60"))
ED <- data.frame(Count = c(5, 15, 20, 40),
                 Education = c("Primary or Less", "O/L", "A/L", "Degree or Higher"))
OC <- data.frame(Count = c(31, 20, 15, 14),
                 Occupation = c("Student", "Gov_Pvt_Job", "Business", "Other"))
HI <- data.frame(Count = c(5, 59, 16),
                 Income = c("Below 10000", "10000 - 50000", "Over 50000"))
RG <- data.frame(Count = c(67, 2, 3, 8),
                 Religion = c("Buddhist", "Hindu", "Islam", "Christian"))

DT
SX
AG
ED
OC
HI
RG

hsize <-2.5

DT_PLOT <- ggplot(DT, aes(x = hsize, y = Count, fill = District)) +
              ggtitle("Composition of District")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))

SX_PLOT <- ggplot(SX, aes(x = hsize, y = Count, fill = Gender)) +
              ggtitle("Composition of Gender")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))

AG_PLOT <- ggplot(AG, aes(x = hsize, y = Count, fill = Age)) +
              ggtitle("Composition of Age")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))

ED_PLOT <- ggplot(ED, aes(x = hsize, y = Count, fill = Education)) +
              ggtitle("Composition of Level of Education")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))

OC_PLOT <- ggplot(OC, aes(x = hsize, y = Count, fill = Occupation)) +
              ggtitle("Composition of Occupation Category")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))

HI_PLOT <- ggplot(HI, aes(x = hsize, y = Count, fill = Income)) +
              ggtitle("Composition of Household Income")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))

RG_PLOT <- ggplot(RG, aes(x = hsize, y = Count, fill = Religion)) +
              ggtitle("Composition of Religion")+
              geom_col() +
              geom_text(aes(label = Count),
              position = position_stack(vjust = 0.5), 
              family="serif") +
              coord_polar(theta = "y") +
              scale_fill_brewer(palette = "Accent") +
              xlim(c(0.2, hsize + 0.5)) +
              theme(legend.position = "right") +
              theme(text = element_text(size = 12, family="serif")) +
              theme(panel.background = element_rect(fill = "white"),
                    panel.grid = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.title = element_blank(),
                    plot.title = element_text(hjust = 0.5))



DT_PLOT
SX_PLOT
AG_PLOT
ED_PLOT
OC_PLOT
HI_PLOT
RG_PLOT
```

```{r Correlation among KPA}
KPA_Corr <- KPA[50:52] #to filter KPA from dataset

COR_Spearman <- cor(KPA_Corr, method = c("spearman"))
round(COR_Spearman,4)

cor.mtest(KPA_Corr)

ggcorrplot(COR_Spearman, method = "circle", outline.col = "white", lab = TRUE ,colors = c("#17DF4E", "white", "#0DE1E4")) + 
  ggtitle("Corrlation Matrix of Scores among Knowledge, practices, and Attitudes") +
  geom_text(aes(label=""), family="serif") + 
  theme(text = element_text(size = 12, family="serif")) + 
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))
```

```{r Precentage of correct answers for Konwledge}

FREQ_WCC <- round(100*sum(KPA$WCC)/80,2)
FREQ_WCC
FREQ_ER <- round(100*sum(KPA$ER)/240,2)
FREQ_ER
FREQ_VP <- round(100*sum(KPA$VP)/240,2)
FREQ_VP
FREQ_CC <- round(100*sum(KPA$CC)/80,2)
FREQ_CC
FREQ_COV <- round(100*sum(KPA$COV)/80,2)
FREQ_COV
FREQ_SRD <- round(100*sum(KPA$SRD)/80,2)
FREQ_SRD
FREQ_DRA <- round(100*sum(KPA$DRA)/80,2)
FREQ_DRA
FREQ_EWA <- round(100*sum(KPA$EWA)/80,2)
FREQ_EWA
FREQ_DSD <- round(100*sum(KPA$DSD)/80,2)
FREQ_DSD
FREQ_FFD <- round(100*sum(KPA$FFD)/80,2)
FREQ_FFD
FREQ_SCS <- round(100*sum(KPA$SCS)/80,2)
FREQ_SCS
FREQ_SN <- round(100*sum(KPA$SN)/80,2)
FREQ_SN
FREQ_LF <- round(100*sum(KPA$LF)/80,2)
FREQ_LF
FREQ_TS <- round(100*sum(KPA$TS)/80,2)
FREQ_TS
FREQ_PCR <- round(100*sum(KPA$PCR)/80,2)
FREQ_PCR
FREQ_ALC <- round(100*sum(KPA$ALC)/80,2)
FREQ_ALC
FREQ_TMP <- round(100*sum(KPA$TMP)/80,2)
FREQ_TMP
FREQ_SU <- round(100*sum(KPA$SU)/80,2)
FREQ_SU
FREQ_BCG <- round(100*sum(KPA$BCG)/80,2)
FREQ_BCG
FREQ_WH <- round(100*sum(KPA$WH)/80,2)
FREQ_WH

A <- data.frame(rbind(FREQ_WCC, FREQ_ER, FREQ_VP, FREQ_CC, FREQ_COV, FREQ_SRD, FREQ_DRA, FREQ_EWA, FREQ_DSD, FREQ_FFD, FREQ_SCS, FREQ_SN, FREQ_LF, FREQ_TS, FREQ_PCR, FREQ_ALC, FREQ_TMP, FREQ_SU, FREQ_BCG, FREQ_WH))
colnames(A) <- c("Percentage")
B <- data.frame(rbind("WCC", "ER", "VP", "CC", "COV", "SRD", "DRA", "EWA", "DSD", "FFD", "SCS", "SN", "LF", "TS", "PCR", "ALC", "TMP", "SU", "BCG","WH"))
colnames(B) <- c("Category")
PERC_KNOWLEDGE <- data.frame(cbind(B,A))
PERC_KNOWLEDGE

BAR_PERC_KNOWLEDGE <- ggplot(data=PERC_KNOWLEDGE, aes(x=Category, y=Percentage, fill=Category)) + 
  geom_bar(stat="identity") + 
  ggtitle("Composition of Correct Answers for the Questions on Knowledge towards COVID-19") + 
  geom_text(aes(label = sprintf("%0.2f", round(Percentage,digits = 2))), hjust=1.1, vjust=0.5, color="white", angle=90, family="serif") + 
  theme_minimal() + 
  theme(text = element_text(size = 12, family="serif")) + 
  theme(legend.position = "NONE") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))
  
BAR_PERC_KNOWLEDGE
```

```{r Distibution of Scores of KPA}
KNOWLEDGE_TOTAL <- data.frame(value = KPA$Knowledge)
PRACTICES_TOTAL <- data.frame(value = KPA$Practices)
ATTITUDE_TOTAL <- data.frame(value = KPA$Attitude)

HIST_KNOWLEDGE_TOTAL <- ggplot(KNOWLEDGE_TOTAL, aes(x=value)) + 
  xlim(c(0, 24)) + 
  geom_histogram(bins = 15, fill = "#38a8c9") + 
  theme_minimal() + 
  ggtitle("Distribution of Scores for Knowledge") + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12, family = "serif"))

HIST_PRACTICES_TOTAL <- ggplot(PRACTICES_TOTAL, aes(x=value)) + 
  xlim(c(0, 49)) + 
  geom_histogram(bins = 15, fill = "#38a8c9") + 
  theme_minimal() + 
  ggtitle("Distribution of Scores for Practices") + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12, family = "serif"))

HIST_ATTITUDE_TOTAL <- ggplot(ATTITUDE_TOTAL, aes(x=value)) + 
  xlim(c(20, 35)) + 
  geom_histogram(bins = 15, fill = "#38a8c9") + 
  theme_minimal() + 
  ggtitle("Distribution of Scores for Attitude") + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12, family = "serif"))

HIST_KNOWLEDGE_TOTAL
HIST_PRACTICES_TOTAL
HIST_ATTITUDE_TOTAL
```

```{r Factors affecting on levels of KPA}

chisq.test(KPA$K_CAT_BIN, KPA$DT)
chisq.test(KPA$K_CAT_BIN, KPA$SX)
chisq.test(KPA$K_CAT_BIN, KPA$AG)
chisq.test(KPA$K_CAT_BIN, KPA$ED)
chisq.test(KPA$K_CAT_BIN, KPA$OC)
chisq.test(KPA$K_CAT_BIN, KPA$HI)
chisq.test(KPA$K_CAT_BIN, KPA$RG)

chisq.test(KPA$P_CAT_BIN, KPA$DT)
chisq.test(KPA$P_CAT_BIN, KPA$SX)
chisq.test(KPA$P_CAT_BIN, KPA$AG)
chisq.test(KPA$P_CAT_BIN, KPA$ED)
chisq.test(KPA$P_CAT_BIN, KPA$OC)
chisq.test(KPA$P_CAT_BIN, KPA$HI)
chisq.test(KPA$P_CAT_BIN, KPA$RG)

chisq.test(KPA$A_CAT_BIN, KPA$DT)
chisq.test(KPA$A_CAT_BIN, KPA$SX)
chisq.test(KPA$A_CAT_BIN, KPA$AG)
chisq.test(KPA$A_CAT_BIN, KPA$ED)
chisq.test(KPA$A_CAT_BIN, KPA$OC)
chisq.test(KPA$A_CAT_BIN, KPA$HI)
chisq.test(KPA$A_CAT_BIN, KPA$RG)
```

```{r Binary Logistic Regression}
KPA_BLR <- read_xlsx("KPA-BINLOG.xlsx")

KPA_BLR$AG <- factor(KPA_BLR$AG)
KPA_BLR$ED <- factor(KPA_BLR$ED)

table(train_BLR_K$AG)
table(train_BLR_K$ED)
KPA_BLR$AG_RL <- relevel(KPA_BLR$AG, ref = "G18-30")
KPA_BLR$ED_RL <- relevel(KPA_BLR$ED, ref = "O/L")

glimpse(KPA_BLR)

# Knowledge
KPA_BLR$K_CAT_BIN_F <- factor(KPA_BLR$K_CAT_BIN_F)


set.seed(1456320)
Split_BLR_K <- createDataPartition(y = KPA_BLR$K_CAT_BIN_F, p = 0.7, list = FALSE)
train_BLR_K <- KPA_BLR[Split_BLR_K,]
test_BLR_K <- KPA_BLR[-Split_BLR_K,]

BLR_K <- glm(K_CAT_BIN_F ~ AG_RL + ED_RL, family = binomial(), data = train_BLR_K)
summary(BLR_K)

exp(coef(BLR_K))

for (i in 1:length(coef(BLR_K))) {
  wald_test_BLR_K <- wald.test(b = coef(BLR_K), Sigma = vcov(BLR_K), Terms = i)
  print(wald_test_BLR_K)
}

nagelkerke(BLR_K)

tidy(BLR_K, exponentiate = TRUE, conf.level = 0.95)

plot(BLR_K, which = 4, id.n = 3)
plot(abs(residuals(BLR_K)))

pred_BLR_K <- round(predict(BLR_K, test_BLR_K, type="response"))
pred_BLR_K

Concordance(test_BLR_K$K_CAT_BIN_F, pred_BLR_K)

confmat_BLR_K <- confusionMatrix(test_BLR_K$K_CAT_BIN_F, pred_BLR_K)
confmat_BLR_K

acc_BLR_K <- (confmat_BLR_K[1,1]+confmat_BLR_K[2,2])/23
acc_BLR_K

sensitivity(test_BLR_K$K_CAT_BIN_F, pred_BLR_K)
specificity(test_BLR_K$K_CAT_BIN_F, pred_BLR_K)

roc_score_K <- roc(test_BLR_K$K_CAT_BIN_F, pred_BLR_K)
plot(roc_score_K, main="ROC Curve - Logistic Regression for Knowledge", col = "Blue")
roc_score_K


# Practice
KPA_BLR$P_CAT_BIN_F <- factor(KPA_BLR$P_CAT_BIN_F)

set.seed(92169)
Split_BLR_P <- createDataPartition(y = KPA_BLR$P_CAT_BIN_F, p = 0.7, list = FALSE)
train_BLR_P <- KPA_BLR[Split_BLR_P,]
test_BLR_P <- KPA_BLR[-Split_BLR_P,]

BLR_P <- glm(P_CAT_BIN_F ~ AG_RL, family = binomial(), data = train_BLR_P)
summary(BLR_P)

exp(coef(BLR_P))

for (i in 1:length(coef(BLR_P))) {
  wald_test_BLR_P <- wald.test(b = coef(BLR_P), Sigma = vcov(BLR_P), Terms = i)
  print(wald_test_BLR_P)
}

nagelkerke(BLR_P)

tidy(BLR_P, exponentiate = TRUE, conf.level = 0.95)

plot(BLR_P, which = 4, id.n = 3)
plot(abs(residuals(BLR_P)))

pred_BLR_P <- round(predict(BLR_P, test_BLR_P, type="response"))
pred_BLR_P

confmat_BLR_P <- confusionMatrix(test_BLR_P$P_CAT_BIN_F, pred_BLR_P)
confmat_BLR_P

acc_BLR_P <- (confmat_BLR_P[1,1]+confmat_BLR_P[2,2])/23
acc_BLR_P

sensitivity(test_BLR_P$P_CAT_BIN_F, pred_BLR_P)
specificity(test_BLR_P$P_CAT_BIN_F, pred_BLR_P)

roc_score_P <- roc(test_BLR_P$P_CAT_BIN_F, pred_BLR_P)
plot(roc_score_P, main="ROC Curve - Logistic Regression for Practice", col = "Blue")
roc_score_P


# Attitude
KPA_BLR$A_CAT_BIN_F <- factor(KPA_BLR$A_CAT_BIN_F)

set.seed(111)
Split_BLR_A <- createDataPartition(y = KPA_BLR$A_CAT_BIN_F, p = 0.7, list = FALSE)
train_BLR_A <- KPA_BLR[Split_BLR_A,]
test_BLR_A <- KPA_BLR[-Split_BLR_A,]

BLR_A <- glm(A_CAT_BIN_F ~ AG_RL + ED_RL, family = binomial(), data = train_BLR_A)
summary(BLR_A)

exp(coef(BLR_A))

for (i in 1:length(coef(BLR_A))) {
  wald_test_BLR_A <- wald.test(b = coef(BLR_A), Sigma = vcov(BLR_A), Terms = i)
  print(wald_test_BLR_A)
}

nagelkerke(BLR_A)

tidy(BLR_A, exponentiate = TRUE, conf.level = 0.95)

plot(BLR_A, which = 4, id.n = 3)
plot(abs(residuals(BLR_A)))

pred_BLR_A <- round(predict(BLR_A, test_BLR_A, type="response"))
pred_BLR_A

confmat_BLR_A <- confusionMatrix(test_BLR_A$A_CAT_BIN_F, pred_BLR_A)
confmat_BLR_A

acc_BLR_A <- (confmat_BLR_A[1,1]+confmat_BLR_A[2,2])/23
acc_BLR_A

sensitivity(test_BLR_A$A_CAT_BIN_F, pred_BLR_A)
specificity(test_BLR_A$A_CAT_BIN_F, pred_BLR_A)

roc_score_A <- roc(test_BLR_A$A_CAT_BIN_F, pred_BLR_A)
plot(roc_score_A, main="ROC Curve - Logistic Regression for Attitude", col = "Blue")
roc_score_A
```

```{r Confirmatory Factor Analysis_Initial}

KPA_CFA <- read_xlsx("KPA-CFA.xlsx")
glimpse(KPA_CFA)

var(KPA_CFA)

CFA_Initial <- 'KNLDG = ~ DT + SX + AG + ED + OC + HI + RG + ER + VP + CC + COV + SRD + DRA + EWA + DSD + FFD + SCS + SN + LF + TS + PCR + ALC + TMP + BCG + WH
PRAC = ~ DT + SX + AG + ED + OC + HI + RG + WM + KM + HM + OM + HS + CP + PT + REM + IY + PD + PF + SP + MCM
ATT = ~ DT + SX + AG + ED + OC + HI + RG + ACP + AWH + AWM + QIN + FG + VC + CN'

fit_CFA_Initial <- cfa(CFA_Initial, data = KPA_CFA, mimic = c("MPlus"), std.lv = TRUE, ordered = TRUE)

summary(fit_CFA_Initial, fit.measures = TRUE, standardized = TRUE)

```

```{r Confirmatory Factor Analysis_Tune_1}
CFA_Tune_1 <- 'KNLDG = ~ DT + ED + OC + ER + VP + CC + DRA + DSD + SCS + LF + TS + PCR + ALC + BCG + WH
PRAC = ~ AG + OC + WM + KM + HM + OM + HS + PT + REM + IY + PD + PF + SP + MCM
ATT = ~ AG + OC + ACP + AWH + AWM + QIN + FG + VC + CN'

fit_CFA_Tune_1 <- cfa(CFA_Tune_1, data = KPA_CFA, mimic = c("MPlus"), std.lv = TRUE, ordered = FALSE)

summary(fit_CFA_Tune_1, fit.measures = TRUE, standardized = TRUE)
```

```{r Confirmatory Factor Analysis_Tune_2}
CFA_Tune_2 <- 'KNLDG = ~ DT + ED + ER + VP + CC + DRA + DSD + SCS + LF + TS + PCR + BCG + WH
PRAC = ~ OC + WM + HM + OM + HS + PT + IY + PD + PF + SP + MCM
ATT = ~ AG + OC + ACP + AWH + AWM + QIN + FG + VC + CN'

fit_CFA_Tune_2 <- cfa(CFA_Tune_2, data = KPA_CFA, mimic = c("MPlus"), std.lv = TRUE, ordered = TRUE)

summary(fit_CFA_Tune_2, fit.measures = TRUE, standardized = TRUE)
```

```{r Confirmatory Factor Analysis_Final}
CFA_Final <- 'KNLDG = ~ DT + ED + ER + VP + CC + DRA + DSD + SCS + LF + TS + PCR + BCG + WH
PRAC = ~ WM + HM + OM + HS + PT + IY + PD + PF + SP + MCM
ATT = ~ AG + ACP + AWH + AWM + QIN + FG + VC + CN'

fit_CFA_Final <- cfa(CFA_Final, data = KPA_CFA, mimic = c("MPlus"), std.lv = TRUE, ordered = FALSE)

residuals(fit_CFA_Initial)$cov
fitMeasures(fit_CFA_Final, c("chisq.scaled", "df", "pvalue.scaled", "rmsea", "CFI", "TLI"))
summary(fit_CFA_Final, fit.measures = TRUE, standardized = TRUE)

lavaanPlot(model = fit_CFA_Final, covs =TRUE, coefs = TRUE, stand = TRUE, sig = 0.05, stars = "covs")
semPlot::semPaths(fit_CFA_Final, whatLabels = "est", residuals = TRUE, intercepts = TRUE)
```

```{r Random Forest Modelling}

KPA_RF <- read_xlsx("KPA-RF.xlsx")
KPA_RF$K_CAT_BIN_F <- factor(KPA_RF$K_CAT_BIN)
KPA_RF$P_CAT_BIN_F <- factor(KPA_RF$K_CAT_BIN)
KPA_RF$A_CAT_BIN_F <- factor(KPA_RF$K_CAT_BIN)
glimpse(KPA_RF)

#Knowledge

set.seed(123)
Split_RF_K <- createDataPartition(y = KPA_RF$K_CAT_BIN_F, p = 0.7, list = FALSE)
train_RF_K <- KPA_RF[Split_RF_K,]
test_RF_K <- KPA_RF[-Split_RF_K,]

x <- train_RF_K[1:7]

mtry_K <- tuneRF(x = x,y = train_RF$K_CAT_BIN_F, stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE)
best.mtry_K <- mtry_K[mtry_K[, 2] == min(mtry_K[, 2]), 1]
print(mtry_K)
print(best.mtry_K)

for (a_K in 1:500) {
  RFM_K <- randomForest(K_CAT_BIN_F ~ DT + SX + AG + ED + OC + HI + RG, data = train_RF_K, mtry_K = best.mtry_K, ntree = a_K)
  ERR_RFM_K <- RFM_K$err.rate[,1]
}

OOB_RF_K <-as.data.frame(ERR_RFM_K)
min(OOB_RF_K$ERR_RFM_K)
min <- which(OOB_RF_K$ERR_RFM_K == min(OOB_RF_K$ERR_RFM_K), arr.ind = TRUE)
min_K <- min[1]
RFM_K <- randomForest(K_CAT_BIN_F ~ DT + SX + AG + ED + OC + HI + RG, data = train_RF_K, mtry_K = best.mtry_K, ntree = min_K)
RFM_K

importance(RFM_K)
varImpPlot(RFM_K)

for (b_K in 1:min_K) {
  reprtree::plot.getTree(RFM_K, k = b_K)
}
pred_test_RF_K <- predict(RFM_K, newdata = test_RF_K)
caret::confusionMatrix(test_RF_K$K_CAT_BIN_F, pred_test_RF_K)

#Practice

set.seed(222)
Split_RF_P <- createDataPartition(y = KPA_RF$P_CAT_BIN_F, p = 0.7, list = FALSE)
train_RF_P <- KPA_RF[Split_RF_P,]
test_RF_P <- KPA_RF[-Split_RF_P,]

x <- train_RF_P[1:7]

mtry_P <- tuneRF(x = x,y = train_RF$P_CAT_BIN_F, stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE)
best.mtry_P <- mtry_P[mtry_P[, 2] == min(mtry_P[, 2]), 1]
print(mtry_P)
print(best.mtry_P)

for (a_P in 1:500) {
  RFM_P <- randomForest(P_CAT_BIN_F ~ DT + SX + AG + ED + OC + HI + RG, data = train_RF_P, mtry_P = best.mtry_P, ntree = a_P)
  ERR_RFM_P <- RFM_P$err.rate[,1]
}

OOB_RF_P <-as.data.frame(ERR_RFM_P)
min(OOB_RF_P$ERR_RFM_P)
min <- which(OOB_RF_P$ERR_RFM_P == min(OOB_RF_P$ERR_RFM_P), arr.ind = TRUE)
min_P <- min[1]
RFM_P <- randomForest(P_CAT_BIN_F ~ DT + SX + AG + ED + OC + HI + RG, data = train_RF_P, mtry_P = best.mtry_P, ntree = min_P)
RFM_P

importance(RFM_P)
varImpPlot(RFM_P)

for (b_P in 1:min_P) {
  reprtree::plot.getTree(RFM_P, k = b_P)
}
pred_test_RF_P <- predict(RFM_P, newdata = test_RF_P)
caret::confusionMatrix(test_RF_P$P_CAT_BIN_F, pred_test_RF_P)

#Attitude

set.seed(752)
Split_RF_A <- createDataPartition(y = KPA_RF$A_CAT_BIN_F, p = 0.7, list = FALSE)
train_RF_A <- KPA_RF[Split_RF_A,]
test_RF_A <- KPA_RF[-Split_RF_A,]

x <- train_RF_A[1:7]

mtry_A <- tuneRF(x = x,y = train_RF$A_CAT_BIN_F, stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE)
best.mtry_A <- mtry_A[mtry_A[, 2] == min(mtry_A[, 2]), 1]
print(mtry_A)
print(best.mtry_A)

for (a_A in 1:500) {
  RFM_A <- randomForest(A_CAT_BIN_F ~ DT + SX + AG + ED + OC + HI + RG, data = train_RF_A, mtry_A = best.mtry_A, ntree = a_A)
  ERR_RFM_A <- RFM_A$err.rate[,1]
}

OOB_RF_A <-as.data.frame(ERR_RFM_A)
min(OOB_RF_A$ERR_RFM_A)
min <- which(OOB_RF_A$ERR_RFM_A == min(OOB_RF_A$ERR_RFM_A), arr.ind = TRUE)
min_A <- min[1]
RFM_A <- randomForest(A_CAT_BIN_F ~ DT + SX + AG + ED + OC + HI + RG, data = train_RF_A, mtry_A = best.mtry_A, ntree = min_A)
RFM_A

importance(RFM_A)
varImpPlot(RFM_A)

for (b_A in 1:min_A) {
  reprtree::plot.getTree(RFM_A, k = b_A)
}
pred_test_RF_A <- predict(RFM_A, newdata = test_RF_A)
caret::confusionMatrix(test_RF_A$A_CAT_BIN_F, pred_test_RF_A)
```


