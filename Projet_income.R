###               Importation de la BD 

df = as.data.frame(read.table(file = "adult.data", header = FALSE, sep = ","))
is.data.frame(df)
print(df)
colnames(df)<- c("age","work_class","fnlwgt","education","education_num","marital_statut","occupation","relationship","race","sex","capital_gain","capital_loss","hours_per_week","native_country","income")
df[df == " ?"] <- NA
summary(df)
nrow(df)
ncol(df)

###         Transformation des variables

#creation de la variable income_r
df$income_r<-ifelse(df$income == " >50K",1,0)
table(df$income_r) #verification

#creation variable marital_statut_r
df$marital_statut_r[df$marital_statut==" Married-AF-spouse"]<- "Married"
df$marital_statut_r[df$marital_statut==" Married-civ-spouse"]<- "Married"
df$marital_statut_r[df$marital_statut==" Married-spouse-absent"]<- "Married"
df$marital_statut_r[df$marital_statut==" Divorced"]<- "Divorced"
df$marital_statut_r[df$marital_statut==" Never-married"]<- "Never-married"
df$marital_statut_r[df$marital_statut==" Widowed"]<- "Widowed"
df$marital_statut_r[df$marital_statut==" Separated"]<- "Separated"
table(df$marital_statut_r) #verification

#creation de la variable hours_per_week_r
df$hours_per_week_r<- ifelse(df$hours_per_week<=40,'NormalWorkLoad','HugeWorkLoad')
table(df$hours_per_week_r) #verification

###             Analyse descriptive

install.packages("sqldf")
library(sqldf)
library(ggplot2)
library(dbplyr)

# Income and Work_class
dat1<- data.frame(table(df$income_r,df$work_class))
dat1
names(dat1) <- c("Income","Work_class","Count")
ggplot(dat1, aes(x=Work_class, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different classes')
#Bref Conclusion: Le secteur prive, qui emploie le plus de personnes, compte le plus grand nombre de personnes qui gagnent plus de 50 000 par an. Cependant, en termes de proportion, les independants sont les gagnants.

# Income and education
dat2<- data.frame(table(df$income_r,df$education))
names(dat2) <- c("Income","Education","Count")
ggplot(dat2, aes(x=Education, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different education level')
#Bref Conclusion: Un niveau d'education plus eleve peut se traduire par une plus grande possibilite d'emploi bien remunere.

#Income and marital statut
dat3<- data.frame(table(df$income_r,df$marital_statut_r))
names(dat3) <- c("Income","Marital_statut","Count")
ggplot(dat3, aes(x=Marital_statut, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different marital statut')
#Bref Conclusion: Une bonne relation conjugale semble etre une cle pour gagner plus d'argent.

#Income and race
dat4<- data.frame(table(df$income_r,df$race))
names(dat4) <- c("Income","Race","Count")
ggplot(dat4, aes(x=Race, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different races')
#Bref Conclusion: D'apres nos donnees les blancs sont avantages en terme de revenu.

#Income and sex
dat5<- data.frame(table(df$income_r,df$sex))
names(dat5) <- c("Income","Sex","Count")
ggplot(dat5, aes(x=Sex, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid within different sexes')
#Bref conclusion: Les hommes ont plus de chance de percevoir un haut revenu.

#Income and work load
dat6<- data.frame(table(df$income_r,df$hours_per_week_r))
names(dat6) <- c("Income","Hours","Count")
ggplot(dat6, aes(x=Hours, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid with different Work Load')
#Bref conclusion: Les personnes qui ont une charge de travail eleve ont plus de chance d'avoir un revenu superieur a 50k.

#income and Native country
dat7<- data.frame(table(df$income_r,df$native_country))
names(dat7) <- c("Income","Native","Count")
ggplot(dat7, aes(x=Native, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid with different Native Country')
(21999 + 7171) / sum(dat7$Count) #91% sont des americains

#Income and Occupation
dat8<- data.frame(table(df$income_r,df$occupation))
names(dat8) <- c("Income","occupation","Count")
ggplot(dat8, aes(x=occupation, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid with different occupation')

#Income and relationship
dat9<- data.frame(table(df$income_r,df$relationship))
names(dat9) <- c("Income","relationship","Count")
ggplot(dat9, aes(x=relationship, y=Count, fill=Income)) + geom_bar(stat="identity", position = 'stack') + 
  theme_bw()+scale_fill_manual(values = c('green','red'))+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Proportions of above-paid with different relationship')


###           Analyse de correlation

## Variable d'interet et variables categorielles

# Income and Work_class
dat1
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ work_class + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "Work class",
           main = "Income by work class")
# Test de Khi-2
chi2IncWrk <- chisq.test(x = df$work_class,
                         y = df$income_r)

chi2IncWrk

##Income and education
dat2
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ education + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "education",
           main = "Income by education")
# Test de Khi-2
chi2Inceduc <- chisq.test(x = df$education,
                          y = df$income_r)
chi2Inceduc

## Income and marital status
dat3
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ marital_statut_r + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "Marital status",
           main = "Income by marital status")
# Test de Khi-2
chi2Incmar <- chisq.test(x = df$marital_statut_r,
                         y = df$income_r)
chi2Incmar

## Income and race
dat4
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ race + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "race",
           main = "Income by race")
# Test de Khi-2
chi2Incrac <- chisq.test(x = df$race,
                         y = df$income_r)
chi2Incrac

## Income and Sex
dat5
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ sex + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "Sex",
           main = "Income by Sex")
# Test de Khi-2
chi2Incsex <- chisq.test(x = df$sex,
                         y = df$income_r)
chi2Incsex

## Income and hours_per_week
dat6
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ hours_per_week_r + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "Workload",
           main = "Income by workload")
# Test de Khi-2

chi2Incwrkld <- chisq.test(x = df$hours_per_week_r,
                           y = df$income_r)
chi2Incwrkld

## Income and native_country
dat7
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ native_country + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "Native_country",
           main = "Income by Native country")
#Test khi-2 income and native country
chi2Incnatc <- chisq.test(x = df$native_country,
                          y = df$income_r)
chi2Incnatc

## Income and occupation
dat8
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ df$occupation + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "occupation",
           main = "Income by occupation")
chi2IncOC<- chisq.test(x = df$occupation,
                       y = df$income_r)
chi2IncOC

## Income and relationship
dat9
# Representation graphique du tableau de contingence
mosaicplot(formula = ~ df$relationship + income_r,
           col = c("grey25", "grey50"),
           data = df,
           ylab = "Income",
           xlab = "relationship",
           main = "Income by relationship")
chi2IncREL<- chisq.test(x = df$relationship,
                        y = df$income_r)
chi2IncREL

## Resume tableau test khi2
rbind1 = rbind(chi2Incwrkld$statistic, chi2Incrac$statistic, chi2Incmar$statistic, chi2Inceduc$statistic, chi2IncWrk$statistic, chi2Incsex$statistic, chi2Incnatc$statistic, chi2IncOC$statistic, chi2IncREL$statistic)
rbind2 = rbind(chi2Incwrkld$parameter, chi2Incrac$parameter, chi2Incmar$parameter, chi2Inceduc$parameter, chi2IncWrk$parameter, chi2Incsex$parameter, chi2Incnatc$parameter, chi2IncOC$parameter, chi2IncREL$parameter)
rbind3 = rbind(chi2Incwrkld$p.value, chi2Incrac$p.value, chi2Incmar$p.value, chi2Inceduc$p.value, chi2IncWrk$p.value, chi2Incsex$p.value, chi2Incnatc$p.value, chi2IncOC$p.value, chi2IncREL$p.value)
tabkhi2 = cbind(rbind1, rbind2, rbind3)
colnames(tabkhi2) <- c("X-squared", "df", "P-value")
rownames(tabkhi2) <- c("Income/WorkLoad", "Income/race", "Income/marital", "Income/education", "Income/Workclass", "Income/sex", "Income/NativeCountry", "Income/occupation", "Income/relationship")
tabkhi2

## Variable d'interet et variables quantitatives

## Income and age
# Boite a moustache
boxplot(df$age ~ df$income_r,
        col = "grey",
        names = c("0", "1"),
        ylab = "Age",
        xlab = "Revenu")
# Analyse de la variance a un facteur (ANOVA)
# Permet de modaliser la relation entre le revenu et l'age
anovaAgeRev <- aov(age ~ income_r, data = df)
summary(anovaAgeRev)

## Income and capital_gain
# Boite a moustache
boxplot(df$capital_gain ~ df$income_r,
        col = "grey",
        names = c("0", "1"),
        ylab = "Capital_gain",
        xlab = "Revenu")
# Analyse de la variance a un facteur (ANOVA)
anovaCapgRev <- aov(capital_gain ~ income_r, data = df)
summary(anovaCapgRev)

## Income and capital_loss
# Boite a moustache
boxplot(df$capital_loss ~ df$income_r,
        col = "grey",
        names = c("0", "1"),
        ylab = "Capital_loss",
        xlab = "Revenu")
# Analyse de la variance a un facteur (ANOVA)
anovaCaplRev <- aov(capital_loss ~ income_r, data = df)
summary(anovaCaplRev)


###                 Modele Logit

## Regression 
modele <- glm(income_r ~ age + education + work_class + hours_per_week_r+ marital_statut_r+ sex + race, data = df, family = binomial(logit))
modele
summary(modele)
library(broom)
library(GGally)
library(broom.helpers)

## Odds Ratio
coeff<-tidy(modele, conf.int = TRUE, exponentiate = TRUE)
ggcoef_model(modele, exponentiate = TRUE)

## prediction profil avec la plus haute proba de percevoir un haut revenu de notre BD 
library(tidyverse)
probabilities <- modele %>% predict(df, type = "response")
probabilities
summary(probabilities)
proba <- data.frame(y= 1:32561, z=probabilities)
install.packages("data.table") 
library(data.table)
setDT(x)[ , .SD[which.max(z)]]
#individu avec la plus haute proba de percevoir un haut revenu correspond a la 8807 ligne de notre BD


###             Loglikehood ratio test

#Modele reduit
modele_r <- glm(income_r ~ work_class + education + marital_statut_r + sex, data = df, family = binomial(logit))
modele_r
#Test
install.packages("lmtest")
library(lmtest)
lrtest(modele, modele_r)

#Matrice de confusion
income.pred <- predict(modele, type = "response", newdata = df)
head(income.pred)
tab<- table(income.pred > 0.5, df$income_r)
tab
#Nous avons donc 5502 (3699+1803) predictions incorrectes sur un total de 30725, soit un taux de mauvais classement de 17,9%.
install.packages("writexl")
library(writexl)
write_xlsx(coeff, path = "coeff.xlsx")
