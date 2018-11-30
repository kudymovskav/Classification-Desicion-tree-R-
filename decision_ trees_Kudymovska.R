risk <- read.csv("Projet_Data.csv", header = TRUE, sep = ",", dec = ".")
View(risk)


install.packages("ggplot2")
library(ggplot2)

attach(risk) #pour referencer les attributs par leur nom seul
qplot(LOANS, data=risk, color=RISK, binwidth = 1)
table(LOANS, RISK)

qplot(INCOME, data=risk, color=RISK, binwidth = 10000)
qplot(MORTGAGE, data=risk, color=RISK)

qplot(AGE, data=risk, color=RISK, binwidth = 10)
qplot(GENDER, data=risk, color=RISK)
qplot(MARITAL, data=risk, color=RISK)
qplot(NUMKIDS, data=risk, color=RISK, binwidth = 1)
qplot(NUMCARDS, data=risk, color=RISK, binwidth = 1)
qplot(HOWPAID, data=risk, color=RISK)
qplot(STORECAR, data=risk, color=RISK, binwidth = 1)


boxplot(INCOME~RISK, data=risk,col=c("red","blue", "green"),main="Revenus selon Risque", xlab="Risque", ylab="Revenus")
tapply(INCOME, RISK, summary)

boxplot(AGE~RISK, data=risk,col=c("red","blue", "green"),main="Age selon Risque", xlab="Risque", ylab="Age")

qplot(INCOME, AGE, data=risk, color=RISK) + geom_jitter()
qplot(INCOME, NUMKIDS, data=risk, color=RISK) + geom_jitter()
qplot(INCOME, MORTGAGE, data=risk, color=RISK) + geom_jitter()
qplot(INCOME, LOANS, data=risk, color=RISK) + geom_jitter()

risk_EA <- risk[1:2745,]
risk_ET <- risk[2746:4117,]

#Suppression de la variable ID
risk_EA <- subset(risk_EA, select = -ID) 
risk_ET <- subset(risk_ET, select = -ID) 

View(risk_EA)
View(risk_ET)
summary(risk_EA)
summary(risk_ET)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


#Classifieur 1
tree1 <- rpart(RISK~., risk_EA)
tree1

prp(tree1, type=4, extra=8, box.col=c("tomato", "skyblue", "green")[tree.rp1$frame$yval])
prp(tree1, type=4, extra=1, box.col=c("tomato", "skyblue", "green")[tree.rp1$frame$yval])

#Application de l'arbre de decision 1 a l'ensemble de test
pred.tree1 <- predict(tree1, risk_ET, type="class")

table(pred.tree1)
table(pred.tree1, risk_ET$RISK)


install.packages("tree")
library(tree)
install.packages("party")
library(party)

#Classifieur 2
tree2 <- ctree(RISK~., risk_EA)
print(tree2)
plot(tree2, type="simple")

#Application de l'arbre de decision 2 a l'ensemble de test
pred.tree2 <- predict(tree2, risk_ET)

table(pred.tree2)
table(risk_ET$RISK, pred.tree2)

#Classifieur 3
tree3 <- tree(RISK~., data=risk_EA)
print(tree3)
plot(tree3)
text(tree3, pretty=0)

#Application de l'arbre de decision 3 a l'ensemble de test
pred.tree3 <- predict(tree3, risk_ET, type="class")

table(pred.tree3)
table(risk_ET$RISK, pred.tree3)

#Application du classifieur 2
# Preparation des donnees a predire
risk_new <- read.csv("Projet_Data_New.csv", sep=",", dec=".", header=T)
View(risk_new)
detach()
attach(risk_new)

class.tree2 <- predict(tree2, risk_new, type="response")
prob.tree2 <- predict(tree2, risk_new, type = "prob")

# Transformation de la liste de probabilites en vecteur
prob.tree2 <- c(do.call("rbind",prob.tree2))

resultat <- data.frame(ID, class.tree2, prob.tree2[1:20],prob.tree2[21:40], prob.tree2[41:60])

# Renommage des colonnes
names(resultat)[2] <- "Prediction"
names(resultat)[3] <- "Bad Loss"
names(resultat)[4] <- "Bad Profit"
names(resultat)[5] <- "Good Risk"
View(resultat)

# Enregistrement du fichier de resultats
write.csv(resultat, file='L3_MASS_Kudymovska.csv', row.names = F)

# Examen des probabilites des predictions par classe
summary(subset(resultat, resultat$Prediction=="bad loss")$"Bad Loss")
summary(subset(resultat, resultat$Prediction=="bad profit")$"Bad Profit")
summary(subset(resultat, resultat$Prediction=="good risk")$"Good Risk")



