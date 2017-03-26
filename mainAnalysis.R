# Name               : mainAnalysis.R
# Type              : Program
# Object             : Construct a work environment.
#                     Cast programs for data manipulation.
#                     Destruct work environment.
# Input             : csv file 
# Output            : datasets and figures
# Author            : A. Soudant
# R version         : 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Creation date : March 2017
# Modified date : March 2017


#################################################################
#
# Script principal pour l'execution des fonctions utiles à l'analyse
# des données pour le projet Salaires
#
#################################################################


#################### Description des données ####################

# Les données sont sous forme d'un fichier csv (data_salaire.csv)

# index : id (range 1-76 <int>)

# variables discriminantes: variables pouvant être la cause d'un traitement inégal des employés en termes de niveau de salaire

# sexe (Logit 0-1 <int>): variable de sexe de l'employé prenant la valeur 0 pour un homme et la valeur 1 pour une femme
# mino (Logit 0-1 <int>): variable de minorité ethnique prenant la valeur 1 si appartenance à une minorité et 0 sinon

# variables de classement: variables permettant de classer les employés en fonction de leur parcours professionnel et ainsi
# éliminer les biais dues à des différences justifiables de niveau de salaire 

# nais (année <int>): année de naissance (contient des valeurs non attribuées enregistrée en valeur 9999)
# educ (nb année <int>): nombre d'années d'études avant embauche
# anc (nb mois <int>): nombre de mois d'ancienneté dans l'entreprise
# csp (logit 1-2-3 <int>): catégorie socio-professionelle  (1 : employé de bureau, 2 : agent de sécurité, 3 : cadre)
# exp (nb mois <int>): nombre de mois d'expérience avant l'embauche dans l'entreprise

# variables cibles: variables quantitatives représentant le niveau de salaire et donc pouvant mettre en évidence un traitement
# inégal des salariés selon leur sexe ou leur appartenance à une minorité ethnique

# salemb (quantité euros <int>): salaire à l'embauche
# salac (quantité euros <int>): Salaire actuel de l'employé

#################### importation des librairies ####################

source('functions.R')


#################### Preparation des données ####################

# stockage des données dans R sous l'objet data
data <- read.table("data_salaire.csv",sep=";",na.strings=" ",header=T)

# affiche les n premieres lignes du dataset
head(data,n=10L)

# affiche un resume des données importées
str(data)

# affiche quelques stats sur les données
summary(data)

# permet d'avoir les variables sans appeler data$
attach(data)

# création des variables qualitatives

sexe_qual <- factor(sexe, levels = c(0,1), labels=c("homme","femme"))
mino_qual <- factor(mino, levels = c(0,1), labels=c("non","oui"))
csp_qual <- factor(csp, levels = c(1,2,3), labels=c("employe","securite","cadre"))


# Transformation des variables de classement en variables catégorielles

educ_qual = f_tr_classement(educ)
anc_qual = f_tr_classement(anc)
exp_qual = f_tr_classement(exp)

# Creation du dataset d'analyse

data_analysis <- data.frame(id,sexe_qual,nais,educ_qual,csp_qual,salemb,salac,anc_qual,exp_qual,mino_qual)

str(data_analysis)

summary(data_analysis)


#################### nettoyage des données ####################

# identifie les employés sans date de naissance
bool_NA = data_analysis['nais']==9999

# creation du dataset des employés sans date de naissance
data_no_nais = matrix(NA,length(which(bool_NA)),length(data_analysis))

k = 1 
for (i in which(bool_NA)){
	for (j in 1:length(data_analysis)){ 
		data_no_nais[k, j] = data_analysis[i,j]
	}
	k = k + 1
}


# on peut attribuer une valeur médiane aux personnes n'ayant pas de date de naissance en fonction de leur sexe et des variables de classement
# l'education et le status socio-professionnel sont utilisés pour filtrer les employés et obtenir une mediane la plus représentatative
# possible, utiliser l'ancienneté et l'expérience devient trop restrictif pour calculer une mediane.


# itération sur les valeurs manquantes
for (i in 1:length(data_no_nais[,1])){ 

# conditions à retrouver dans data_analysis pour trier les employés
	if (data_no_nais[i,2] == 1){ sexe_select = 'homme'}else{sexe_select = 'femme'}
	if (data_no_nais[i,4] == 1){ educ_select = 'faible'}else{if((data_no_nais[i,4] == 2)){educ_select = 'moyen'}else{educ_select = 'fort'}}
	if (data_no_nais[i,5] == 1){ csp_select = 'employe'}else{if((data_no_nais[i,5] == 2)){csp_select = 'securite'}else{csp_select = 'cadre'}}

	nais_fill = data_analysis['nais'][data_analysis['sexe_qual'] == sexe_select
			& data_analysis['educ_qual'] == educ_select
			& data_analysis['csp_qual'] == csp_select]

	nais_median = median(nais_fill[nais_fill != 9999])

	data_no_nais[i,3] = as.integer(nais_median)

# Dans le cas d'une valeur 'NA' voulant dire que l'employé est dans une combinaison unique en termes de sexe, educ et csp,
# on lui affectera une date de naissance estimée par la median sur le sexe uniquement.

	if (is.na(data_no_nais[i,3])){data_no_nais[i,3] = median(data_analysis['nais'][data_analysis['sexe_qual'] == sexe_select]) }
}

# injection des valeurs estimées dans data_analysis

k = 1
for (i in data_no_nais[,1]){
	data_analysis[i,'nais'] = data_no_nais[k,3]
	k = k + 1
}

# creation d'une nouvelle variable age deduite de l'année de naissance

data_analysis["age"] = 2017 - data_analysis['nais']

#################### analyse des données ####################

# variables d'intérets: salemb et salac (quantitatives)

#__________________ étude du salaire par sexe (qualitative)

#_________ Salaire à l'embauche

f_egalite_salaire(data_analysis,0)

#_________ Salaire actuel

f_egalite_salaire(data_analysis,1)

#__________________ étude du salaire par sexe selon les variables de classement

#_________ csv

data.employe = data_analysis[data_analysis['csp_qual'] == 'employe',]
data.securite = data_analysis[data_analysis['csp_qual'] == 'securite',]
data.cadre = data_analysis[data_analysis['csp_qual'] == 'cadre',]

f_egalite_salaire(data.employe,0)
#f_egalite_salaire(data.securite,0)
# pas suffisamment de femmes dans la securite pour les tests
f_egalite_salaire(data.cadre,0)

#_________ educ

data.educ.faible = data_analysis[data_analysis['educ_qual'] == 'faible',]
data.educ.moyen = data_analysis[data_analysis['educ_qual'] == 'moyen',]
data.educ.fort = data_analysis[data_analysis['educ_qual'] == 'fort',]

f_egalite_salaire(data.educ.faible,0)
f_egalite_salaire(data.educ.moyen,0)
f_egalite_salaire(data.educ.fort,0)

#_________ anc

data.anc.faible = data_analysis[data_analysis['anc_qual'] == 'faible',]
data.anc.moyen = data_analysis[data_analysis['anc_qual'] == 'moyen',]
data.anc.fort = data_analysis[data_analysis['anc_qual'] == 'fort',]

f_egalite_salaire(data.anc.faible,0)
f_egalite_salaire(data.anc.moyen,0)
f_egalite_salaire(data.anc.fort,0)


#_________ exp

data.exp.faible = data_analysis[data_analysis['exp_qual'] == 'faible',]
data.exp.moyen = data_analysis[data_analysis['exp_qual'] == 'moyen',]
data.exp.fort = data_analysis[data_analysis['exp_qual'] == 'fort',]

f_egalite_salaire(data.exp.faible,0)
f_egalite_salaire(data.exp.moyen,0)
f_egalite_salaire(data.exp.fort,0)


#_________ age

# test entre deux quantités
mod_q1 <-  lm(data_analysis$salemb ~ data_analysis$age)
summary(mod_q1)

age2 <- data_analysis$age^2

mod_q2 <-  lm(data_analysis$salemb ~ data_analysis$age + age2)
summary(mod_q2)

#__________________ Modélisation

#_________ qda

libray(MASS)

modQuad <- qda(data_analysis$sexe_qual ~ data_analysis$salemb)

predictions <- predict(modQuad,data_analysis)$class

table(predictions,data_analysis$sexe_qual)

score <- 1 - sum(predictions != data_analysis$sexe_qual) / nrow(data_analysis)
score

#_________ svm

library(e1071)

model <- svm(data_analysis$sexe_qual ~ data_analysis$salemb , data_analysis)
 
predictedY <- predict(model, data_analysis)

table(predictedY,data_analysis$sexe_qual)

score <- 1 - sum(predictedY != data_analysis$sexe_qual) / nrow(data_analysis)
score

#_________ knn

train <- data[sample(1:nrow(data_analysis), 50,replace=FALSE),]
test <- data[sample(1:nrow(data_analysis), 20,replace=FALSE),]

train <- train[,c(csp_qual,salemb, age,educ_qual,anc_qual,exp_qual)]

library(kknn)
k <- kknn(data_analysis$sexe_qual~data_analysis$salemb + data_analysis$csp_qual + data_analysis$age 
	+ data_analysis$educ_qual + data_analysis$anc_qual + data_analysis$exp_qual ,train,test)
summary(k)

score <- 1 - sum(k$fit != data_analysis$sexe_qual) / nrow(data)
score

#_________ rpart

library(rpart)
rpartichio <- rpart(data_analysis$sexe_qual ~ .,data_analysis)
rpartichio

#_________ random forest

library(randomForest)
RFdeschio <- randomForest(data_analysis$sexe_qual ~ .,data_analysis)
RFdeschio

score <- 1 - sum(RFdeschio$predicted != data_analysis$sexe_qual) / nrow(data_analysis)
score






 
 



















