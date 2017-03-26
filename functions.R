# Name               : functions.R
# Type              : Program
# Object             : library of functions to use in mainAnalysis.R
# Input             : None 
# Output            : None
# Author            : A. Soudant
# R version         : 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Creation date : March 2017
# Modified date : March 2017


#################################################################
#
# Librairie de fonctions qui seront appelées dans l'analyse principale
#
#################################################################


# Transformation des variables de classement en variables catégorielles
# contenant 3 classes attribuées selon les valeurs inter-quartiles
# input : variable à transformer <int> ou <float>
# output : variable catégorielle <int> 

f_tr_classement <- function(var)
{
cat("Transformation des variables de classement...", "\n")

summary_var = summary(var)

# creation des 3 classes de 8 ans à 12 ans, de 12 ans à 15 ans et de 15 ans à 21 ans d'études
var_qual = var
var_qual[var_qual < summary_var[2]] = 1
var_qual[var_qual >= summary_var[2] & var_qual <= summary_var[5]] = 2
var_qual[var_qual > summary_var[5]] = 3

var_final <- factor(var_qual, levels = c(1,2,3), labels=c("faible","moyen","fort"))

return(var_final)
}


# analyse de l'égalité des salaires entre hommes et femmes


f_egalite_salaire <- function(dataset,bool_salaire)
{

f_sexe = data_analysis$sexe_qual
sexe = "sexe_qual"
if (bool_salaire == 0){
	f_salaire = data_analysis$salemb
	salaire = "salemb"
			}else{f_salaire = data_analysis$salac
				salaire= "salac"}

# moyenne du salaire par sexe
print('moyenne du salaire par sexe')
mean_sexe = tapply(f_salaire, f_sexe, mean, na.rm=TRUE)
print(mean_sexe)
if((mean_sexe[2] - mean_sexe[1]) < 0){
print('on observe que le salaire des hommes est en moyenne plus important que celui des femmes')
}else{print('on observe que le salaire des femmes est en moyenne plus important que celui des hommes')}


# test d'égalité des variances
var_sexe = var.test(f_salaire ~ f_sexe)
print(var_sexe)
if(var_sexe[3] < 0.05){
print("p value inférieure a 0.05 => on rejete l'égalité des variances")
}else{print("p value supérieure a 0.05 => on ne rejete pas l'égalité des variances")}

select.sexe.homme <- dataset[ , sexe] == "homme"
select.sexe.femme <- dataset[ , sexe] == "femme"

# test de normalité
shap.homme = shapiro.test(data[select.sexe.homme,salaire])
shap.femme = shapiro.test(data[select.sexe.femme,salaire])
print(shap.homme)
print(shap.femme)

if(shap.homme$p.value < 0.05 || shap.femme$p.value < 0.05){
print("p value inferieure a 0.05 => non normalité des données")
bool_norm = FALSE
}else{print("p value supérieure a 0.05 => normalité des données")
bool_norm=TRUE}
# p value tres petite => non normalité des données

# test d'égalité des moyennes
 
if(bool_norm == TRUE){
student = t.test(f_salaire ~ f_sexe) 
print(student)
if(student$p.value < 0.05 ){
print("p value inférieure a 0.05 => le salaire diffère selon le sexe")
}else{print("p value supérieure a 0.05 => le salaire est le meme selon le sexe")}
}else{
kruskal = kruskal.test(f_salaire ~ f_sexe) 
print(kruskal)
if(kruskal$p.value < 0.05 ){
print("p value inférieure a 0.05 => le salaire diffère selon le sexe")
}else{print("p value supérieure a 0.05 => le salaire est le meme selon le sexe")}
}
}



