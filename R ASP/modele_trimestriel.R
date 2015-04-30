# Modele trimestriel et projection sur le simpplexe
# Deux versions : multiplicatif et log lineaire

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)
library(MSBVAR)

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.r")
source("simplex.r")

###########################################################################################################
# Variables utilisées dans les modèles

#IN et IO1 sont définis dans désaisonnalisation
N = matrix(1,3,1)
INreg_trim = create_mat_trimestre(INreg_cjo)%*%N

###########################################################################################################
# Troncature de la période 2008-2009

# la période décembre 2008 - decembre 2010 représente les dates 169 à 193
IO1reg_cjo_2 = c(IO1reg_cjo[7:147],IO1reg_cjo[181:219]) 
INreg_trim_2 = c(INreg_trim[8:168],INreg_trim[202:230]) 
print(IO1reg_cjo)

###########################################################################################################
# modèle trimestriel
res20=lm ( IO1reg_cjo_2[1:209]~INreg_trim_2[1:209]+INreg_trim_2[4:212]+INreg_trim_2[7:215]+INreg_trim_2[10:218]-1)
summary(res20)  
plot(IO1reg_cjo_2)
par(new=T)
plot(fitted(res20))

# vérification de la non autocorrélation des résidus

# interprétation des résultats


###########################################################################################################
# projection du modèle sur le volume engendré par le simplexe

P=c(coef(res20)[1],coef(res20)[2],coef(res20)[3],coef(res20)[4])
print(P)
P_simplex=proj(P)
print(coef(res20)[1])
# P appartient au simplexe




# MODELE avec IO14 ! annuel




