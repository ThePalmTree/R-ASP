# Modele trimestriel et projection sur le simpplexe
# Deux versions : multiplicatif et log lineaire

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)
library(MSBVAR)

#Importation des fichiers R n�c�ssaires
source("desaisonnalisation.r")
source("basic_functions.r")
source("simplex.r")

###########################################################################################################
# Variables utilis�es dans les mod�les

#IN et IO1 sont d�finis dans d�saisonnalisation
N = matrix(1,3,1)
INreg_trim = create_mat_trimestre(INreg_cjo)%*%N

###########################################################################################################
# Troncature de la p�riode 2008-2009

# la p�riode d�cembre 2008 - decembre 2010 repr�sente les dates 169 � 193
IO1reg_cjo_2 = c(IO1reg_cjo[7:147],IO1reg_cjo[181:219]) 
INreg_trim_2 = c(INreg_trim[8:168],INreg_trim[202:230]) 
print(IO1reg_cjo)

###########################################################################################################
# mod�le trimestriel
res20=lm ( IO1reg_cjo_2[1:209]~INreg_trim_2[1:209]+INreg_trim_2[4:212]+INreg_trim_2[7:215]+INreg_trim_2[10:218]-1)
summary(res20)  
plot(IO1reg_cjo_2)
par(new=T)
plot(fitted(res20))

# v�rification de la non autocorr�lation des r�sidus

# interpr�tation des r�sultats


###########################################################################################################
# projection du mod�le sur le volume engendr� par le simplexe

P=c(coef(res20)[1],coef(res20)[2],coef(res20)[3],coef(res20)[4])
print(P)
P_simplex=proj(P)
print(coef(res20)[1])
# P appartient au simplexe




# MODELE avec IO14 ! annuel



