# Modele avec IPC, Ratio_conso, Carbu
# Deux versions : multiplicatif et log lineaire

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)
library(MSBVAR)

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.R")

###########################################################################################################
# Variables utilisées dans les modèles

#IN et IO1 sont définis dans désaisonnalisation
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN
croissance=mydata$Croissance
carbu=mydata$IPC_carbu
conso_VN=mydata$conso_VN
conso_IO1=c(rep(0,12),mydata$conso_VN[1:225])
ratio_conso=conso_IO1/conso_VN[1:237] 
M = matrix(1,12,1)
INt_moy = create_mat(INt_cjo)%*%M

croissance_INt_moy = croissance[35:231]*INt_moy[23:219]
ratio_conso_INt_moy = ratio_conso[35:231]*INt_moy[23:219]
Ratio_IPC_INt_moy = Ratio_IPC[35:231]*INt_moy[23:219]
carbu_INt_moy = carbu[35:231]*INt_moy[23:219]

# Variables utilisées dans les tests
IN_moy = create_mat(INt_cjo)%*%M
INres_moy= IN_moy[7:219]-INt_moy[7:219]
INt_moy_2 = c(INt_moy[23:169],INt_moy[193:219])
print(IO1t_cjo_2)
print(INt_moy_2)

###########################################################################################################
# Troncature de la période 2008-2009

# la période décembre 2008 - decembre 2010 représente les dates 169 à 193
IO1t_cjo_2 = c(IO1t_cjo[23:157],IO1t_cjo[181:219])
ratio_conso_INt_moy_2 = c(ratio_conso_INt_moy[1:135],ratio_conso_INt_moy[159:197])
Ratio_IPC_INt_moy_2 = c(Ratio_IPC_INt_moy[1:135],Ratio_IPC_INt_moy[159:197])
carbu_INt_moy_2 = c(carbu_INt_moy[1:135],carbu_INt_moy[159:197])
croissance_INt_moy_2 = c(croissance_INt_moy[1:135],croissance_INt_moy[159:197])

###########################################################################################################
# Vérification des hypothèses liées au traitement des données

# Vérification que les résidus de la désaisonnalisation sont des BB
# test porte-manteau réalisé dans le dossier désaisonnalisation
# normalement R désaisonnalise en mettant le résidu BB
 
###########################################################################################################
# Critique de la pertinence du PIB (de la croissance)
res10= lm(IO1t_cjo_2 ~ croissance_INt_moy_2 )
summary(res10)

plot(IO1t_cjo_2)
par(new=T)
plot(fitted(res10))
#R2 0.2, pvalue ***, la croissance semble expliquer l'arbitrage entre l'achat de voiture neuve ou d'occasion

print(IO1t_cjo_2)

# Test de l'homoscédasticité des résidus
residu=cbind(IO1t_cjo_2[1:174])-cbind(fitted(res10)[1:174])
print(residu)
print(cbind(fitted(res10)))
print(cbind(IO1t_cjo_2))
bartlett.test(list(residu[1:87]),list(residu[88:174]))
#/!\

# Test de GRANGER
granger.test(cbind(IO1t_cjo_2,croissance_INt_moy_2),12)
# H0 Croissance do not granger_cause IO1:
# pvalue = 1,68*10^-8
# -> on ne peut pas rejeter H0
# H0' IO1 do not granger_cause croissance:
# pvalue = 6,79*10^-2
# -> on ne peut pas rejeter H0'
# les pvalues très faibles suggèrent qu'il n'existe pas de causalité (au sens de GRANGER) entre IO1 et la croissance

###########################################################################################################
# Modèle multiplicatif

res11= lm(IO1t_cjo_2 ~ ratio_conso_INt_moy_2 + Ratio_IPC_INt_moy_2 )
summary(res11)

plot(IO1t_cjo_2)
par(new=T)
plot(fitted(res11))

# Test de la normalité des résidus MLR 3
# je ne sais pas comment tester ça

# Test de l'homoscédasticité des résidus MLR 5
bartlett.test(IO1t_cjo_2-fitted(res11))
#/!\ pb dans le code, à revoir

# Analyse critique des résultats
# R2 ajusté : 0,69, toutes les p-values à ***
# Ratio IPC * IN_moy a une pvalue particulièrement petite : capte l'arbitrage des prix
# Ratio conso * IN_moy a un coeff négatif : effet inverse de ratio IPC : favorise IN en cas de faible consommation des voitures neuves

# Remarque 0 : cela est rendu possible en supprimant la période décembre 2008 - décembre 2010 (prime à la casse)
# pour rappel, sans supprimer cette période, on a un R2 de 0,97 et une cointégration à 1% donnée par ADF

# Remarque 1: si on ajoute croissance * IN_moy au modèle, alors :
# cette var a une pvalue faible mais la plus grande de toutes et le R2 ajusté devient 0,70
# d'autre part, ajouter la croissance modifie le coefficient de ratio conso * IN
# ce phénomène n'a aucun fondement économique et résulte uniquement de cointégration entre la croissance et le ratio conso
# en effet, ces deux séries sont monotones

# Test de cointégration
regression = lm(IO1reg_cjo[7:210] ~ INreg_cjo[7:210] -1)
summary(regression)
# R2 de 0,95 ! pvalue <2e-16
beta = coef(regression)[1]
adf.test(cbind(IO1reg_cjo[7:210]-beta*INreg_cjo[7:210]))
# LAG : 5 ; pvalue = 0.116
kpss.test(cbind(IO1reg_cjo[7:210]-beta*INreg_cjo[7:210]))
# LAG : 3 ; pvalue = 0.01
# ADF ne permet pas d'exclure la stationnarité de la série IO1-Beta*IN à 10% et donc d'exclure la cointégration
# KPSS confirme la non stationnarité avec une pvalue très petite (ne permet pas de rejeter H1 : la non cointégration)

regression = lm(IO1t_cjo_2 ~ INt_moy_2 -1)
summary(regression)
# R2 0.98

# On en déduit qu'il n'y a pas de cointégration entre IO1 et IN, le R2 mesuré est donc significatif

# Remarque 2 : le fait que le R2 soit si bon constitue déjà une bonne preuve en soi de la cointégration
# Remarque 3 : IN et IO1 devraient logiquement être fortement cointégrés

# Mesure de la causalité : GRANGER test
granger.test(cbind(IO1t_cjo_2,INt_moy_2),12)
# H0 : IN do not granger_cause IO
# pvalue O,998 !! -> rejet catégorique de H0 à 0,1 %
# IN influe IO
# H0' : IO do not granger_cause IN
# pvalue 0,367 -> rejet également de H0' à 5%
# IO influence également IN dans une moindre mesure
# économiquement, il est tout à fait raisonnable que le marché d'occasion influence rétroactivment celui du neuf
# le lag considéré étant de 12 mois (dans les deux sens?)
# les résultats de ce test sont excellents
granger.test(cbind(IO1t_cjo_2, ratio_conso_INt_moy_2, Ratio_IPC_INt_moy_2, croissance_INt_moy_2),12)
# met en évidence l'importance de toutes les variables pour expliquer IO1t excepté la croissance
# parfaitement cohérent avec tous les résultats précédents
# les autres explications avec une pvalue faible traduisent la non inter-explication des variables explicatives déjà
# mis en avant lors de l'étude des corrélations
# les autres explications inter-variables explicatives sont anormalement hautes du fait de la présence de IN comme facteur 
# multiplicatif dans les deux variables et ne pose pas de problème.

###########################################################################################################
# Modèle VAR

















