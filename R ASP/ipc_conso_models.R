# Modele avec IPC et Conso

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)

#Importation des fichiers R n�c�ssaires
source("desaisonnalisation.r")
source("basic_functions.R")



###########################################################################################################


######## declaration des variables
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN
PIB_evol=mydata$PIB_evol
carbu=mydata$IPC_carbu
#carbu commence � la date 1 et finit � la date 240

# dummy pour supprimer l'effet de la pime � la casse : date 158 � 181
D=c(rep(1,158),rep(0,23),rep(1,56))
print(D)
plot(D[7:231]*IO1t_cjo[7:231])
plot(IO1t_cjo[7:231])
par(new=T)


# Creation des variables de consommation des voitures
conso_VN=mydata$conso_VN
conso_IO1<-c(rep(0,12),mydata$conso_VN[1:225])
# conso_IO14 : comment determiner quelle annee de consommation VN il faut considerer... ?
# conso_IO4 : meme probleme !

# Creation des indices de consommation
ratio_conso <- conso_IO1/conso_VN[1:237] 
# N'a de sens (bien-entendu) qu'a partir de 1996
# d�but date 1 fin date 237


######## creation de l a matrice de regression
IN_mat = create_mat(IN_cjo)

# cette matrice commence � la date 13 et finit � la date 237
# n�anmoins, les 6 premi�res valeurs et les 6 derni�res sont N.A


######## Creation du vecteur IN_moy, contenant la moyenne mobile des IN. 
M = matrix(1,12,1)
IN_moy = create_mat(IN_cjo)%*%M
#IN_moy d�marre � la date 13 et termine � la date 237
#ses 6 premi�res et 6 derni�res valeurs sont N.A

######## declaration des variables multiplicatives
ratio_conso_IN_moy = ratio_conso[35:237]*IN_moy[23:225]
PIB_evol_IN_moy = PIB_evol[35:237]*IN_moy[23:225]
Ratio_IPC_IN_moy = Ratio_IPC[35:237]*IN_moy[23:225]
# ratio_IPC commence � la date 35 et termine � la date 237 (ie 1:202)
# contrairement aux autres grandeurs, ses derni�res valeurs ne sont pas N.A
# on fait commencer le PIB � 35 par soucis d'homog�n�it� avec ratio_IPC
carbu_IN_moy = carbu[35:237]*IN_moy[23:225]
carbu_Ratio_conso_IN_moy = carbu[35:237]*ratio_conso_IN_moy[1:203]

######## declaration des variables diff�renti�es
IN_lag=IN_cjo[2:237]-IN_cjo[1:236]
IO1_lag=IO1_cjo[2:225]-IO1_cjo[1:224]

Ratio_IPC_lag = Ratio_IPC[36:237]-Ratio_IPC[35:236]
carbu_IN_moy_lag=carbu_IN_moy[2:202]-carbu_IN_moy[1:201]
Ratio_IPC_IN_moy_lag=Ratio_IPC_IN_moy[2:202]-Ratio_IPC_IN_moy[1:201]

####### declaration des variables log
log_IN_moy = log(create_mat(IN_cjo)%*%M) #13 � 237
log_IO1t = log(IO1t_cjo) #13� 18: NA / 19 � 231  /232 � 237: NA
log_carbu_ratio_conso_ratio_IPC =log(ratio_conso [35:237]+carbu[35:237]+Ratio_IPC[35:237])

####### declaration des variables log sans prime � la casse et la crise
# tronqu�s de 146 � 181
# Les vecteurs suivant vont de la date 35 � 146 puis de 181 � 231
#taille 175
log_IN_moy_D = c(log_IN_moy[23:134],log_IN_moy[169:219]) 
log_IO1t_D = c(log_IO1t[35:146],log_IO1t[181:231])
log_carbu_ratio_conso_ratio_IPC_D = c(log_carbu_ratio_conso_ratio_IPC[1:112],log_carbu_ratio_conso_ratio_IPC[147:197])

###########################################################################################################

####### Tests de Coint�gration

PP.test(IO1reg_cjo[7:210])
# un test de Philipps Pearson ne nous permet pas de rejeter la non stationnarit� des variables �tudi�es
kpss.test(IO1reg_cjo[7:210])
# un test KPSS confirme la non stationnarit� (pvalue<0.01)
regression = lm(IO1reg_cjo[7:210] ~ INreg_cjo[7:210] -1)
beta = coef(regression)[1]
adf.test(cbind(IO1reg_cjo[7:210]-beta*INreg_cjo[7:210]))
# un test ADF sur le spread confirme la coint�gration (pvalue<0.09)
# On a bien une coint�gration entre IN et IO, cependant, il semblerait que cette coint�gration soit logique en
# raison de la nature des variables
carbu_ratio_conso=carbu[35:237]*ratio_conso[35:237]

#mydata2 <- cbind(IO1reg_cjo[23:219],
#                Ratio_IPC[35:231],
#                carbu[35:231],
#                ratio_conso[35:231],
#                carbu_ratio_conso[1:197],
#                carbu_IN_moy[1:197],
#                carbu_Ratio_conso_IN_moy[1:197],
#                ratio_conso_IN_moy[1:197],
#                Ratio_IPC_IN_moy[1:197], 
#                IN_moy[23:219])

cor(Ratio_IPC[35:231],carbu[35:231])
# -0.73
plot(Ratio_IPC[35:231])
plot(carbu[35:231])
cor(Ratio_IPC[35:231],ratio_conso[35:231])
# -0.46
cor(Ratio_IPC[35:231],carbu_ratio_conso[1:197])
# -0.73
cor(carbu[35:231],ratio_conso[35:231])
# 0.52
cor(carbu[35:231],carbu_ratio_conso[1:197])
# 0.99
cor(ratio_conso[35:231],carbu_ratio_conso[1:197])
# 0.58

plot(carbu)
plot(ratio_conso[35:237])
res=lm(carbu[35:231]~ratio_conso[35:231])
summary(res)
cor(IO1reg_cjo[23:219],ratio_conso[35:231])

print(IO1reg_cjo)

####### Etude des s�ries diff�renti�es

PP.test(IN_moy_lag[1:223]) 
# lag=1:pvalue 0.01
# lag=2:pvalue 0.01
# lag=3:pvalue 0.01
kpss.test(IN_moy_lag[1:223]) 
# lag=1:pvalue 0.1
# lag=2:pvalue >0.1
# lag=3:pvalue >0.1
print(IN_moy_lag[1:225])

PP.test(IO1_lag[1:222]) 
# lag=1:pvalue 0.01
# lag=2:pvalue 0.01
# lag=3:pvalue 0.01
kpss.test(IO1_lag[1:222]) 
# lag=1:pvalue >0.1
# lag=2:pvalue >0.1
# lag=3:pvalue >0.1
print(IO1_lag[1:210])

regression = lm(IO1_lag[1:223] ~ IN_moy_lag[1:223] -1)
beta = coef(regression)[1]
adf.test(cbind(IO1_lag[1:223]-beta*IN_moy_lag[1:223]))
summary(regression)
# lag=1:pvalue 0.01<
# lag=2:pvalue 0.01<
# lag=3:pvalue 0.01<

# On retient la diff�renciation au premier degr� de lag

PP.test(IN_moy_lag[1:211]) 
# lag=1:pvalue 0.2643
# lag=2:pvalue 0,2912
# lag=3:pvalue 0,3166
kpss.test(IN_moy_lag[1:211]) 
# lag=1:pvalue 0.0834
# lag=2:pvalue 0,0817
# lag=3:pvalue 0,0787
print(IN_moy_lag[1:211])

PP.test(IO1reg_lag[1:210]) 
# lag=1:pvalue 0.0201
# lag=2:pvalue 0.1639
# lag=3:pvalue 0.2886
kpss.test(IO1reg_lag[1:210]) 
# lag=1:pvalue 0.01
# lag=2:pvalue 0.01
# lag=3:pvalue 0.01
print(IO1reg_lag[1:210])

regression = lm(IO1reg_lag[1:210] ~ IN_moy_lag[1:210] -1)
beta = coef(regression)[1]
adf.test(cbind(IO1reg_lag[1:210]-beta*IN_moy_lag[1:210]))
# lag=3:pvalue 0.1161

# Conclusion: l'�tude des s�ries diff�renci�es ne permet pas d'�liminer la coint�gration pour les s�ries reg mais
# fonctionne tr�s bien pour les s�ries IO1 et IN brutes

PP.test(Ratio_IPC_lag[1:202]) 
# lag=1:pvalue 0.01
kpss.test(Ratio_IPC_lag[1:202]) 
# lag=1:pvalue 0.09

carbu_lag = carbu[2:237]-carbu[1:236]
plot(carbu_lag)
PP.test(carbu_lag[1:236]) 
# lag=1:pvalue 0.01
kpss.test(carbu_lag[1:236]) 
# lag=1:pvalue 0.01<

ratio_conso_lag = ratio_conso[48:237]-ratio_conso[35:224]
print(ratio_conso_lag)
plot(ratio_conso_lag[1:190])
PP.test(ratio_conso_lag[1:190]) 
# lag=1:pvalue 0.088 bof
kpss.test(ratio_conso_lag[1:190]) 
# lag=1:pvalue 0.01<


#Conclusion: on a 
#           PP test: pvalue=0.01 -> ne permet pas de rejeter la non stationnarit�
#           KPSS test: pvalue>0.01 -> ne permet pas de rejeter la stationnarit�
# rien ne permet de justifier que les s�ries �tudi�es sont bien stationnaires, peu importe le lag

###########################################################################################################

# IPCVN et IPCVO designent les indices de Prix a la consommation de l'INSEE : Indice Prix a la consommation Voiture Neuve/Occasion
# Plus d'information concernant ces indices et leur construction dans le pdf de methodologie de l'INSEE joint au dossier Git
# Le coefficient CJO_5J pondere en fonction du nombre de jours ouvres
# Regression de : IO1_cjo[t]= sum(i=1:12)( alpha(i)*IN_cjo[t-i]  ) 
# avec apha(i,t)= beta0(i)+beta1(i)*PIB_evol(t)+beta2*Ratio_IPC(t)
# ce modele rend compte de l'arbitrage du consommateur entre le prix des voitures neuves et celles d'occasion
# et de l'influence du context economique (PIB)

   res3= lm(IO1t_cjo[35:233]~
           (PIB_evol[35:233]+Ratio_IPC[35:233])*(IN_mat[22:220,1]
                                                   +IN_mat[22:220,2]
                                                   +IN_mat[22:220,3]
                                                   +IN_mat[22:220,4]
                                                   +IN_mat[22:220,5]
                                                   +IN_mat[22:220,6]
                                                   +IN_mat[22:220,7]
                                                   +IN_mat[22:220,8]
                                                   +IN_mat[22:220,9]
                                                   +IN_mat[22:220,10]
                                                   +IN_mat[22:220,11]
                                                   +IN_mat[22:220,12]-1))
summary(res3)
plot(res3)
coefficients(res3)
anova(res3)

# le probleme d'overfit et le fait que les elasticites des consommateurs sont constantes dans le temps nous amene a considerer un nouveau modele
# de plus il n'est pas certain que le mod�le estim� soit bient celui que l'on souhaite �tudier

###########################################################################################################

# nouveau modele :
# Regression de : IO1_cjo[t]= ( alpha0 + alpha1*PIB_evol(t)+alpha2*Ratio_IPC(t)) * sum(i=1:12)(IN_cjo[t-i]) 


res4= lm(IO1t_cjo[23:219]~
           PIB_evol_IN_moy[1:197] + Ratio_IPC_IN_moy[1:197] + IN_moy[23:219] - 1)
# on fait commencer IO1t_cjo � 23 (ie � la date 35) et terminer � 219 (ie date 231) pour supprimer les N.A
# on fait commencer PIB � 1 (ie date 35) et terminer � 197 (ie date 231)
# on fait commencer Ratio � 1 et finir � 197 de m�me
# on fait commencer IN_moy � 23 (ie date 35) et terminer � 219 (ie 231)
summary(res4)
plot(res4)
coefficients(res4)
anova(res4)
# le signe du coeff devant IN_moy est anormal.
# le coefficient devant IN_moy est beaucoup trop grand, il explique � lui seul l'inertie
# le pib est � chier

###########################################################################################################

# Modele sans la croissance

res5= lm(IO1t_cjo[23:219]~
          Ratio_IPC_IN_moy[1:197] + IN_moy[23:219] - 1)
plot(IO1t_cjo)
par(new=T)
plot(fitted(res5))

summary(res5)
plot(res5)
coefficients(res5)
anova(res5)

# Une fois de plus le coefficiant n�gatif devant IN_moy est anormal. 

###########################################################################################################

# Modele avec IPC et Conso

# modele : IO1_cjo[t]= ( alpha0 + alpha1*ratio_conso(t)+alpha2*Ratio_IPC(t)) * sum(i=1:12)(IN_cjo[t-i]) 

res6= lm(IO1t_cjo[35:231]~
           ratio_conso_IN_moy[1:197] + Ratio_IPC_IN_moy[1:197] + IN_moy[23:219] - 1)

summary(res6)
plot(res6)
coefficients(res6)
anova(res6)
plot(IO1t_cjo)
par(new=T)
plot(fitted(res6))

# On a enfin un coeff devant IN_moy qui est coh�rent
# Les autres restent � analyser

###########################################################################################################

# Modele avec Prix du Carburant

# Carbu repr�sente le prix du carburant
# Carbu*Ratio_conso pond�re l'importance d'une conommation faible
# ie: l'arbitrage entre l'achat d'une voiture d'occasion ou celle d'une voiture neuve d�pend du co�t d'utilisation
# rq: si Carbu n'a pas d'influence significative, alors les conso int�grent le co�t d'utilisation dans conso uniquement
# rq: si en revanche Carbu a une influence, on deux ph�nom�nes : arbitrage � l'achat et arbitrage contextuel


# On r�gresse en ajoutant le Bruit et avec un �chantillon r�duit.
res7= lm(IO1reg_cjo[35:131]~
           carbu_IN_moy[1:97] + 
           carbu_Ratio_conso_IN_moy[1:97] + 
           ratio_conso_IN_moy[1:97] + 
           Ratio_IPC_IN_moy[1:97] + 
           IN_moy[23:119] - 1)

summary(res7)
plot(res7)
coefficients(res7)
anova(res7)
plot(IO1t_cjo)
par(new=T)
plot(fitted(res7))

A=-0.014400*carbu_IN_moy[1:197]+0.014167*carbu_Ratio_conso_IN_moy[1:197]-1.633273*ratio_conso_IN_moy[1:197]-0.034510*Ratio_IPC_IN_moy[1:197]+1.710943*IN_moy[23:219]

plot(IO1t_cjo)
par(new=T)
plot(A)

# On obtient un excellent R� ajust�, cela nous a amen� � faire le m�me test sur une plus petite portion de 
# l'�chantillon pour tester une �ventuelle co-int�gration
# On obtient de tr�s mauvais r�sultats de pr�vision


###########################################################################################################

# Regression avec les variables stationnaris�es

res8= lm(IO1reg_lag[23:124]~
           carbu_IN_moy_lag[1:102] + 
#           carbu_Ratio_conso_IN_moy[1:197] + 
#           ratio_conso_IN_moy_lag[1:197] + 
           Ratio_IPC_IN_moy_lag[1:102] + 
           INreg_moy_lag[24:125] - 1)

summary(res8)
plot(res8)
coefficients(res8)
anova(res8)
plot(IO1_lag)
par(new=T)
plot(fitted(res8))
A=coef(res8)[1]*carbu_IN_moy[1:202]+coef(res8)[2]*Ratio_IPC_IN_moy[1:202]+coef(res8)[3]*IN_moy[24:225]

plot(IO1t_cjo)
par(new=T)
plot(A)

###########################################################################################################

# regression avec log (modele lineaire)
# mod�le : log(IO1) = log(carbu + ratioconso + ratioIPC) + log(INmoy)
# soit Y = A + B


# avec interf�rence de la prime � la casse
res9= lm(log_IO1t[35:231]~ log_carbu_ratio_conso_ratio_IPC[1:197] + log_IN_moy[23:219])
summary(res9)
plot(res9)
coefficients(res9)
anova(res9)
plot(log_IO1t[35:231])
par(new=T)
plot(fitted(res9))

# R�=0.77

# sans la prime � la casse et crise de 2007 (dates tronqu�es de 146 � 181 )

res9b= lm(log_IO1t_D~ log_carbu_ratio_conso_ratio_IPC_D + log_IN_moy_D)
summary(res9b)
plot(res9b)
coefficients(res9b)
anova(res9b)
plot(log_IO1t_D)
par(new=T)
plot(fitted(res9b))

# R� = 0.83

par(new = TRUE)
plot(fitted(res9b)[1:100], log_IO1t_D[1:100], axes = FALSE)
axis(side=4, at = pretty(range(z)))

cor(carbu[35:237],Ratio_IPC[1:203])
###########################################################################################################

# faire avec IO14 pour capter l'effet de l'inovation technologique
# v�rifier que ce n'est pas juste l'effet du prix de l'essence
# mensualiser avec le prix de l'essence du mois
# mod�le avec trimestres et simplexes
# dummmy pour prime � la casse