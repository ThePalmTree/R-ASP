# Modele avec IPC et Conso

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.R")



###########################################################################################################


######## declaration des variables
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN
PIB_evol=mydata$PIB_evol
carbu=mydata$IPC_carbu
#carbu commence à la date 1 et finit à la date 240

# dummy pour supprimer l'effet de la pime à la casse : date 158 à 181
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
# début date 1 fin date 237


######## creation de l a matrice de regression
IN_mat = create_mat(IN_cjo)

# cette matrice commence à la date 13 et finit à la date 237
# néanmoins, les 6 premières valeurs et les 6 dernières sont N.A


######## Creation du vecteur IN_moy, contenant la moyenne mobile des IN. 
M = matrix(1,12,1)
IN_moy = create_mat(IN_cjo)%*%M
#IN_moy démarre à la date 13 et termine à la date 237
#ses 6 premières et 6 dernières valeurs sont N.A

######## declaration des variables multiplicatives
ratio_conso_IN_moy = ratio_conso[35:237]*IN_moy[23:225]
PIB_evol_IN_moy = PIB_evol[35:237]*IN_moy[23:225]
Ratio_IPC_IN_moy = Ratio_IPC[35:237]*IN_moy[23:225]
# ratio_IPC commence à la date 35 et termine à la date 237 (ie 1:202)
# contrairement aux autres grandeurs, ses dernières valeurs ne sont pas N.A
# on fait commencer le PIB à 35 par soucis d'homogénéité avec ratio_IPC
carbu_IN_moy = carbu[35:237]*IN_moy[23:225]
carbu_Ratio_conso_IN_moy = carbu[35:237]*ratio_conso_IN_moy[1:203]

######## declaration des variables différentiées
IN_lag=IN_cjo[2:237]-IN_cjo[1:236]
IO1_lag=IO1_cjo[2:225]-IO1_cjo[1:224]

Ratio_IPC_lag = Ratio_IPC[36:237]-Ratio_IPC[35:236]
carbu_IN_moy_lag=carbu_IN_moy[2:202]-carbu_IN_moy[1:201]
Ratio_IPC_IN_moy_lag=Ratio_IPC_IN_moy[2:202]-Ratio_IPC_IN_moy[1:201]

####### declaration des variables log
log_IN_moy = log(create_mat(IN_cjo)%*%M) #13 à 237
log_IO1t = log(IO1t_cjo) #13à 18: NA / 19 à 231  /232 à 237: NA
log_carbu_ratio_conso_ratio_IPC =log(ratio_conso [35:237]+carbu[35:237]+Ratio_IPC[35:237])

####### declaration des variables log sans prime à la casse
log_IN_moy_D = log_IN_moy[23:219]*D[35:231]
log_IO1t_D = log_IO1t[35:231]*D[35:231]
log_carbu_ratio_conso_ratio_IPC_D = log_carbu_ratio_conso_ratio_IPC[1:197]*D[35:231]

###########################################################################################################

####### Tests de Cointégration

PP.test(IO1reg_cjo[7:210])
# un test de Philipps Pearson ne nous permet pas de rejeter la non stationnarité des variables étudiées
kpss.test(IO1reg_cjo[7:210])
# un test KPSS confirme la non stationnarité (pvalue<0.01)
regression = lm(IO1reg_cjo[7:210] ~ INreg_cjo[7:210] -1)
beta = coef(regression)[1]
adf.test(cbind(IO1reg_cjo[7:210]-beta*INreg_cjo[7:210]))
# un test ADF sur le spread confirme la cointégration (pvalue<0.09)
# On a bien une cointégration entre IN et IO, cependant, il semblerait que cette cointégration soit logique en
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

####### Etude des séries différentiées

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

# On retient la différenciation au premier degré de lag

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

# Conclusion: l'étude des séries différenciées ne permet pas d'éliminer la cointégration pour les séries reg mais
# fonctionne très bien pour les séries IO1 et IN brutes

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
#           PP test: pvalue=0.01 -> ne permet pas de rejeter la non stationnarité
#           KPSS test: pvalue>0.01 -> ne permet pas de rejeter la stationnarité
# rien ne permet de justifier que les séries étudiées sont bien stationnaires, peu importe le lag

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
# de plus il n'est pas certain que le modèle estimé soit bient celui que l'on souhaite étudier

###########################################################################################################

# nouveau modele :
# Regression de : IO1_cjo[t]= ( alpha0 + alpha1*PIB_evol(t)+alpha2*Ratio_IPC(t)) * sum(i=1:12)(IN_cjo[t-i]) 


res4= lm(IO1t_cjo[23:219]~
           PIB_evol_IN_moy[1:197] + Ratio_IPC_IN_moy[1:197] + IN_moy[23:219] - 1)
# on fait commencer IO1t_cjo à 23 (ie à la date 35) et terminer à 219 (ie date 231) pour supprimer les N.A
# on fait commencer PIB à 1 (ie date 35) et terminer à 197 (ie date 231)
# on fait commencer Ratio à 1 et finir à 197 de même
# on fait commencer IN_moy à 23 (ie date 35) et terminer à 219 (ie 231)
summary(res4)
plot(res4)
coefficients(res4)
anova(res4)
# le signe du coeff devant IN_moy est anormal.
# le coefficient devant IN_moy est beaucoup trop grand, il explique à lui seul l'inertie
# le pib est à chier

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

# Une fois de plus le coefficiant négatif devant IN_moy est anormal. 

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

# On a enfin un coeff devant IN_moy qui est cohérent
# Les autres restent à analyser

###########################################################################################################

# Modele avec Prix du Carburant

# Carbu représente le prix du carburant
# Carbu*Ratio_conso pondère l'importance d'une conommation faible
# ie: l'arbitrage entre l'achat d'une voiture d'occasion ou celle d'une voiture neuve dépend du coût d'utilisation
# rq: si Carbu n'a pas d'influence significative, alors les conso intègrent le coût d'utilisation dans conso uniquement
# rq: si en revanche Carbu a une influence, on deux phénomènes : arbitrage à l'achat et arbitrage contextuel


# On régresse en ajoutant le Bruit et avec un échantillon réduit.
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

# On obtient un excellent R² ajusté, cela nous a amené à faire le même test sur une plus petite portion de 
# l'échantillon pour tester une éventuelle co-intégration
# On obtient de très mauvais résultats de prévision


###########################################################################################################

# Regression avec les variables stationnarisées

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
# modèle : log(IO1) = log(carbu + ratioconso + ratioIPC) + log(INmoy)
# soit Y = A + B


# avec interférence de la prime à la casse
res9= lm(log_IO1t[35:231]~ log_carbu_ratio_conso_ratio_IPC[1:197] + log_IN_moy[23:219])
summary(res9)
plot(res9)
coefficients(res9)
anova(res9)
plot(log_IO1t[35:231])
par(new=T)
plot(fitted(res9))

# R²=0.77

# sans la prime à la casse

res9b= lm(log_IO1t_D~ log_carbu_ratio_conso_ratio_IPC_D + log_IN_moy_D)
summary(res9b)
plot(res9b)
coefficients(res9b)
anova(res9b)
plot(log_IO1t_D[35:231])
par(new=T)
plot(fitted(res9b))

# R² = 0.997




###########################################################################################################

# faire avec IO14 pour capter l'effet de l'inovation technologique
# vérifier que ce n'est pas juste l'effet du prix de l'essence
# mensualiser avec le prix de l'essence du mois
# modèle avec trimestres et simplexes
# dummmy pour prime à la casse