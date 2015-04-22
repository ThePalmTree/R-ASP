# Modele avec IPC et Conso

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.R")


# declaration des variables
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN
PIB_evol=mydata$PIB_evol


# creation de l a matrice de regression
IN_mat = create_mat(INreg_cjo)

# cette matrice commence à la date 13 et finit à la date 237
# néanmoins, les 6 premières valeurs et les 6 dernières sont N.A


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

# nouveau modele :
# Regression de : IO1_cjo[t]= ( alpha0 + alpha1*PIB_evol(t)+alpha2*Ratio_IPC(t)) * sum(i=1:12)(IN_cjo[t-i]) 

# Creation du vecteur IN_moy, contenant la moyenne mobile des IN. 
M = matrix(1,12,1)
IN_moy = IN_mat%*%M
#IN_moy démarre à la date 13 et termine à la date 237
#ses 6 premières et 6 dernières valeurs sont N.A

# Déclaration des vars
PIB_evol_IN_moy = PIB_evol[35:237]*IN_moy[23:225]

Ratio_IPC_IN_moy = Ratio_IPC[35:237]*IN_moy[23:225]
# ratio_IPC commence à la date 35 et termine à la date 237 (ie 1:202)
# contrairement aux autres grandeurs, ses dernières valeurs ne sont pas N.A
# on fait commencer le PIB à 35 par soucis d'homogénéité avec ratio_IPC
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


# Modele incluant la conso

# Creation des variables de consommation des voitures
conso_VN=mydata$conso_VN
conso_IO1<-c(rep(0,12),mydata$conso_VN[1:225])
print(conso_IO1)
# conso_IO14 : comment determiner quelle annee de consommation VN il faut considerer... ?
# conso_IO4 : meme probleme !


# Creation des indices de consommation
ratio_conso <- conso_IO1/conso_VN[1:237] # N'a de sens (bien-entendu) qu'a partir de 1996
# début date 1 fin date 237

# Modele avec IPC et Conso

# declaration des variables
ratio_conso_IN_moy = ratio_conso[35:237]*IN_moy[23:225]

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


# Modele avec Prix du Carburant

# Creation des variables de consommation des voitures
carbu=mydata$IPC_carbu
#carbu commence à la date 1 et finit à la date 240
carbu_IN_moy = carbu[35:237]*IN_moy[23:225]
carbu_Ratio_conso_IN_moy = carbu[35:237]*ratio_conso_IN_moy[1:203]
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




# Tests de Cointégration

PP.test(IO1reg_cjo[7:210])
# un test de Philipps Pearson ne nous permet pas de rejeter la non stationnarité des variables étudiées
kpss.test(IO1reg_cjo[7:210])
# un test KPSS confirme la non stationnarité (pvalue<0.01)
regression = lm(IO1reg_cjo[7:210] ~ INreg_cjo[7:210] -1)
summary(regression)
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





# Etude des séries différentiées

INreg_lag=INreg_cjo[10:231]-INreg_cjo[7:228]
IO1reg_lag=IO1reg_cjo[10:219]-IO1reg_cjo[7:216]
IN_mat_lag = create_mat(INreg_lag)
IN_moy_lag = IN_mat_lag%*%M

plot(INreg_cjo)
plot(IO1reg_cjo)
plot(IN_moy_lag)

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

# Conclusion: l'étude des séries différenciées ne permet pas d'éliminer la cointégration






# faire avec IO14 pour capter l'effet de l'inovation technologique
# vérifier que ce n'est pas juste l'effet du prix de l'essence
# mensualiser avec le prix de l'essence du mois
# modèle avec trimestres et simplexes