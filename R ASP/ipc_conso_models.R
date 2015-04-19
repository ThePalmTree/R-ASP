# Modele avec IPC et Conso

# Nettoyage de l'environement de travail
rm(list=ls())

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.R")


# declaration des variables
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN
PIB_evol=mydata$PIB_evol


# creation de l a matrice de regression
IN_mat = create_mat(INt_cjo)

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

# Déclaration des vars
PIB_evol_IN_moy = PIB_evol[35:233]*IN_moy[22:220]
Ratio_IPC_IN_moy = Ratio_IPC[35:233]*IN_moy[22:220]

res4= lm(IO1t_cjo[35:233]~
           PIB_evol_IN_moy + Ratio_IPC_IN_moy + IN_moy[22:220] - 1)

summary(res4)
plot(res4)
coefficients(res4)
anova(res4)
# le signe du coeff devant IN_moy est anormal.


# Modele sans la croissance

res5= lm(IO1t_cjo[35:233]~
           Ratio_IPC_IN_moy + IN_moy[22:220] - 1)

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


# Modele avec IPC et Conso

# declaration des variables
ratio_conso_IN_moy = ratio_conso[35:233]*IN_moy[22:220]

# modele : IO1_cjo[t]= ( alpha0 + alpha1*ratio_conso(t)+alpha2*Ratio_IPC(t)) * sum(i=1:12)(IN_cjo[t-i]) 

res5= lm(IO1t_cjo[35:233]~
           ratio_conso_IN_moy + Ratio_IPC_IN_moy + IN_moy[22:220] - 1)

summary(res5)
plot(res5)
coefficients(res5)
anova(res5)

# On a enfin un coeff devant IN_moy qui est cohérent
# Les autres restent à analyser


















# faire avec IO14 pour capter l'effet de l'inovation technologique
# vérifier que ce n'est pas juste l'effet du prix de l'essence
# mensualiser avec le prix de l'essence du mois