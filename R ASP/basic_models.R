# Ce fichier contient les premiers modeles etudies, ces modeles sont principalement des regressions lineraires

# Nettoyage de l'environement de travail
rm(list=ls())

# Importation des fichiers source necessaires
source("desaisonnalisation.r")
source("basic_functions.r")

# declaration des variables



# Creation de la matrice de regression
IN_mat = create_mat(INt_cjo)


# MODELE DE BASE

# Il faut creer la matrice de regression en ne retenant que la trend de IN puis faire la reression sur la trend
# test de la regression multilineaire de IO sur l'ensemble des IN : modele de base sans contraintes sur les coefficients
res1= lm(IO1t_cjo~IN_mat[1:225,1]+IN_mat[1:225,2]+IN_mat[1:225,3]+IN_mat[1:225,4]+IN_mat[1:225,5]+IN_mat[1:225,6]
         +IN_mat[1:225,7]+IN_mat[1:225,8]+IN_mat[1:225,9]+IN_mat[1:225,10]+IN_mat[1:225,11]+IN_mat[1:225,12])
summary(res1)
plot(res1)
coefficients(res1)
anova(res1)

# les resultats ne semblent pas tres adaptes (constante non nulle, R2 mauvais), cependant une analyse plus poussee
# du modele sans contraintes peut etre interessant, pour voir a quel point le fait de fixer des contraintes reduit la qualite de la regression.

# Test du meme modele mais avec une contrainte de nullite sur la constante 
res2= lm(IO1t_cjo~IN_mat[1:225,1]+IN_mat[1:225,2]+IN_mat[1:225,3]+IN_mat[1:225,4]+IN_mat[1:225,5]+IN_mat[1:225,6]
         +IN_mat[1:225,7]+IN_mat[1:225,8]+IN_mat[1:225,9]+IN_mat[1:225,10]+IN_mat[1:225,11]+IN_mat[1:225,12]-1)
summary(res2)
plot(res2)
coefficients(res2)
anova(res2)
# Les resultats sont bien meilleurs en terme de R2 mais il y a toujours des valeurs de coefficients negatifs, 
# cependant la contrainte qui impose la somme des coefficiants plus petite que 1 semble naturellement respectee.


