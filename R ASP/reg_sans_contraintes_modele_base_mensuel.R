
# veuillez installer le package xlsx, et activer les packages rJava, xlsxjars et xlsx dans cet ordre pour pouvoir importer les fichiers xlsx
library(rJava,xlsxjars)
library(xlsx)

# importation du fichier xls
path = getwd()
mydata = read.xlsx(paste(path,"/GitHub/R-ASP/R ASP/donnees_mensuelles.xlsx",sep=""),1)  # read from first sheet

# déclaration des variables

PIB=mydata$PIB
IPC_VO=mydata$IPC_VO
IPC_VN=mydata$IPC_VN




# regression linéraire de IO1 sur IN
res = lm(IO1 ~ IN,data=mydata)
res

# création d'une matrice dont les colonnes sont le vecteur IN_cjo décalé de l'indice de colonne
# cette matrice prend en compte le coefficient de jours ouvrés
createINd_cjo = function() {
  m = matrix(c(rep(c(rep(0,237)),12)),nrow=237,byrow=TRUE)
  for (j in 1:12) {
    for(i in (j+1):237)
      m[i,j]=mydata$IN_cjo[i-j]
  }
  INd_cjo = m[13:237,1:12] # on supprime les mois où l'on a pas toutes les données à cause du décallage
  return(INd_cjo)
}

INdm = createINd_cjo() # création de la matrice
IO1d =  mydata$IO1_cjo[13:237]# décallage du vecteur pour avoir la bonne taille pour la regréssion


# test de la régression multilinéaire de IO sur l'ensemble des IN : modèle de base sans contraintes sur les coefficients
res1= lm(IO1d~INdm[1:225,1]+INdm[1:225,2]+INdm[1:225,3]+INdm[1:225,4]+INdm[1:225,5]+INdm[1:225,6]
         +INdm[1:225,7]+INdm[1:225,8]+INdm[1:225,9]+INdm[1:225,10]+INdm[1:225,11]+INdm[1:225,12])
summary(res1)
plot(res1)
coefficients(res1)
anova(res1)

# les résultats ne semblent pas très adaptés (constante non nulle, R2 mauvais), cependant une analyse plus poussée
# du modèle sans contraintes peut être interessant, pour voir à quel point le fait de fixer des contraintes réduit la qualité de la régression.

# Test du même modèle mais avec une contrainte de nullité sur la constante 
res2= lm(IO1d~INdm[1:225,1]+INdm[1:225,2]+INdm[1:225,3]+INdm[1:225,4]+INdm[1:225,5]+INdm[1:225,6]
         +INdm[1:225,7]+INdm[1:225,8]+INdm[1:225,9]+INdm[1:225,10]+INdm[1:225,11]+INdm[1:225,12]-1)
summary(res2)
plot(res2)
coefficients(res2)
anova(res2)

# Les résultats sont bien meilleurs en terme de R2 mais il y a toujours des valeurs de coefficiants négatifs, 
# cependant la contrainte qui impose la somme des coefficiants plus petite que un semble naturellement respectée.

# IPCVN et IPCVO désignent les indices de Prix à la consommation de l'INSEE : Indice Prix à la consommation Voiture Neuve/Occasion
# Plus d'information concernant ces indices et leur construction dans le pdf de méthodologie de l'INSEE joint au dossier Git
# Le coefficient CJO_5J pondère en fonction du nombre de jours ouvrés

# Régression de : IO1_cjo[t]= sum(i=1:12)( alpha(i,t)*IN_cjo[t-i]  ) 
# avec apha(i,t)= beta0(i)+beta1(i)*PIB(t)+beta2*IPC_VN(t)+beta3(i)*IPC_VO(t)

# ce modèle rend compte de l'arbitrage du consommateur entre le prix des voitures neuves et celles d'occasion
# et de l'influence du context économique (PIB)
res3= lm(IO1d~
           (1+PIB[13:237]+IPC_VO[13:237]+IPC_VN[13:237])*(INdm[1:225,1]
         +INdm[1:225,2]
         +INdm[1:225,3]
         +INdm[1:225,4]
         +INdm[1:225,5]
         +INdm[1:225,6]
         +INdm[1:225,7]
         +INdm[1:225,8]
         +INdm[1:225,9]
         +INdm[1:225,10]
         +INdm[1:225,11]
         +INdm[1:225,12]-1))
summary(res3)
plot(res3)
coefficients(res3)
anova(res3)

# analyse des résultats :
# le prix des voitures neuves a un impact plus significatif que IPC_VO
# les coefficients sur les IN sont mauvais -> il faut désaisonnaliser les séries
# le R² ajusté est de 0,991 e la p-value totale de 2,2e-16 !

















