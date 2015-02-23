
# veuillez installer le package xlsx, et activer les packages rJava, xlsxjars et xlsx dans cet ordre pour pouvoir importer les fichiers xlsx
library(rJava,xlsxjars)
library(xlsx)

# importation du fichier xls
path = getwd()
mydata = read.xlsx(paste(path,"/donnees_mensuelles.xlsx",sep=""),1)  # read from first sheet

# regression linéraire de IO1 sur IN
res = lm(IO1 ~ IN,data=mydata)
res

# création d'une matrice dont les collonnes sont le vecteur IN décalé de l'indice de colonne
createINd = function() {
  m = matrix(c(rep(c(rep(0,237)),12)),nrow=237,byrow=TRUE)
  for (j in 1:12) {
    for(i in (j+1):237)
      m[i,j]=mydata$IN[i-j]
  }
  INd = m[13:237,1:12] # on supprime les mois où l'on a pas toutes les données à cause du décallage
  return(INd)
}

INdm = createINd() # création de la matrice
IO1d =  mydata$IO1[13:237]# décallage du vecteur pour avoir la bonne taille pour la regréssion


# test de la régression multilinéaire de IO sur l'ensemble des IN : modèle de base sans contraintes sur les coefficiants

res1= lm(IO1d~INdm[1:225,1]+INdm[1:225,2]+INdm[1:225,3]+INdm[1:225,4]+INdm[1:225,5]+INdm[1:225,6]
         +INdm[1:225,7]+INdm[1:225,8]+INdm[1:225,9]+INdm[1:225,10]+INdm[1:225,11]+INdm[1:225,12])
summary(res1)
plot(res1)
coefficients(res1)
anova(res1)

# les résultats ne semblent pas très adaptés (constante non nulle, R2 à chier), certains coefficiants sont plus grands que 1, cependant une analyse plus poussée
# du modèle sans contraintes peut être interessant, pour voir à quel point le fait de fixer des contraintes réduit la qualité de la régression.

# il faut trouver un package qui possède une fonction comme lm() mais avec des contraintes sur les coefficiants

