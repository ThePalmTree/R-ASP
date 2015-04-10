
# veuillez installer le package xlsx, et activer les packages rJava, xlsxjars et xlsx dans cet ordre pour pouvoir importer les fichiers xlsx
library(rJava,xlsxjars)
library(xlsx)

# importation du fichier xls
path = getwd()
mydata = read.xlsx("C:\\Users\\lenovo\\Documents\\GitHub\\R-ASP\\R ASP\\donnees_mensuelles.xlsx",1)  # read from first sheet

# declaration des variables

PIB=mydata$PIB
PIB_evol=mydata$PIB_evol
IPC_VO=mydata$IPC_VO
IPC_VN=mydata$IPC_VN
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN



# CREATION DES OBJETS TIMESERIES
require(graphics)
INts<-ts(mydata$IN, start=c(1995, 1), end=c(2014, 9), frequency=12) 

# REPRESENTATION DECOMPOSITION
plot(stl(INts,"per"))

# DECOMPOSITION EFFECTIVE
# la desaisonnalisation doit etre multiplicative, la saison haute affecte les ventes avec un facteur multiplicatif et non en additionnant
# un volume constant
INdecomp<-decompose(INts) # l'objet INdecomp contient alors la decomposition de IN
INdecomp<-decompose(INts, type='multiplicative', filter=NULL)
plot(INdecomp)

# MATRICE DE REGRESSION
# creation d'une matrice dont les colonnes sont le vecteur IN_cjo decale de l'indice de colonne
# cette matrice prend en compte le coefficient de jours ouvres
createINd_cjo = function() {
  m = matrix(c(rep(c(rep(0,237)),12)),nrow=237,byrow=TRUE)
  for (j in 1:12) {
    for(i in (j+1):237)
      m[i,j]=mydata$IN_cjo[i-j]
  }
  INd_cjo = m[13:237,1:12] # on supprime les mois ou l'on a pas toutes les donnees a cause du decallage
  return(INd_cjo)
}

INdm = createINd_cjo() # creation de la matrice
IO1d =  mydata$IO1_cjo[13:237]# decallage du vecteur pour avoir la bonne taille pour la regression
IO1d_cut = mydata$IO1_cjo[36:237]#décallage superieur pour fitter la régression avec Ratio_IPC
# creation de la matrive vecteur colonne 1:1 pour obtenir le vecteur
M = matrix(1,12,1)
for (i in 1:12){
  M[i,1]=1
}
IN_moy = INdm%*%M




# Il faut créer la matrice de regression en ne retenant que la trend de IN puis faire la réression sur la trend








# test de la regression multilineaire de IO sur l'ensemble des IN : modele de base sans contraintes sur les coefficients
res1= lm(IO1d~INdm[1:225,1]+INdm[1:225,2]+INdm[1:225,3]+INdm[1:225,4]+INdm[1:225,5]+INdm[1:225,6]
         +INdm[1:225,7]+INdm[1:225,8]+INdm[1:225,9]+INdm[1:225,10]+INdm[1:225,11]+INdm[1:225,12])
summary(res1)
plot(res1)
coefficients(res1)
anova(res1)

# les resultats ne semblent pas tres adaptes (constante non nulle, R2 mauvais), cependant une analyse plus poussee
# du modele sans contraintes peut etre interessant, pour voir a quel point le fait de fixer des contraintes reduit la qualite de la regression.

# Test du meme modele mais avec une contrainte de nullite sur la constante 
res2= lm(IO1d~INdm[1:225,1]+INdm[1:225,2]+INdm[1:225,3]+INdm[1:225,4]+INdm[1:225,5]+INdm[1:225,6]
         +INdm[1:225,7]+INdm[1:225,8]+INdm[1:225,9]+INdm[1:225,10]+INdm[1:225,11]+INdm[1:225,12]-1)
summary(res2)
plot(res2)
coefficients(res2)
anova(res2)

# Les resultats sont bien meilleurs en terme de R2 mais il y a toujours des valeurs de coefficients negatifs, 
# cependant la contrainte qui impose la somme des coefficiants plus petite que un semble naturellement respectee.

# IPCVN et IPCVO designent les indices de Prix a la consommation de l'INSEE : Indice Prix a la consommation Voiture Neuve/Occasion
# Plus d'information concernant ces indices et leur construction dans le pdf de methodologie de l'INSEE joint au dossier Git
# Le coefficient CJO_5J pondere en fonction du nombre de jours ouvres

# Regression de : IO1_cjo[t]= sum(i=1:12)( alpha(i)*IN_cjo[t-i]  ) 
# avec apha(i,t)= beta0(i)+beta1(i)*PIB_evol(t)+beta2*Ratio_IPC(t)

# ce modele rend compte de l'arbitrage du consommateur entre le prix des voitures neuves et celles d'occasion
# et de l'influence du context economique (PIB)

res3= lm(IO1d~
           (1+PIB_evol[13:237]+Ratio_IPC[36:237])*(INdm[24:225,1]
            +INdm[24:225,2]
            +INdm[24:225,3]
            +INdm[24:225,4]
            +INdm[24:225,5]
            +INdm[24:225,6]
            +INdm[24:225,7]
            +INdm[24:225,8]
            +INdm[24:225,9]
            +INdm[24:225,10]
            +INdm[24:225,11]
            +INdm[24:225,12]-1))
summary(res3)
plot(res3)
coefficients(res3)
anova(res3)



# le probleme d'overfit et le fait que les élasticités des consommateurs sont constantes dans le temps nous amène à considérer un nouveau modèle


# nouveau modèle :
# Regression de : IO1_cjo[t]= ( alpha0 + alpha1*PIB_evol(t)+alpha2*Ratio_IPC(t)) * sum(i=1:12)(IN_cjo[t-i]) 

res4= lm(IO1d_cut~
           (PIB_evol[36:237]+Ratio_IPC[36:237])*(IN_moy[24:225]))
summary(res4)
plot(res4)
coefficients(res4)
anova(res4)

#erreur in PIB_evol et Ratio_IPC
