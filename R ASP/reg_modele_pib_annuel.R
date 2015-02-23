
# veuillez installer le package xlsx, et activer les packages rJava, xlsxjars et xlsx dans cet ordre pour pouvoir importer les fichiers xlsx
library(rJava,xlsxjars)
library(xlsx)

# importation du fichier xls
path = getwd()
mydata = read.xlsx(paste(path,"/donnees_annuelles.xlsx",sep=""),1)  # read from first sheet

# regression linéraire de IO1 sur IN
res = lm(IO1 ~ IN,data=mydata)
res



# test de la régression multilinéaire de IO sur l'ensemble des IN : modèle de base sans contraintes sur les coefficiants
IO1=mydata$IO1[2:20]
INd=mydata$IN[1:19]  # on décalle pour pouvoir faire les regressions

res1= lm(IO1~INd)
summary(res1)
plot(res1)
coefficients(res1)
anova(res1)

# les résultats ne semblent pas très adaptés (constante non nulle, R2 à chier), certains coefficiants sont plus grands que 1, cependant une analyse plus poussée
# du modèle sans contraintes peut être interessant, pour voir à quel point le fait de fixer des contraintes réduit la qualité de la régression.

# il faut trouver un package qui possède une fonction comme lm() mais avec des contraintes sur les coefficiants

#modèle avec PIB sans contraintes
INPIB=mydata$IN[1:19]*mydata$PIB[2:20] #même décallage

res2= lm(IO1~INPIB)
summary(res2)
