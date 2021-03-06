
# veuillez installer le package xlsx, et activer les packages rJava, xlsxjars et xlsx dans cet ordre pour pouvoir importer les fichiers xlsx
library(rJava,xlsxjars)
library(xlsx)

# importation du fichier xls
path = getwd()
mydata = read.xlsx(paste(path,"/GitHub/R-ASP/R ASP/donnees_annuelles.xlsx",sep=""),1)  # read from first sheet

# regression lin�raire de IO1 sur IN
res = lm(IO1 ~ IN,data=mydata)
res



# test de la r�gression multilin�aire de IO sur l'ensemble des IN : mod�le de base sans contraintes sur les coefficiants
IO1=mydata$IO1[2:20]
INd=mydata$IN[1:19]  # on d�calle pour pouvoir faire les regressions

res1= lm(IO1~INd)
summary(res1)
plot(res1)
coefficients(res1)
anova(res1)

# les r�sultats ne semblent pas tr�s adapt�s (constante non nulle, R2 � chier), certains coefficiants sont plus grands que 1, cependant une analyse plus pouss�e
# du mod�le sans contraintes peut �tre interessant, pour voir � quel point le fait de fixer des contraintes r�duit la qualit� de la r�gression.

# il faut trouver un package qui poss�de une fonction comme lm() mais avec des contraintes sur les coefficiants

#mod�le avec PIB sans contraintes
INPIB=mydata$IN[1:19]*mydata$PIB[2:20] #m�me d�callage

res2= lm(IO1~INPIB)
summary(res2)
