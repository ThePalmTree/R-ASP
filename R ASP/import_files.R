# Ce fichier se charge d'importer les données utilisées à partir d'un fichier excel

# veuillez installer le package xlsx, et activer les packages rJava, xlsxjars et xlsx dans cet ordre pour pouvoir importer les fichiers xlsx
library(rJava,xlsxjars)
library(xlsx)

# importation du fichier xls
path = getwd()
mydata = read.xlsx("C:/Users/Loïc/Documents/GitHub/R-ASP/R ASP/donnees_mensuelles.xlsx",1)  # read from first sheet

