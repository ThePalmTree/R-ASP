# Modele trimestriel et projection sur le simpplexe
# Deux versions : multiplicatif et log lineaire

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)
library(MSBVAR)
library(prais)

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.r")
source("simplex.r")

###########################################################################################################
# Variables utilisées dans les modèles

#IN, IO1 et IO14 sont définis dans désaisonnalisation
N = matrix(1,3,1)
M = matrix(1,12,1)
INreg_trim = create_mat_trimestre(INreg_cjo)%*%N
INreg_year = create_mat(INreg_cjo)%*%M
print(INreg_year)
print(IO14reg_cjo)

###########################################################################################################
# Troncature de la période 2008-2009

# la période décembre 2008 - decembre 2010 représente les dates 169 à 193
IO1reg_cjo_2 = c(IO1reg_cjo[7:147],IO1reg_cjo[181:219]) 
INreg_trim_2 = c(INreg_trim[8:168],INreg_trim[202:230])

IO14reg_cjo_2 = c(IO14reg_cjo[7:61],IO14reg_cjo[105:147])
INreg_year_2 = c(INreg_year[45:168],INreg_year[216:218])
print(IO14reg_cjo_2)
# taille : 1:98
print(INreg_year_2)
# taille : 1:125

###########################################################################################################
# MODELE avec IO1 trimestriel
res20=lm ( IO1reg_cjo_2[1:209]~INreg_trim_2[1:209]+INreg_trim_2[4:212]+INreg_trim_2[7:215]+INreg_trim_2[10:218]-1)
summary(res20)  
plot(IO1reg_cjo_2)
par(new=T)
plot(fitted(res20))

# Sortie : 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# INreg_trim_2[1:209]  0.007705   0.008405   0.917   0.3605    
# INreg_trim_2[4:212]  0.017103   0.009055   1.889   0.0606 .  
# INreg_trim_2[7:215]  0.008707   0.009045   0.963   0.3371    
# INreg_trim_2[10:218] 0.053917   0.008449   6.382  1.5e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.398 on 176 degrees of freedom
# (29 observations deleted due to missingness)
# Multiple R-squared:  0.9857,  Adjusted R-squared:  0.9854 
# F-statistic:  3037 on 4 and 176 DF,  p-value: < 2.2e-16


# Test d'autocorrélation des résidus : Durbin & Watson Test
# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=200, k=4 (4 variables explicatives et 200 observations)
# 0 -> 1,643 : rejet de H0
# 1,73 -> 1,81 : incertitude
# 1,81 -> 2,19 : non rejet de H0
# 2,19 -> 2,27 : incertitude
# 2,27 -> 4 : rejet de H0

dwtest(IO1reg_cjo_2[1:209]~INreg_trim_2[1:209]+INreg_trim_2[4:212]+INreg_trim_2[7:215]+INreg_trim_2[10:218]-1)

# interprétation des résultats
#Durbin-Watson test
#data:  IO1reg_cjo_2[1:209] ~ INreg_trim_2[1:209] + INreg_trim_2[4:212] +     INreg_trim_2[7:215] + INreg_trim_2[10:218] - 1
#DW = 0.1969, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0


# Test de Prais Winsten
prais.winsten(IO1reg_cjo_2[1:209]~INreg_trim_2[1:209]+INreg_trim_2[4:212]+INreg_trim_2[7:215]+INreg_trim_2[10:218]-1, data=mydata)


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.6663 -1.5580 -0.0304  1.5096  8.7204 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# Intercept            34.893981  15.746603   2.216  0.02798 * 
#   INreg_trim_2[1:209]  -0.013444   0.009330  -1.441  0.15139   
# INreg_trim_2[4:212]  -0.002480   0.010200  -0.243  0.80822   
# INreg_trim_2[7:215]   0.004664   0.010240   0.455  0.64938   
# INreg_trim_2[10:218]  0.027908   0.009393   2.971  0.00338 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.802 on 175 degrees of freedom
# Multiple R-squared:  0.7269,  Adjusted R-squared:  0.7191 
# F-statistic: 93.17 on 5 and 175 DF,  p-value: < 2.2e-16



###########################################################################################################
# projection du modèle sur le volume engendré par le simplexe

P=c(coef(res20)[1],coef(res20)[2],coef(res20)[3],coef(res20)[4])
print(P)
P_simplex=proj(P)
print(coef(res20)[1])
# P appartient au simplexe


###########################################################################################################
# MODELE avec IO14 Annuel 
res21=lm ( IO14reg_cjo_2[1:98]~INreg_year_2[1:98]+INreg_year_2[13:110]+INreg_year_2[25:122]-1)
summary(res21)  
plot(IO14reg_cjo_2)
par(new=T)
plot(fitted(res21))

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-15.2967  -5.8281  -0.8082   4.7251  28.3505 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#INreg_year_2[1:98]    0.078613   0.009791   8.029 2.62e-12 ***
#  INreg_year_2[13:110] -0.031353   0.012838  -2.442   0.0164 *  
#  INreg_year_2[25:122]  0.015181   0.009807   1.548   0.1249    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 7.778 on 95 degrees of freedom
#Multiple R-squared:  0.9966,  Adjusted R-squared:  0.9965 
#F-statistic:  9301 on 3 and 95 DF,  p-value: < 2.2e-16


# Test d'autocorrélation des résidus : Durbin & Watson Test

# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=100, k=3 (3 variables explicatives et 100 observations)
# 0 -> 1,61 : rejet de H0
# 1,61 -> 1,74 : incertitude
# 1,74 -> 2,26 : non rejet de H0
# 2,26 -> 2,39 : incertitude
# 2,39 -> 4 : rejet de H0

dwtest(IO14reg_cjo_2[1:98]~INreg_year_2[1:98]+INreg_year_2[13:110]+INreg_year_2[25:122]-1)

#Durbin-Watson test
#data:  IO14reg_cjo_2[1:98] ~ INreg_year_2[1:98] + INreg_year_2[13:110] +     INreg_year_2[25:122] - 1
#DW = 0.9293, p-value = 1.119e-09
#alternative hypothesis: true autocorrelation is greater than 0

# Test de Prais Winsten
prais.winsten(IO14reg_cjo_2[1:98]~INreg_year_2[1:98]+INreg_year_2[13:110]+INreg_year_2[25:122]-1, data=mydata)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -16.994  -4.582  -0.207   3.438  34.272 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# Intercept            55.782170  45.934472   1.214 0.227642    
# INreg_year_2[1:98]    0.062549   0.017678   3.538 0.000628 ***
#   INreg_year_2[13:110] -0.029848   0.021320  -1.400 0.164811    
# INreg_year_2[25:122]  0.003046   0.019772   0.154 0.877902    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 6.568 on 94 degrees of freedom
# Multiple R-squared:  0.9896,  Adjusted R-squared:  0.9891 
# F-statistic:  2232 on 4 and 94 DF,  p-value: < 2.2e-16



###########################################################################################################
# projection du modèle sur le volume engendré par le simplexe

P=c(coef(res21)[1],coef(res21)[2],coef(res21)[3])
print(P)
P_simplex=proj(P)
print(coef(res21)[1])
# P appartient au simplexe