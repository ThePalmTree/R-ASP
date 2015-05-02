# Modele avec IPC, Ratio_conso, Carbu
# Deux versions : multiplicatif et log lineaire

# Nettoyage de l'environement de travail
rm(list=ls())
setwd("~/GitHub/R-ASP/R ASP")
library(tseries)
library(MSBVAR)
library(car)
library(lmtest)
library(prais)
library(het.test)

#Importation des fichiers R nécéssaires
source("desaisonnalisation.r")
source("basic_functions.R")

###########################################################################################################
# Variables utilisées dans les modèles

#IN et IO1 sont définis dans désaisonnalisation
Ratio_IPC=mydata$Ratio_IPC # Ratio_IPC = IPC_VO / IPC_VN
croissance=mydata$Croissance
carbu=mydata$IPC_carbu
conso_VN=mydata$conso_VN
conso_IO1=c(rep(0,12),mydata$conso_VN[1:225])
ratio_conso=conso_IO1/conso_VN[1:237] 
M = matrix(1,12,1)
INt_moy = create_mat(INt_cjo)%*%M
INreg_moy = create_mat(INreg_cjo)%*%M

croissance_INreg_moy = croissance[35:231]*INreg_moy[23:219]
ratio_conso_INreg_moy = ratio_conso[35:231]*INreg_moy[23:219]
Ratio_IPC_INreg_moy = Ratio_IPC[35:231]*INreg_moy[23:219]
carbu_INreg_moy = carbu[35:231]*INreg_moy[23:219]

croissance_INt_moy = croissance[35:231]*INt_moy[23:219]
ratio_conso_INt_moy = ratio_conso[35:231]*INt_moy[23:219]
Ratio_IPC_INt_moy = Ratio_IPC[35:231]*INt_moy[23:219]
carbu_INt_moy = carbu[35:231]*INt_moy[23:219]

# Variables utilisées dans les tests
IN_moy = create_mat(INt_cjo)%*%M
INres_moy= IN_moy[7:219]-INreg_moy[7:219]
INreg_moy_2 = c(INreg_moy[23:169],INreg_moy[193:219])
print(IO1reg_cjo_2)
print(INreg_moy_2)

###########################################################################################################
# Troncature de la période 2008-2009

# la période décembre 2008 - decembre 2010 représente les dates 169 à 193
croissance_2=c(croissance[35:169],croissance[193:231])
INreg_moy_2=c(INreg_moy[23:157],INreg_moy[181:219])
IO1reg_cjo_2 = c(IO1reg_cjo[23:157],IO1reg_cjo[181:219])

ratio_conso_INreg_moy_2 = c(ratio_conso_INreg_moy[1:135],ratio_conso_INreg_moy[159:197])
Ratio_IPC_INreg_moy_2 = c(Ratio_IPC_INreg_moy[1:135],Ratio_IPC_INreg_moy[159:197])
carbu_INreg_moy_2 = c(carbu_INreg_moy[1:135],carbu_INreg_moy[159:197])
croissance_INreg_moy_2 = c(croissance_INreg_moy[1:135],croissance_INreg_moy[159:197])

IO14reg_cjo_3 = c(IO14reg_cjo[7:76],IO14reg_cjo[100:147])
print(IO14reg_cjo_3)
print(INreg_moy_3)
INreg_moy_3 = INreg_moy_2[57:174]
Ratio_IPC_INreg_moy_3 = Ratio_IPC_INreg_moy_2[57:174]
carbu_INreg_moy_3 = carbu_INreg_moy_2[57:174]
croissance_INreg_moy_3 = croissance_INreg_moy_2[57:174]
###########################################################################################################
# Critique de la pertinence du PIB (de la croissance)
res10= lm(log(IO1reg_cjo_2)~ log(1+croissance_2)+log(INreg_moy_2))
summary(res10)
plot(croissance_2)
plot(IO1reg_cjo_2)
par(new=T)
plot(fitted(res10))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            -3.8230     0.9903  -3.860  0.00016 ***
#  log(1 + croissance_2)  13.0224     1.7058   7.634 1.53e-12 ***
#  log(INreg_moy_2)        0.9835     0.1300   7.564 2.30e-12 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.1245 on 171 degrees of freedom
# Multiple R-squared:  0.4158,  Adjusted R-squared:  0.4089 
# F-statistic: 60.84 on 2 and 171 DF,  p-value: < 2.2e-16

# R2 0.4, pvalue ***, la croissance semble expliquer l'arbitrage entre l'achat de voiture neuve ou d'occasion

print(IO1reg_cjo_2)

# Test de l'hétérocédasticité des résidus : test de White
# H0 : var(résidu)=cste (homoscédasticité)
# H1 : var(résidu) non cste (hétéroscédasticité)

whites.htest(lm(log(IO1reg_cjo_2)~ log(1+croissance_2)+log(INreg_moy_2)))















# Test d'autocorrélation des résidus : Durbin & Watson Test
# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=200, k=2 (2 variables explicatives et 200 observations)
# 0 -> 1,653 : rejet de H0
# 1,653 -> 1,693 : incertitude
# 1,693 -> 2,307 : non rejet de H0
# 2,307 -> 2,347 : incertitude
# 2,347 -> 4 : rejet de H0

dwtest(log(IO1reg_cjo_2)~ log(1+croissance_2)+log(INreg_moy_2))

#data:  log(IO1reg_cjo_2) ~ log(1 + croissance_2) + log(INreg_moy_2)
#DW = 0.3865, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0
# rejet de H0, il y a autocorrelation des résidus

# H0 est rejeté, on estime les coefficients avec la méthode de Prais-Winsten



# Test prais winsten
prais.winsten(log(IO1reg_cjo_2)~ log(1+croissance_2)+log(INreg_moy_2), data=mydata)

#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.166216 -0.044708  0.002868  0.038266  0.166035 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#Intercept               0.4591     2.7730   0.166   0.8687  
#log(1 + croissance_2)   2.5416     1.4100   1.803   0.0732 .
#log(INreg_moy_2)        0.4318     0.3650   1.183   0.2384  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.06309 on 171 degrees of freedom
#Multiple R-squared:  0.9748,  Adjusted R-squared:  0.9743 
#F-statistic:  2201 on 3 and 171 DF,  p-value: < 2.2e-16

# les pvalues ne sont pas significatives, ce qui montre la limite de l'interprétation précédente














# Test de GRANGER
granger.test(cbind(IO14reg_cjo_3,croissance_INreg_moy_3),12)
# H0 Croissance do not granger_cause IO1:
# pvalue = 0.9625596
# -> on ne peut pas rejeter H0
# H0' IO1 do not granger_cause croissance:
# pvalue = 0.2901934
# -> on ne peut pas rejeter H0'
# les pvalues très faibles suggèrent qu'il n'existe pas de causalité (au sens de GRANGER) entre IO1 et la croissance

###########################################################################################################
# Modèle multiplicatif

#1)

# Test de cointégration
regression = lm(IO1reg_cjo[7:210] ~ INreg_cjo[7:210] -1)
summary(regression)
# R2 de 0,95 ! pvalue <2e-16
beta = coef(regression)[1]
adf.test(cbind(IO1reg_cjo[7:210]-beta*INreg_cjo[7:210]))
# LAG : 5 ; pvalue = 0.116
kpss.test(cbind(IO1reg_cjo[7:210]-beta*INreg_cjo[7:210]))
# LAG : 3 ; pvalue = 0.01
# ADF ne permet pas d'exclure la stationnarité de la série IO1-Beta*IN à 10% et donc d'exclure la cointégration
# KPSS confirme la non stationnarité avec une pvalue très petite (permet de rejeter H0 : la non cointégration)
# On en déduit qu'il n'y a pas de cointégration entre IO1 et IN, le R2 mesuré est donc significatif

# Remarque 2 : le fait que le R2 soit si bon constitue déjà une bonne preuve en soi de la cointégration
# Remarque 3 : IN et IO1 devraient logiquement être fortement cointégrés


regression = lm(IO1reg_cjo_2 ~ INreg_moy_2 -1)
summary(regression)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-16.6268  -3.9337   0.7427   4.3478  14.7586 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# INreg_moy_2 0.0216056  0.0002325   92.91   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 6.286 on 173 degrees of freedom
#Multiple R-squared:  0.9804,  Adjusted R-squared:  0.9802 
#F-statistic:  8632 on 1 and 173 DF,  p-value: < 2.2e-16


# Test d'autocorrélation des résidus : Durbin & Watson Test
# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=200, k=1 (1 variable explicative et 200 observations)
# 0 -> 1,664 : rejet de H0
# 1,664 -> 1,684 : incertitude
# 1,684 -> 2,316 : non rejet de H0
# 2,316 -> 2,336: incertitude
# 2,336 -> 4 : rejet de H0

dwtest(IO1reg_cjo_2 ~ INreg_moy_2 -1)
#Durbin-Watson test
#data:  IO1reg_cjo_2 ~ INreg_moy_2 - 1
#DW = 0.2258, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0

# H0 est rejeté, on estime les coefficients avec la méthode de Prais-Winsten

prais.winsten(IO1reg_cjo_2 ~ INreg_moy_2 -1, data=mydata)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.0490 -1.8767  0.0177  1.7012  8.6963 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#Intercept   20.792039  15.955644   1.303    0.194
#INreg_moy_2  0.011347   0.007852   1.445    0.150
#Residual standard error: 2.9 on 172 degrees of freedom
#Multiple R-squared:  0.7241,  Adjusted R-squared:  0.7209 
#F-statistic: 225.7 on 2 and 172 DF,  p-value: < 2.2e-16
#[[2]]
#Rho Rho.t.statistic Iterations
#0.8982654        26.56041          8

# coeffs non significatifs



#3)

res11= lm(IO1reg_cjo_2 ~ ratio_conso_INreg_moy_2 + Ratio_IPC_INreg_moy_2 + carbu_INreg_moy_2)
summary(res11)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-11.8322  -3.0122  -0.7007   3.0205  12.7112 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              7.982e+00  4.486e+00   1.779  0.07699 .  
#ratio_conso_INreg_moy_2  1.960e-02  6.968e-03   2.813  0.00548 ** 
#  Ratio_IPC_INreg_moy_2    8.746e-03  4.677e-03   1.870  0.06322 .  
#carbu_INreg_moy_2       -7.909e-05  7.535e-06 -10.497  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3.478 on 170 degrees of freedom
#Multiple R-squared:  0.768,  Adjusted R-squared:  0.7639 
#F-statistic: 187.6 on 3 and 170 DF,  p-value: < 2.2e-16


plot(IO1reg_cjo_2)
par(new=T)
plot(fitted(res11))

# Test de l'homoscédasticité des résidus MLR 5
white.htest()













# Test d'autocorrélation des résidus : Durbin & Watson Test
# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=200, k=3 (1 variable explicative et 200 observations)
# 0 -> 1,643 : rejet de H0
# 1,643 -> 1,704 : incertitude
# 1,704 -> 2,296 : non rejet de H0
# 2,296 -> 2,357 : incertitude
# 2,357 -> 4 : rejet de H0

dwtest(IO1reg_cjo_2 ~ ratio_conso_INreg_moy_2 + Ratio_IPC_INreg_moy_2 + carbu_INreg_moy_2)

#Durbin-Watson test
#data:  IO1reg_cjo_2 ~ ratio_conso_INreg_moy_2 + Ratio_IPC_INreg_moy_2 +     carbu_INreg_moy_2
#DW = 0.857, p-value = 2.863e-15
#alternative hypothesis: true autocorrelation is greater than 0

# H0 est rejeté, on estime les coefficients avec la méthode de Prais-Winsten

prais.winsten(IO1reg_cjo_2 ~ ratio_conso_INreg_moy_2 + Ratio_IPC_INreg_moy_2 + carbu_INreg_moy_2, data=mydata)


#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8.4342 -1.5354 -0.1461  1.3201  9.3650 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#Intercept                1.245e+01  7.751e+00   1.606   0.1101    
#ratio_conso_INreg_moy_2  1.042e-02  1.031e-02   1.010   0.3138    
#Ratio_IPC_INreg_moy_2    1.347e-02  6.962e-03   1.934   0.0547 .  
#carbu_INreg_moy_2       -6.242e-05  1.179e-05  -5.294 3.66e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 2.839 on 170 degrees of freedom
#Multiple R-squared:  0.9759,  Adjusted R-squared:  0.9754 
#F-statistic:  1723 on 4 and 170 DF,  p-value: < 2.2e-16
#[[2]]
#Rho Rho.t.statistic Iterations
#0.6014297        9.858966         11



# Analyse critique des résultats
# R2 ajusté : 0,61, toutes les p-values à ***
# Ratio IPC * IN_moy a une pvalue particulièrement petite : capte l'arbitrage des prix
# Ratio conso * IN_moy a un coeff négatif : effet inverse de ratio IPC : favorise IN en cas de faible consommation des voitures neuves

# Remarque 0 : cela est rendu possible en supprimant la période décembre 2008 - décembre 2010 (prime à la casse)
# pour rappel, sans supprimer cette période, on a un R2 de 0,97 et une cointégration à 1% donnée par ADF

# Remarque 1: si on ajoute croissance * IN_moy au modèle, alors :
# cette var a une pvalue faible mais la plus grande de toutes et le R2 ajusté devient 0,70
# d'autre part, ajouter la croissance modifie le coefficient de ratio conso * IN
# ce phénomène n'a aucun fondement économique et résulte uniquement de cointégration entre la croissance et le ratio conso
# en effet, ces deux séries sont monotones

# Mesure de la causalité : GRANGER test
granger.test(cbind(IO1reg_cjo_2,INreg_moy_2),12)
# H0 : IN do not granger_cause IO
# pvalue 0,06 -> rejet de H0 à 6 %
# IN influe IO
# H0' : IO do not granger_cause IN
# pvalue 0,03 -> rejet également de H0' à 5%
# IO influence également IN dans une certaine mesure
# économiquement, il est tout à fait raisonnable que le marché d'occasion influence rétroactivment celui du neuf
# le lag considéré étant de 12 mois (dans les deux sens?)
# les résultats de ce test sont excellents
granger.test(cbind(IO1reg_cjo_2, ratio_conso_INreg_moy_2, Ratio_IPC_INreg_moy_2, croissance_INreg_moy_2),12)


#                                                 F-statistic    p-value
#ratio_conso_INreg_moy_2 -> IO1reg_cjo_2             1.3005901 0.22489517 
#Ratio_IPC_INreg_moy_2 -> IO1reg_cjo_2               1.6630013 0.08161912 ratio IPC explique IO1
#croissance_INreg_moy_2 -> IO1reg_cjo_2              0.3974800 0.96255959 croissance n'explique pas IO1
#IO1reg_cjo_2 -> ratio_conso_INreg_moy_2             1.5223031 0.12309815 IO1 explique légèrement ratio conso
#Ratio_IPC_INreg_moy_2 -> ratio_conso_INreg_moy_2    1.1810155 0.30261571
#croissance_INreg_moy_2 -> ratio_conso_INreg_moy_2   0.3397650 0.98031710 (°)
#IO1reg_cjo_2 -> Ratio_IPC_INreg_moy_2               1.8748480 0.04256288 IO1 explique ratio IPC
#ratio_conso_INreg_moy_2 -> Ratio_IPC_INreg_moy_2    1.0746697 0.38617142 (°)
#croissance_INreg_moy_2 -> Ratio_IPC_INreg_moy_2     0.7781614 0.67203210 (°)
#IO1reg_cjo_2 -> croissance_INreg_moy_2              1.1984712 0.29019342
#ratio_conso_INreg_moy_2 -> croissance_INreg_moy_2   0.9599509 0.49010334 (°)
#Ratio_IPC_INreg_moy_2 -> croissance_INreg_moy_2     0.4825049 0.92222231 (°)

#(°) : les variables explicatives ne s'interexpliquent pas les unes les autres

###########################################################################################################





# après chaque régression, tester l'autocorrélation des résidus
# test de durbin watson
# si ce test permet de rejeter l'hypotèse H0: non autocorrélation
# alors on fait une estimation par price winston et non par MCO




###########################################################################################################
###########################################################################################################
###########################################################################################################

# IO14

# Test de cointégration
regression = lm(IO14reg_cjo[7:145] ~ INreg_cjo[93:231] -1)
summary(regression)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#INreg_cjo[93:233] 0.764911   0.007565   101.1   <2e-16 ***
#Residual standard error: 15.26 on 138 degrees of freedom
#(2 observations deleted due to missingness)
#Multiple R-squared:  0.9867,  Adjusted R-squared:  0.9866 
#F-statistic: 1.022e+04 on 1 and 138 DF,  p-value: < 2.2e-16

beta = coef(regression)[1]
adf.test(cbind(IO14reg_cjo[7:145]-beta*INreg_cjo[93:231]))
# Dickey-Fuller = -2.2922, Lag order = 5, p-value = 0.4548
kpss.test(cbind(IO1reg_cjo[7:145]-beta*INreg_cjo[93:231]))
# LAG : 2 ; pvalue = 0.01

regression = lm(IO14reg_cjo_3 ~ INreg_moy_3 -1)
summary(regression)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-29.779  -5.996   1.326   7.035  37.301 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#INreg_moy_3  0.06445    0.00045   143.2   <2e-16 ***
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 9.956 on 117 degrees of freedom
#Multiple R-squared:  0.9943,  Adjusted R-squared:  0.9943 
#F-statistic: 2.051e+04 on 1 and 117 DF,  p-value: < 2.2e-16



# Test d'autocorrélation des résidus : Durbin & Watson Test
# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=200, k=1 (1 variable explicative et 100 observations)
# 0 -> 1,65 : rejet de H0
# 1,65 -> 1,69 : incertitude
# 1,69 -> 2,31 : non rejet de H0
# 2,31 -> 2,35: incertitude
# 2,35 -> 4 : rejet de H0

dwtest(IO14reg_cjo_3 ~ INreg_moy_3 -1)
#Durbin-Watson test
#data:  IO14reg_cjo_3 ~ INreg_moy_3 - 1
#DW = 0.5944, p-value = 6.832e-15
#alternative hypothesis: true autocorrelation is greater than 0

prais.winsten(IO14reg_cjo_3 ~ INreg_moy_3 -1, data=mydata)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-21.403  -3.928  -1.068   2.829  37.328 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#Intercept   86.96047   24.15326   3.600 0.000469 ***
#  INreg_moy_3  0.02177    0.01186   1.835 0.069051 .  
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 6.809 on 116 degrees of freedom
#Multiple R-squared:  0.9834,  Adjusted R-squared:  0.9831 
#F-statistic:  3428 on 2 and 116 DF,  p-value: < 2.2e-16
#Rho Rho.t.statistic Iterations
#0.6095911         8.19367          5




#3)

res14= lm(IO14reg_cjo_3 ~ Ratio_IPC_INreg_moy_3 + carbu_INreg_moy_3)
summary(res11)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-22.9618  -3.6317  -0.0402   4.0310  29.4304 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            1.087e+02  9.606e+00  11.311  < 2e-16 ***
#  Ratio_IPC_INreg_moy_3  1.912e-02  3.786e-03   5.050 1.68e-06 ***
#  carbu_INreg_moy_3     -5.214e-05  1.226e-05  -4.252 4.34e-05 ***
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 7.362 on 115 degrees of freedom
#Multiple R-squared:  0.3372,  Adjusted R-squared:  0.3256 
#F-statistic: 29.25 on 2 and 115 DF,  p-value: 5.378e-11

plot(IO14reg_cjo_3)
par(new=T)
plot(fitted(res11))

# Test de l'homoscédasticité des résidus MLR 5
white.htest()













# Test d'autocorrélation des résidus : Durbin & Watson Test
# H0 : phi=0 absence de corrélation entre les résidus
# H1 : phi non nul : corrélation entre les résidus
# la lecture de la table de durbin et watson donne les zones de rejet
# n=200, k=3 (1 variable explicative et 200 observations)
# 0 -> 1,643 : rejet de H0
# 1,643 -> 1,704 : incertitude
# 1,704 -> 2,296 : non rejet de H0
# 2,296 -> 2,357 : incertitude
# 2,357 -> 4 : rejet de H0

dwtest(IO14reg_cjo_3 ~ Ratio_IPC_INreg_moy_3 + carbu_INreg_moy_3)
#Durbin-Watson test
#data:  IO14reg_cjo_3 ~ Ratio_IPC_INreg_moy_3 + carbu_INreg_moy_3
#DW = 1.0449, p-value = 2.957e-08
#alternative hypothesis: true autocorrelation is greater than 0
 
prais.winsten(IO14reg_cjo_3 ~ Ratio_IPC_INreg_moy_3 + carbu_INreg_moy_3, data=mydata)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#Intercept              1.054e+02  1.512e+01   6.971 2.11e-10 ***
#  Ratio_IPC_INreg_moy_3  2.029e-02  6.029e-03   3.365  0.00104 ** 
#  carbu_INreg_moy_3     -4.980e-05  1.938e-05  -2.570  0.01144 *  
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 6.48 on 115 degrees of freedom
#Multiple R-squared:  0.9915,  Adjusted R-squared:  0.9913 
#F-statistic:  4487 on 3 and 115 DF,  p-value: < 2.2e-16
#Rho Rho.t.statistic Iterations
#0.4771451        5.820566   
# des pvalues significatives même en ajustant l'autocorrélation des résidus

# Mesure de la causalité : GRANGER test
granger.test(cbind(IO14reg_cjo_3,INreg_moy_3),12)
#F-statistic   p-value
#INreg_moy_3 -> IO14reg_cjo_3   0.4213842 0.9508822
#IO14reg_cjo_3 -> INreg_moy_3   1.1325880 0.3462710
# pas de réelle causalité

granger.test(cbind(IO14reg_cjo_3, Ratio_IPC_INreg_moy_3, croissance_INreg_moy_3),12)
#F-statistic    p-value
#Ratio_IPC_INreg_moy_3 -> IO14reg_cjo_3            0.8272084 0.62224348
#croissance_INreg_moy_3 -> IO14reg_cjo_3           0.9542019 0.49876909
#IO14reg_cjo_3 -> Ratio_IPC_INreg_moy_3            1.7267153 0.07604835
#croissance_INreg_moy_3 -> Ratio_IPC_INreg_moy_3   0.8732434 0.57669536
#IO14reg_cjo_3 -> croissance_INreg_moy_3           1.5107269 0.13729368
#Ratio_IPC_INreg_moy_3 -> croissance_INreg_moy_3   0.6403613 0.80170135
# Pas de réelle causalité

# il faut prendre en compte le fait que bcp d'observations sont supprimées (plus de la moitié)
# en effet, IO14 commence en 2002, on supprime 2009 et 2010 à cause de la prime à la casse, 
# on perd encore deux fois 6 mois avec la désaisonnalisation
# et encore 1 an avec les valeurs de INreg_moy qui somment entres autre des valeurs de IN de 2009-2010...
# au total il ne reste plus que 180 valeurs sur 337...










