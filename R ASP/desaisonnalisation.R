# Ce fichier se charge de la désaisonalisation des séries
source("import_files.r")

# CREATION DES OBJETS TIMESERIES
require(graphics)

# REPRESENTATION DECOMPOSITION

IO1 = mydata$IO1[13:237]
IN = mydata$IN[1:237]
IO14=mydata$IO14[85:237]


# DECOMPOSITION EFFECTIVE

# Avant la desaisonnalisation faite, il faut multiplier par les CJO
IN_cjo = IN/mydata$X.CJO_5J[1:237]
# on commence à l'indice 1 car INt débute en 1995
IO1_cjo = IO1/mydata$X.CJO_5J[13:237]
# en revanche, ici on commence bien à 13
IO14_cjo = IO14/mydata$X.CJO_5J[85:237]

INts_cjo<-ts(IN_cjo, start=c(1995, 1), end=c(2014, 9), frequency=12)
plot(stl(INts_cjo,"per"))

# IO1 commence en janvier 1996 et termine en septembre 2014
IO1ts_cjo<-ts(IO1_cjo, start=c(1996, 1), end=c(2014, 9), frequency=12)
plot(stl(IO1ts_cjo,"per"))
# IO14 commence en janvier 2002 et se termiie en septembre 2014
IO14ts_cjo<-ts(IO14_cjo, start=c(2002, 1), end=c(2014, 9), frequency=12)
plot(stl(IO14ts_cjo,"per"))

# il est plus logique d'appliquer ce coefficient au début pour capter la saisonnalité due aux mois

# Méthode multiplicative et additive
IN_decomp_add<-decompose(INts_cjo, type="additive")
IN_decomp_mult<-decompose(INts_cjo, type="multiplicative")


plot(IN_decomp_add)
plot(IN_decomp_mult)
# les deux types de désaisonnalisation donnent la même trend, cependant la désaisonnalisation multiplicatiove
# s'interprète économiquement par un accroissement de l'activité économique (en % et non en volume)

# Le choix de la meilleure désaisonalisation est encore en débat, pour l'heure les deux sont conservées
# Le plot des différentes décomposition ne permet pas de conclure facilement

IO1_decomp_add<-decompose(IO1ts_cjo, type="additive")
IO1_decomp_mult<-decompose(IO1ts_cjo, type="multiplicative")
#IO1_decomp_mult commence en Juillet 1996 et termine en Mars 2014

IO14_decomp_add<-decompose(IO14ts_cjo, type="additive")
IO14_decomp_mult<-decompose(IO14ts_cjo, type="multiplicative")

plot(IO1_decomp_add)
plot(IO1_decomp_mult)

plot(IO14_decomp_add)
plot(IO14_decomp_mult)

# Création des variables désaisonnalisées

INt_cjo = IN_decomp_add$trend
INs_cjo = IN_decomp_add$seasonal
INr_cjo = IN_decomp_add$random
INreg_cjo = INt_cjo+INr_cjo

IO1t_cjo = IO1_decomp_add$trend
IO1s_cjo = IO1_decomp_add$seasonal
IO1r_cjo = IO1_decomp_add$random
IO1reg_cjo = IO1t_cjo + IO1r_cjo

IO14t_cjo = IO14_decomp_add$trend
IO14s_cjo = IO14_decomp_add$seasonal
IO14r_cjo = IO14_decomp_add$random
IO14reg_cjo = IO14t_cjo + IO14r_cjo
# INt va de juillet 1995 à mars 2014
# IO1 va de juillet 1996 à mars 2014
# IO14 va de juillet 2002 à mars 2014
