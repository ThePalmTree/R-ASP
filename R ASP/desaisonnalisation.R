# Ce fichier se charge de la désaisonalisation des séries
source("import_files.r")

# CREATION DES OBJETS TIMESERIES
require(graphics)
INts<-ts(mydata$IN, start=c(1995, 1), end=c(2014, 9), frequency=12)
IO1 = mydata$IO1[13:237]
IO1ts<-ts(IO1, start=c(1996, 1), end=c(2014, 9), frequency=12)

# REPRESENTATION DECOMPOSITION
plot(stl(INts,"per"))


# DECOMPOSITION EFFECTIVE
# Méthode multiplicative et additive
IN_decomp_add<-decompose(INts, type="additive")
IN_decomp_mult<-decompose(INts, type="multiplicative")


plot(IN_decomp_add)
plot(IN_decomp_mult)

# Le choix de la meilleure désaisonalisation est encore en débat, pour l'heure les deux sont conservées
# Le plot des différentes décomposition ne permet pas de conclure facilement

IO1_decomp_add<-decompose(IO1ts, type="additive")
IO1_decomp_mult<-decompose(IO1ts, type="multiplicative")

plot(IO1_decomp_add)
plot(IO1_decomp_mult)

# Création des variables désaisonnalisées

INt = IN_decomp_mult$trend
INs = IN_decomp_mult$seasonal

IO1t = IO1_decomp_mult$trend
IO1s = IO1_decomp_mult$seasonal

# Une fois la desaisonnalisation faite, il faut multiplier par les CJO

INt_cjo = INt/mydata$X.CJO_5J[1:237]
INs_cjo = INs/mydata$X.CJO_5J[1:237]

IO1t_cjo = IO1t/mydata$X.CJO_5J[13:237]
IO1s_cjo = IO1s/mydata$X.CJO_5J[13:237]

