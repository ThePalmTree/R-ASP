//--------------------------------//
// TD4 - PROPRIETES ASYMPTOTIQUES //
//--------------------------------//


// EXERCICE 2 //-------------------------------------------------------------------------------------------------


// Question (a)-------------------------------------
// Question préliminaire pour se familiariser avec les commandes: création des variables x1 et u pour 1000 observations

set obs 1000 
* crée une table de 1000 lignes

gen x1=uniform()
* [0,1] par défaut - Stata crée une variable x1 et attribue à chacune des 1000 observations une valeur comprise entre 0 et 1 selon une loi uniforme
su x1

gen u=rchi2(1) - 1
* Stata crée une variable u et attribue à chacune des 1000 observations une valeur selon une loi du chi2 de paramètre 1, et lui enlève 1
su u
* Check: zero-mean
corr(x1 u)
* Check: zero-covariance

histogram x1

histogram u

histogram x1, normal
* trace l'histogramme de x1 et la densité d'une normale par desssus

histogram u, normal

// Question (b)-------------------------------------
// Simulation Monte-Carlo: objectif: tester les propriétés de la moyenne empirique, comme estimateur de la moyenne dans la population; hypothèse: beta0=beta1=beta2=0 (donc y = u)
* Premier essai: nb obs dans l'échantillon: 5; nb réplications: 10 000

* Etape 1: stata crée une variable y et attribue à 5 observations une valeur selon nos hypothèses (ici: y = u)

drop _all
set obs 5
gen y=rchi2(1)-1
sum y

* Etape 2: si l'on efface et répète l'opération, la moyenne des y sur les 5 observations change. 
drop _all
set obs 5
gen y=rchi2(1)-1
sum y

* Etape 3: répéter l'opération 10 000 fois et créer une variable ymean égale à la moyenne obtenue à chaque opération 
program define mysim, rclass  // option rclass permet d'extraire des statistiques sur les variables construites ex: r(mean)
drop _all
set obs 5
gen y=rchi2(1)-1
sum y
return scalar mean=r(mean)
end

simulate ymean=r(mean), reps(10000): mysim   // à chaque itération i, stata stocke la moyenne des y obtenue dans ymean_i --> à la fin, 10 000 observations pour ymean

browse

su ymean

// Question (c)-------------------------------------
// Même chose avec des échantillons de tailles différentes: 10, 100 et 1000 observations (nb réplications constant à 10 000) 

// de taille n=10

program define mysim10, rclass
drop _all
set obs 10
gen y=rchi2(1)-1
sum y
return scalar mean=r(mean)
end

simulate ymean=r(mean), reps(10000): mysim10
summarize ymean

/* 
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
       ymean |     10000    .0030032    .4448839  -.9010138   2.516836
*/
* Attention, le tableau ci-dessus n'est qu'un exemple: personne n'obtiendra 2 fois la même variable ymean: elle dépend des 10,000 échantillons qui ont été tirés, à chaque fois, aléatoirement.


// de taille n=100

program define mysim100, rclass
drop _all
set obs 100
gen y=rchi2(1)-1
sum y
return scalar mean=r(mean)
end

simulate ymean=r(mean), reps(10000): mysim100
summarize ymean

/*
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
       ymean |     10000    .0005834    .1410371  -.4814137   .6653621
*/


// de taille n=1000

program define mysim1000, rclass
drop _all
set obs 1000
gen y=rchi2(1)-1
sum y
return scalar mean=r(mean)
end

simulate ymean=r(mean), reps(10000): mysim1000
summarize ymean

/*
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
       ymean |     10000   -.0004671    .0448102  -.1706862   .1738213
*/

* La moyenne des moyennes empiriques E(ymean) est de plus en plus proche de 0 à mesure que N augmente, et son écart-type sd(ymean) de plus en plus petit 
* --> la distribution de ymean se resserre autour d'un point (loi des grands nombres)

// Question (d)-------------------------------------
// La vitesse de convergence de la moyenne empirique

di 0.4449*sqrt(10)/sqrt(100)
* on obtient l'écart-type obtenu pour n=100
di 0.4449*sqrt(10)/sqrt(1000)
* on obtient l'écart-type obtenu pour n=1000

/* La vitesse de convergence est proportionnelle à l'inverse de la racine carrée de la taille de l'échantillon
Si l'on prend comme point de référence n=10 et sd_10=0.4449, et que l'on augmente la taille de l'échantillon à n' observations,on aura sd'=sd_10*sqrt(10)/sqrt(n')
On cherche donc n'=(sd_10*sqrt(10)/0.001)^2 */
di (0.4449*sqrt(10)/0.001)^2
* environ 2 millions d'observations sont nécessaires (répliquées 10 000 fois!) pour obtenir un écart-type de 0.001.

// Question (e)-------------------------------------
// Histogramme

// pour n=10
capture simulate ymean=r(mean), reps(10000): mysim10
hist ymean, normal

// pour n=100
capture simulate ymean=r(mean), reps(10000): mysim100
hist ymean, normal

// pour n=1000
capture simulate ymean=r(mean), reps(10000): mysim1000
hist ymean, normal

* La distribution de l'estimateur ymean se rapproche de plus en plus d'une loi normale centrée réduite à mesure que N augmente (théorème central limite)



// Question (f)-------------------------------------
// Simulation Monte-Carlo: objectif: tester les estimateurs MCO des coefficients et de leurs écarts-types; hypothèse: beta0=1; beta1=2; beta2=10 (donc y =1+2*x1+10*x2+u)
// même nombre de réplications: 10 000

// pour n=10

program define mysim2, rclass
drop _all
set obs 10
gen x1=runiform()
gen x2=rbinomial(1, 0.3)
gen y=1 + 2*x1 + 10*x2 + (rchi2(1) - 1)
reg y x1 x2  // régression MCO
return scalar b0=_b[_cons]  // on stocke les statistiques des estimateurs qui nous intéressent: la valeur du coefficient et son écart-type
return scalar b1=_b[x1]
return scalar b2=_b[x2]
return scalar se0=_se[_cons]
return scalar se1=_se[x1]
return scalar se2=_se[x2]
end

capture simulate b0=r(b0) b1=r(b1) b2=r(b2) se0=r(se0) se1=r(se1) se2=r(se2), reps(10000) : mysim2

// pour n=1000

program define mysim21000, rclass
drop _all
set obs 1000
gen x1=runiform()
gen x2=rbinomial(1, 0.3)
gen y=1 + 2*x1 + 10*x2 + (rchi2(1) - 1)
reg y x1 x2
return scalar b0=_b[_cons]
return scalar b1=_b[x1]
return scalar b2=_b[x2]
return scalar se0=_se[_cons]
return scalar se1=_se[x1]
return scalar se2=_se[x2]
end


// Question (g)-------------------------------------
// Convergence des estimateurs MCO 

* Comparaison des moyennes et écarts-types des estimateurs des coeffcients quand n=10 et n=1000
capture simulate b0=r(b0) b1=r(b1) b2=r(b2), reps(10000) : mysim2
su b0 b1 b2 
/*    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
          b0 |     10000    1.001857    1.129081  -6.187531   12.62099
          b1 |     10000    1.998392    1.903847  -13.80215   23.95813
          b2 |     10000    9.706349    2.063246          0   23.41768
*/

capture simulate b0=r(b0) b1=r(b1) b2=r(b2), reps(10000) : mysim21000
su b0 b1 b2 
/*    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
          b0 |     10000    1.001936    .0936518   .5923231   1.417774
          b1 |     10000    1.998434    .1549956   1.384937   2.573035
          b2 |     10000    9.997804    .0974264    9.63007   10.50118
*/
* Valeurs estimées plus proches du "vrai" paramètre dans la population (écarts-types plus petits).

* Comparaison des distributions des estimateurs des coefficients
capture simulate b0=r(b0) b1=r(b1) b2=r(b2), reps(10000) : mysim2
hist b0, normal
hist b1, normal
hist b2, normal

capture simulate b0=r(b0) b1=r(b1) b2=r(b2), reps(10000) : mysim21000
hist b0, normal
hist b1, normal
hist b2, normal

* Les estimateurs des écarts-types sont-ils convergents? 
* Espérance: Moyenne des estimateurs des écarts-types = écarts-types des estimateurs des coefficients quand N est grand? OUI
* Variance: Ecarts-types (des estimateurs des écarts-types) diminuent quand N est grand? OUI
capture simulate se0=r(se0) se1=r(se1) se2=r(se2), reps(10000) : mysim2
su se0 se1 se2
/*    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
         se0 |     10000    .9392506    .5911678   .0331861   5.956776		// On compare 0.94 à 1.13
         se1 |     10000    1.600175    .9845822    .055496   10.06372		// On compare 1.60 à 1.90
         se2 |     10000    .9544125    .6113503          0   7.344592		// On compare 0.95 à 2.06
*/

capture simulate se0=r(se0) se1=r(se1) se2=r(se2), reps(10000) : mysim21000
su se0 se1 se2
/*    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
         se0 |     10000    .0940288    .0057854   .0744345   .1183672		// On compare 0.0940 à 0.0937
         se1 |     10000    .1548322    .0093604   .1231831   .1921677		// On compare 0.1548 à 0.1550
         se2 |     10000    .0975255    .0058475    .078394   .1206568		// On compare 0.0975 à 0.0974
*/

// Question (h)-------------------------------------
//  Simulation Monte-Carlo: objectif: tester la distribution de la F-stat générée par le test de H0: beta0 = 1 et beta1 = 2 ; même hypothèse: beta0=1; beta1=2; beta2=10 (donc y =1+2*x1+10*x2+u)
// même nombre de réplications: 10 000 

program define mysim3, rclass
drop _all
set obs 10
gen x1=runiform()
gen x2=rbinomial(1,0.3)
gen y=1 + 2*x1 + 10*x2 + (rchi2(1)-1)
reg y x1 x2 // régression MCO
test (_cons=1) (x1=2) // F-test beta0 = 1 et beta1 = 2: ce sont les vraies valeurs des paramètres donc on ne devrait pas rejeter H0.
return scalar F=r(F) // on stocke les valeurs des statistiques qui nous intéressent: la F-stat
end

capture simulate F=r(F), reps(10000) : mysim3

browse
* On obtient bien 10 000 valeurs différentes pour la F-stat (une à chaque réplication)

sum F

count if F>2.1
* 2573: On compte le nombre de réplications où la stat de test simulée est supérieure à 2.1 - Attention, ce chiffre sera différent à chaque simulation. 
* en proportion: 2573/10000=0.26

di Ftail(2, 7, 2.1)
* 0.19
/* Sous l'hypothèse de normalité des résidus, l'estimateur de la F-stat suit une loi de Fischer de paramètres q=2 (nombre de restrictions), n-k-1=7 (degrés de liberté)
On calcule la probabilité d'obsever 2.1 ou plus pour une variable suivant une telle loi: 0.19 à comparer à 0.26
Ici, les résidus ne suivent pas une loi normale, mais une loi du chi2 --> quand n est petit (=10), l'estimateur de la F-stat ne suit pas exactement une loi de Fischer.*/

count if F>6.1
* 799; en proportion: 0.08 --> en utilisant la distribution simulée des estimateurs de F (proche de la distribution réelle de F), on observe F>6.1 dans 8% des cas
* donc on ne rejette pas H0 au seuil de 5% si on observe F>6.1. 

di Ftail(2, 7, 6.1)
* 0.03: en faisant l'hypothèse (fausse) que les résidus sont gaussiens et que la F-stat suit une loi de Fischer, on rejetterait H0 à 5% si on observait F>6.1, ce qui serait une erreur étant donnés les paramètres dans la population.

// Question (i)-------------------------------------
// Idem en asymptotique : n=1000

program define mysim31000, rclass
drop _all
set obs 1000
gen x1=runiform()
gen x2=rbinomial(1,0.3)
gen y=1 + 2*x1 + 10*x2 + (rchi2(1)-1)
reg y x1 x2
test (_cons=1) (x1=2)
return scalar F=r(F)
end

capture simulate F=r(F), reps(10000) : mysim31000

count if F>2.1
* proportion: 0.124
di Ftail(2, 997, 2.1)
*0.123

count if F>6.1
*proportion: 0.003
di Ftail(2, 997, 6.1)
*0.002
* les p-values simulées (proches de la distribution réelle) se rapprochent des valeurs théoriques d'une loi de Fischer: les risques d'erreur sont moins élevées 
* ici, on ne rejette pas H0 si on observe 2.1 et on rejette si on observe 6.1, ce qui est cohérent avec les simulations. 

* Conclusion: l'approximation par une loi de Fischer n'est valable que (i) si l'on a de bons arguments pour justifier que les résidus sont gaussiens (rare...); ou (ii) si n est grand.
