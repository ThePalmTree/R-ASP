//----------------------------//
// TD2 - MULTIPLE  REGRESSION //
//----------------------------//


// EXERCICE 5 //-------------------------------------------------------------------------------------------------


use "\\ulysse\users\PRossi\TD\Econométrie 1\Bases\emp2007.dta"

browse

* salred = salaire mensuel net
* hhc = nombre d'heures travaillées dans la semaine
* eduy = nombre d'années d'études depuis le CP

* OBJECTIF DE L'EXERCICE: déterminer une équation de salaire (horaire) à partir de l'éducation et de l'âge.

// Question (a)-------------------------------------
/* Corrélation (âge, éducation)
Le coefficient sur eduy change quand on contrôle pour l'âge (il augmente). Le coefficient de la régression "naive", sans contrôle, est donc biaisé, à la baisse (sous-estime les returns on education). 
--> le biais introduit par l'omission de l'âge est négatif: comme le coef. sur l'âge est positif, on en déduit que cov(age, eduy)<0
Interprétation: l'âge est négativement corrélé à l'éducation: augmentation constante du niveau moyen d'éducation avec le temps.*/

/* Calcul de la corrélation à partir des tables données: rappel corr(eduy, age) = cov(eduy, age)/[sd(eduy)*sd(age)] 
Rappel formule du cours: Biais = Beta2 * Delta1 = Beta2 * cov(x1, x2) / var(x1) donc cov(x1, x2) = Biais * var(x1) / Beta2 */
dis (0.0528-0.0629)*2.69^2/(0.0145*2.69*8.51)
* -0.22 
corr(eduy age)
/*(obs=31237)

             |     eduy      age
-------------+------------------
        eduy |   1.0000
         age |  -0.2205   1.0000
*/

// Question (b)-------------------------------------
// Régression du log du salaire horaire sur le nombre d'années d'études

* On calcule le log du salaire horaire
gen lnw=ln(12*salred/(52*hhc))

reg lnw eduy age

/*    Source |       SS       df       MS              Number of obs =   31237
-------------+------------------------------           F(  2, 31234) = 4299.80
       Model |  1085.56489     2  542.782447           Prob > F      =  0.0000
    Residual |  3942.80705 31234  .126234458           R-squared     =  0.2159
-------------+------------------------------           Adj R-squared =  0.2158
       Total |  5028.37194 31236  .160980021           Root MSE      =  .35529

------------------------------------------------------------------------------
         lnw |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        eduy |   .0629493   .0007651    82.27   0.000     .0614496    .0644489
         age |   .0145086   .0002423    59.87   0.000     .0140336    .0149836
       _cons |   .9515069   .0142606    66.72   0.000     .9235556    .9794582
------------------------------------------------------------------------------
*/
* Le modèle prédit qu'à niveau d'éducation constant, avoir un an de plus se traduit par une augmentation du salaire horaire de 1.5%. 

// Question (c)-------------------------------------
// Effet quadratique de l'âge: objectif: rendre compte d'une relation non-monotone ex: croissante dans la première phase de la vie professionnelle puis décroissante

gen age2=age^2

reg lnw eduy age age2

/*      Source |       SS       df       MS              Number of obs =   31237
-------------+------------------------------           F(  3, 31233) = 2965.09
       Model |  1114.64492     3  371.548308           Prob > F      =  0.0000
    Residual |  3913.72702 31233  .125307432           R-squared     =  0.2217
-------------+------------------------------           Adj R-squared =  0.2216
       Total |  5028.37194 31236  .160980021           Root MSE      =  .35399

------------------------------------------------------------------------------
         lnw |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        eduy |   .0618749   .0007656    80.82   0.000     .0603744    .0633754
         age |   .0473089   .0021666    21.84   0.000     .0430622    .0515555
        age2 |  -.0004588   .0000301   -15.23   0.000    -.0005178   -.0003997
       _cons |   .4115976   .0381834    10.78   0.000     .3367567    .4864385
------------------------------------------------------------------------------
*/

/* Effet marginal = dy/dx = dlnw/dage = Beta2 + 2*Beta3*age
--> calculer un effet marginal n'a de sens que pour un individu référent! Généralement, on construit un individu "moyen", qui a pour caractéristiques les valeurs moyennes de l'échantillon (ici, âge moyen: 36 ans)
--> Stata: commande _b[var1]: conserve en mémoire le coefficient beta correspondant à la variable 1 dans la dernière régression effectuée */

* Calcul de l'effet marginal pour une personne âgée de 20 ans
display _b[age] + 2*_b[age2]*20
*.02895878

* Pour une personne âgée de 50 ans
display _b[age] + 2*_b[age2]*50
*.00143365

* A quel âge l'effet devient négatif ? il faut calculer l'âge tel que dy/dx = Beta2 + 2*Beta3*age =0
display -_b[age]/(2*_b[age2])
* 51.562552 --> Au-delà de 52 ans, plus une personne est âgée, moins son salaire est élevé, à niveau d'éducation constant.  


// Question (d)-------------------------------------
// Méthode alternative ("partialling out"): construire une nouvelle variable qui ne capture que la part de l'éducation non expliquée par l'âge --> les résidus d'une régression OLS de edu sur age

reg eduy age age2

/*      Source |       SS       df       MS              Number of obs =   31237
-------------+------------------------------           F(  2, 31234) =  938.32
       Model |  12845.9019     2  6422.95093           Prob > F      =  0.0000
    Residual |  213800.707 31234  6.84512732           R-squared     =  0.0567
-------------+------------------------------           Adj R-squared =  0.0566
       Total |  226646.609 31236  7.25594214           Root MSE      =  2.6163

------------------------------------------------------------------------------
        eduy |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         age |   .1898534   .0159773    11.88   0.000     .1585372    .2211696
        age2 |  -.0036236   .0002216   -16.35   0.000     -.004058   -.0031892
       _cons |   10.22516   .2762183    37.02   0.000     9.683763    10.76656
------------------------------------------------------------------------------
*/

predict residedu, res
/* Stata: 	commande predict newvar crée une nouvelle variable égale aux valeurs de y prédites par la dernière regression
			commande predict newvar, res crée une nouvelle variable égale aux valeurs des résidus prédites par la dernière regression 
Nouvelle variable residedu ne capture que la part de l'éducation non expliquée par l'âge */

reg lnw residedu

/*      Source |       SS       df       MS              Number of obs =   31237
-------------+------------------------------           F(  1, 31235) = 6073.16
       Model |  818.536791     1  818.536791           Prob > F      =  0.0000
    Residual |  4209.83515 31235  .134779419           R-squared     =  0.1628
-------------+------------------------------           Adj R-squared =  0.1628
       Total |  5028.37194 31236  .160980021           Root MSE      =  .36712

------------------------------------------------------------------------------
         lnw |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
    residedu |   .0618749    .000794    77.93   0.000     .0603187    .0634311
       _cons |   2.240249   .0020772  1078.50   0.000     2.236178    2.244321
------------------------------------------------------------------------------

--> c'est le même coefficient beta1 qu'en question c) mais bien entendu pas la même constante. Les 2 méthodes sont équivalentes mais la première est plus facile à interpréter.
*/


// Question (e)-------------------------------------
// L'expérience potentielle: approchée par sa borne supérieure: le nombre d'années depuis que l'individu a quitté l'école (hypothèse (forte...): entrée directe dans la vie professionnelle, pas d'interruption de carrière)

gen pexp=age- eduy-6

reg  lnw eduy age pexp

/*      Source |       SS       df       MS              Number of obs =   31237
-------------+------------------------------           F(  2, 31234) = 4299.80
       Model |  1085.56489     2  542.782447           Prob > F      =  0.0000
    Residual |  3942.80705 31234  .126234458           R-squared     =  0.2159
-------------+------------------------------           Adj R-squared =  0.2158
       Total |  5028.37194 31236  .160980021           Root MSE      =  .35529

------------------------------------------------------------------------------
         lnw |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        eduy |   .0774579    .000852    90.91   0.000     .0757879    .0791278
         age |  (dropped)
        pexp |   .0145086   .0002423    59.87   0.000     .0140336    .0149836
       _cons |   1.038558   .0131857    78.76   0.000     1.012714    1.064403
------------------------------------------------------------------------------

--> Violation de l'hypothèse de non-colinéarité parfaite: pexp est une CL de age et eduy. Conséquence: Stata élimine une variable (ici l'âge). 
Interprétation mathématique: X'X est non-invertible donc on ne peut pas calculer Beta = (X'X)-1 X'Y
Interprétation intuitive: impossible de calculer l'effet d'une variable "toutes choses égales par ailleurs" car on ne peut pas garder l'éducation et l'expérience constantes, et faire varier l'âge.
*/
