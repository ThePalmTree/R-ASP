//-----------------//
// TD3 - INFERENCE //
//-----------------//


// EXERCICE 3 //-------------------------------------------------------------------------------------------------

use http://fmwww.bc.edu/ec-p/data/wooldridge/hprice1

browse

* price = housing prices
* assess = évaluation du prix du logement 
* bdrms = nombre de chambres
* lotsize = taille du lotissement en square feet (1 square foot = 0.09290304 m2)
* sqrft = taille de la maison en square feet 
* colonial = indicatrice = 1 si la maison est de style colonial


// Question (a)-------------------------------------
// Estimation de la régression de price sur assess

reg  price  assess

/*    Source |       SS       df       MS              Number of obs =      88
-------------+------------------------------           F(  1,    86) =  390.54
       Model |  752209.994     1  752209.994           Prob > F      =  0.0000
    Residual |  165644.511    86  1926.09897           R-squared     =  0.8195
-------------+------------------------------           Adj R-squared =  0.8174
       Total |  917854.506    87  10550.0518           Root MSE      =  43.887

------------------------------------------------------------------------------
       price |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      assess |   .9755538   .0493652    19.76   0.000     .8774191    1.073689   
       _cons |  -14.47179   16.27339    -0.89   0.376    -46.82221    17.87863   
------------------------------------------------------------------------------
*/

/* Test de H0 : beta0 = 0
On lit directement dans le tableau t0=-0.89
A quoi le comparer? Au quantile d'ordre 5% d'un student à N-K-1 (=88-2) degrés de liberté */
display invttail(86, 0.025)
* 1.9879342 >> abs(t0)
* On ne peut pas rejeter H0 à 5%. Plus rapide: 0 appartient à l'intervalle de confiance 95%, on ne peut pas exclure cette possibilité.

/* Test de H0 : beta1 = 1
On ne peut pas lire t1 directement dans le tableau, il faut le recalculer à la main */
gen t1=(_b[assess]-1)/_se[assess]
display t1
* -0.495 à comparer à 1.988 --> on ne peut pas rejeter H0
* Plus rapide: 1 appartient à l'intervalle de confiance 95%, on ne peut pas exclure cette possibilité.

* Autre option: commande "test"
test assess=1
/*
 ( 1)  assess = 1

       F(  1,    86) =    0.25
            Prob > F =    0.6217
*/

* Conclusion: les tests séparés ne nous permettent pas de rejeter l'hypothèse de rationalité de l'évaluation des prix de l'immobilier.

// Question (b)-------------------------------------
// Test de la rationalité de l'évaluation du prix de l'immobilier

* Test joint de H0 : beta0 = 0 et beta1 = 1 (à la main --> il faut construire la F-stat)

* Etape 1: Somme des carrés des résidus du modèle non contraint : price = beta0 + beta1 assess + v 
* On le lit dans le tableau de régression: SSRnc = 165 644.511
gen SSRnc = 165644.511

* Etape 2: Somme des carrés des résidus du modèle contraint, càd qu'on impose les valeurs des coefficients : price = 0 + 1*assess + u
* Dans le modèle contraint, le prix prédit est égal à assess, donc les résidus sont égaux à (price - assess)
egen SSRc=sum((price - assess)^2)
di SSRc
*209449

*Etape 3: Calcul de la stat de test avec nb de restrictions=2 et nb degrés de liberté=86 :
display (SSRc-SSRnc)/SSRnc * 86/2
*11.37

*Etape 4: Comparaison avec le quantile d'ordre 5% d'un chi2 de paramètres 2 et 86.
di invFtail(2,86,0.05)
* 3.10
*Idem 1%
di invFtail(2,86,0.01)
* 4.86

* Conclusion: On rejette H0 à 5 et 1%. Le test conjoint nous permet de conclure que l'évaluation des prix de l'immobilier n'est pas parfaitement rationnelle. 
* NB: il existe une commande directe sous Stata: test (var1=0) (var2=a) etc. 
test (_cons=0) (assess=1)
/* ----------------------------------
( 1)	_cons = 0
( 2)	assess = 1

	F(  2,    86)	=	11.37
	Prob > F	=	0.0000
---------------------------------- */

// Question (c)-------------------------------------
// Régression de price = beta0 + beta1 assess + beta2 sqrft + beta3 lotsize + beta4 bdrms + w

reg  price  assess  sqrft  lotsize  bdrms

/*    Source |       SS       df       MS              Number of obs =      88
-------------+------------------------------           F(  4,    83) =  100.74
       Model |  761089.801     4   190272.45           Prob > F      =  0.0000
    Residual |  156764.704    83  1888.73138           R-squared     =  0.8292
-------------+------------------------------           Adj R-squared =  0.8210
       Total |  917854.506    87  10550.0518           Root MSE      =   43.46

------------------------------------------------------------------------------
       price |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      assess |   .9082991   .1040386     8.73   0.000     .7013706    1.115228
       sqrft |  -.0005175   .0170849    -0.03   0.976    -.0344986    .0334636
     lotsize |   .0005867   .0004963     1.18   0.240    -.0004004    .0015738
       bdrms |   11.60249   6.549515     1.77   0.080    -1.424233    24.62921
       _cons |  -38.88702   21.49853    -1.81   0.074    -81.64673    3.872696
------------------------------------------------------------------------------
*/

* Test de H0 : beta2 = beta3 = beta4 =0

* Le modèle contraint est celui estimé en question a): R²= 0.8195; le modèle non contraint est celui estimé ci-dessus: R²= 0.8292
* On peut calculer directement la stat de test en utilisant les R². Attention, nb de restrictions=3 et nb degrés de liberté=83
display (0.8292-0.8195)/(1-0.8292) * (88-5)/3
* 1.57

* Ordre de grandeur pour la p-value: comparaison par rapport au quantile d'ordre 5%
di invFtail(3,83,0.05)
* 2.71 --> on ne peut pas rejeter H0 à 5% càd les variables sqrft, lotsize et bdrms ne sont pas conjointement significatives à 5%.

* Calcul exact de la p-value: plus petit seuil permettant de rejeter H0 càd tel que |Fisher d'ordre p|=|Fobservé|=1.57
di Ftail(3,83,1.57)
*0.203 --> on ne peut rejeter H0 qu'avec un seuil de 20,3% et plus. 

* Commande directe sous Stata:
test (sqrft=0) (lotsize=0) (bdrms=0)
/* --------------------------------------
 ( 1)  sqrft = 0
 ( 2)  lotsize = 0
 ( 3)  bdrms = 0

       F(  3,    83) =    1.57
            Prob > F =    0.2035
-------------------------------------- */


// Question (d)------------------------------------
/* Violation de l'hypothèse d'homoscedasticité: la stat de test F ne suit pas une loi de Fischer sous H0. 
La compararaison avec les quantiles usuels n'apporte donc aucune information pertinente. */

// Question (e)-------------------------------------
// Régression de lprice = beta0 + beta1 sqrft + beta2 bdrms + u

reg  lprice  sqrft  bdrms

/*    Source |       SS       df       MS              Number of obs =      88
-------------+------------------------------           F(  2,    85) =   60.73
       Model |  4.71671468     2  2.35835734           Prob > F      =  0.0000
    Residual |  3.30088884    85  .038833986           R-squared     =  0.5883
-------------+------------------------------           Adj R-squared =  0.5786
       Total |  8.01760352    87  .092156362           Root MSE      =  .19706

------------------------------------------------------------------------------
      lprice |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       sqrft |   .0003794   .0000432     8.78   0.000     .0002935    .0004654
       bdrms |   .0288844   .0296433     0.97   0.333    -.0300543    .0878232
       _cons |   4.766027   .0970445    49.11   0.000     4.573077    4.958978
------------------------------------------------------------------------------
*/

* Effet de l'ajout d'1 chambre de 150 square feet (environ 14 m2)
display _b[sqrft]*150 + _b[bdrms] 
* 0.0858 --> Prédiction: augmentation du prix de 8.6%; mais on ne connait pas l'intervalle de confiance.

// Question (f)-------------------------------------
// Ré-écriture de la régression pour estimer directement l'effet de l'ajout d'une chambre de 150 square feet

* On veut estimer theta = beta1*150 + beta2 (cf exercice 2c)
* On crée une nouvelle variable: new = x1 - 150*x2 et on estime y = beta0 + beta1*new + theta*x2 + v

gen new=sqrft-150*bdrms

reg  lprice  new  bdrms

/*    Source |       SS       df       MS              Number of obs =      88
-------------+------------------------------           F(  2,    85) =   60.73
       Model |  4.71671468     2  2.35835734           Prob > F      =  0.0000
    Residual |  3.30088884    85  .038833986           R-squared     =  0.5883
-------------+------------------------------           Adj R-squared =  0.5786
       Total |  8.01760352    87  .092156362           Root MSE      =  .19706

------------------------------------------------------------------------------
      lprice |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         new |   .0003794   .0000432     8.78   0.000     .0002935    .0004654
       bdrms |   .0858013  .0267675     3.21   0.002     .0325804    .1390223
       _cons |   4.766027   .0970445    49.11   0.000     4.573077    4.958978
------------------------------------------------------------------------------
*/

* Construction d'un intervalle de confiance pour theta
* On peut le lire directement dans le tableau: [3.2% - 13.9%] ou le retrouver à la main:
gen q95=invttail(85,0.025)
di _b[bdrms] - q95*_se[bdrms]
*0.0326
di _b[bdrms] + q95*_se[bdrms]
*0.1390
