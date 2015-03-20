//---------------------//
// TD5 - SPECIFICATION //
//---------------------//


// EXERCICE 3 //-------------------------------------------------------------------------------------------------

use http://fmwww.bc.edu/ec-p/data/wooldridge/gpa2


* sat : SAT score (test entrée à l'université)
* tothrs : nombre d'heures durant le 1er semestre
* colgpa : grade point average : moyenne des notes du premier semestre
* athlete : dummy pour athlete
* verbmath : SAT score en math/anglais
* hsize : taille de la cohorte diplômée du bac, en centaines (dans le lycée d'origine)
* hsrank : rang dans la cohorte diplômée du bac (dans le lycée d'origine)

* hsperc : 100*(hsrank/hsize)
* white : dummy
* black : dummy
* hsizesq : hsize^2 

// Question (a)-------------------------------------
// Effet de la taille de la cohorte sur le SAT score : sat = b0 + b1 hsize + b2 hsize^2 + u

reg sat hsize hsizesq

/*    Source |       SS       df       MS              Number of obs =    4137
-------------+------------------------------           F(  2,  4134) =   15.93
       Model |  614822.099     2   307411.05           Prob > F      =  0.0000
    Residual |  79759024.2  4134  19293.4263           R-squared     =  0.0076
-------------+------------------------------           Adj R-squared =  0.0072
       Total |  80373846.3  4136  19432.7481           Root MSE      =   138.9

------------------------------------------------------------------------------
         sat |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       hsize |   19.81446   3.990666     4.97   0.000     11.99061    27.63831
     hsizesq |  -2.130606    .549004    -3.88   0.000    -3.206949   -1.054263
       _cons |   997.9805   6.203448   160.88   0.000     985.8184    1010.143
------------------------------------------------------------------------------
*/

// On utilise une expression quadratique pour modéliser le fait que la taille de la cohorte a un impact positif jusqu'à un certain point, puis négatif. 
// Intuition: les SAT scores sont moins bons dans les très petits et dans les très grands lycées par rapport aux lycées de taille moyenne. 
// Ce choix est validé par le fait que le coefficient du terme quadratique est très significatif (t-stat=-3.88)


// Question (b)-------------------------------------
// Taille de cohorte optimale = hsize*? 

// Il faut trouver le seuil au-delà duquel l'impact de la cohorte devient négatif --> hsize* est le point où la dérivée de sat en fonction de hsize s'annule
// d_sat/d_hsize = b1 + 2*b2*hsize = 0 --> hsize* = -b1/(2*b2) 

di -_b[hsize]/(2*_b[hsizesq])
* 4.6499585. Attention aux unités: en centaines d'élèves. La cohorte optimale compte 465 étudiants. 
* NB: R2 est très faible: la taille de la cohorte dans le lycée d'origine n'explique que très partiellement la variation dans les tests d'entrée. 

// Question (c)-------------------------------------
// Effet de la taille de cohorte sur le log du SAT score 

gen logsat=log(sat) 

reg logsat hsize hsizesq

/*    Source |       SS       df       MS              Number of obs =    4137
-------------+------------------------------           F(  2,  4134) =   16.19
       Model |  .614405204     2  .307202602           Prob > F      =  0.0000
    Residual |  78.4287724  4134  .018971643           R-squared     =  0.0078
-------------+------------------------------           Adj R-squared =  0.0073
       Total |  79.0431776  4136   .01911102           Root MSE      =  .13774

------------------------------------------------------------------------------
      logsat |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       hsize |   .0196029   .0039572     4.95   0.000     .0118445    .0273612
     hsizesq |  -.0020872   .0005444    -3.83   0.000    -.0031546   -.0010199
       _cons |   6.896029   .0061515  1121.03   0.000     6.883969    6.908089
------------------------------------------------------------------------------
*/

// Exactement le même calcul du seuil à partir duquel l'impact de hsize sur le score (cette fois, en pourcentage, mais cela ne change rien) devient négatif.
di -_b[hsize]/(2*_b[hsizesq])
* 4.695923: 469 étudiants, très proche du résultat trouvé en (b)

// Question (d)-------------------------------------
// Quel est le score prédit pour quelqu'un qui a été au lycée avec une cohorte de taille 200 ? Attention, il faut prédire y à partir d'un modèle où la variable expliquée est log(y)
// Rappel de la formule: y_chapeau = exp(b0+b1*x1+...+bk*xk) * exp(sigma_chapeau^2/2) 
// Pourquoi? MLR6: l'erreur suit une loi normale centrée, d'écart-type sigma; Fonction génératrice du moment d'ordre 1 = E(exp(u)) = exp(sigma^2/2)

gen logsat200 = _b[_cons] + _b[hsize]*2 + _b[hsizesq]*2^2
di exp(logsat200)*exp(e(rmse)^2/2) // la commande e(rmse) stocke l'estimation de l'écart-type des résidus (sigma chapeau); Root Mean-Squared Error = [SSR/(n-k-1)]^(1/2) = 0.1377 ici.
* 1029.03: le modèle prédit qu'un étudiant faisant partie d'une cohorte de 200 personnes aura un score d'entrée de 1029 points.
* A comparer à la distribution de SAT: 1029 points est très proche de la moyenne dans l'échantillon.
su sat
/*
    Variable |       Obs        Mean    Std. Dev.       Min        Max
-------------+--------------------------------------------------------
         sat |      4137    1030.331    139.4014        470       1540
*/


/* Question (e)-------------------------------------
Les deux seuls coefficients dont on peut anticiper le signe sont les proxys des capacités intellectuelles de l'étudiant: 
b3<0: l'étudiant fait partie du top x%, donc plus x est petit, plus le gpa devrait être élevé.
b4>0: plus le score aux tests d'entrée est élevé, plus la moyenne du premier trimestre devrait être élevée.
Il est difficile de se prononcer a priori sur le signe des autres coefficients */

// Question (f)-------------------------------------
// Estimation de : colgpa = b0 + b1 hsize + b2 hsize2 + b3 hsperc + b4 sat + b5 female + b6 athlete + u

reg colgpa hsize hsizesq hsperc sat female athlete 

/*    Source |       SS       df       MS              Number of obs =    4137
-------------+------------------------------           F(  6,  4130) =  284.59
       Model |  524.819305     6  87.4698842           Prob > F      =  0.0000
    Residual |  1269.37637  4130  .307355053           R-squared     =  0.2925
-------------+------------------------------           Adj R-squared =  0.2915
       Total |  1794.19567  4136  .433799728           Root MSE      =   .5544

------------------------------------------------------------------------------
      colgpa |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       hsize |  -.0568543   .0163513    -3.48   0.001    -.0889117   -.0247968
     hsizesq |   .0046754   .0022494     2.08   0.038     .0002654    .0090854
      hsperc |  -.0132126   .0005728   -23.07   0.000    -.0143355   -.0120896
         sat |   .0016464   .0000668    24.64   0.000     .0015154    .0017774
      female |   .1548814   .0180047     8.60   0.000     .1195826    .1901802
     athlete |   .1693064   .0423492     4.00   0.000     .0862791    .2523336
       _cons |   1.241365   .0794923    15.62   0.000     1.085517    1.397212
------------------------------------------------------------------------------
*/
* Le modèle prédit que, ttes choses égale par ailleurs, un athlète aura une moyenne au premier trimestre supérieure de 0.169 points comparé à un non-athlète.
* Magnitude? GPA est compris entre 0 et 4, avec une moyenne à 2.65. Le coefficient est statistiquement significatif à 1%. 

// Question (g)-------------------------------------
// On enlève 'sat' de l'estimation

reg colgpa hsize hsizesq hsperc female athlete 

/*    Source |       SS       df       MS              Number of obs =    4137
-------------+------------------------------           F(  5,  4131) =  191.92
       Model |  338.217123     5  67.6434246           Prob > F      =  0.0000
    Residual |  1455.97855  4131   .35245184           R-squared     =  0.1885
-------------+------------------------------           Adj R-squared =  0.1875
       Total |  1794.19567  4136  .433799728           Root MSE      =  .59368

------------------------------------------------------------------------------
      colgpa |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       hsize |  -.0534038   .0175092    -3.05   0.002    -.0877313   -.0190763
     hsizesq |   .0053228   .0024086     2.21   0.027     .0006007     .010045
      hsperc |  -.0171365   .0005892   -29.09   0.000    -.0182916   -.0159814
      female |   .0581231   .0188162     3.09   0.002     .0212333     .095013
     athlete |   .0054487   .0447871     0.12   0.903    -.0823582    .0932556
       _cons |   3.047698   .0329148    92.59   0.000     2.983167    3.112229
------------------------------------------------------------------------------
*/
* Lorsqu'on ne contrôle pas pour le score aux tests d'entrée, le modèle ne prédit pas de différence significative entre les athlètes et les non-athlètes. 
* Cela signifie qu'aux tests d'entrée, les athlètes sont globalement moins bons, mais qu'ils rattrapent leur retard pendant le premier trimestre (ou que le sport compte plus dans GPA que dans SAT...)

// Question (h)-------------------------------------
// Est-ce que l'impact d'être un athlète dépend du genre? Il faut créer des termes d'interaction entre le statut athlete et le genre (4 catégories au total)
// On veut tester la différence entre les femmes athlètes et les femmes non athlètes: on choisit donc comme catégorie de référence les femmes non athlètes, et on crée les 3 dummies restantes

gen maleathlete=(1-female)*athlete
gen malenonathlete=(1-female)*(1-athlete)
gen femaleathlete=female*athlete 

reg colgpa hsize hsizesq hsperc sat maleathlete malenonathlete femaleathlete

/*    Source |       SS       df       MS              Number of obs =    4137
-------------+------------------------------           F(  7,  4129) =  243.88
       Model |  524.821272     7  74.9744674           Prob > F      =  0.0000
    Residual |   1269.3744  4129  .307429015           R-squared     =  0.2925
-------------+------------------------------           Adj R-squared =  0.2913
       Total |  1794.19567  4136  .433799728           Root MSE      =  .55446

------------------------------------------------------------------------------
      colgpa |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       hsize |  -.0568006   .0163671    -3.47   0.001    -.0888889   -.0247124
     hsizesq |   .0046699   .0022507     2.07   0.038     .0002573    .0090825
      hsperc |  -.0132114    .000573   -23.06   0.000    -.0143349    -.012088
         sat |   .0016462   .0000669    24.62   0.000     .0015151    .0017773
 maleathlete |   .0128034   .0487395     0.26   0.793    -.0827523    .1083591
malenonath~e |  -.1546151   .0183122    -8.44   0.000    -.1905168   -.1187133
femaleathl~e |   .1751106   .0840258     2.08   0.037     .0103748    .3398464
       _cons |    1.39619   .0755581    18.48   0.000     1.248055    1.544324
------------------------------------------------------------------------------
*/
* Le modèle prédit que, ttes choses égale par ailleurs (en particulier le test SAT), une femme athlète aura une moyenne au premier trimestre supérieure de 0.175 point comparée à une femme non-athlète. 
* Le coefficient est statistiquement significatif à 4%. 


// Question (i)-------------------------------------
// Est-ce que l'effet de SAT sur GPA dépend du genre ? Il faut créer un terme d'interaction entre le genre et le score (catégorie de référence: les hommes)

gen satfemale = sat*female

reg colgpa hsize hsizesq hsperc sat female athlete satfemale

/*    Source |       SS       df       MS              Number of obs =    4137
-------------+------------------------------           F(  7,  4129) =  243.91
       Model |  524.867644     7   74.981092           Prob > F      =  0.0000
    Residual |  1269.32803  4129  .307417784           R-squared     =  0.2925
-------------+------------------------------           Adj R-squared =  0.2913
       Total |  1794.19567  4136  .433799728           Root MSE      =  .55445

------------------------------------------------------------------------------
      colgpa |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       hsize |  -.0569121   .0163537    -3.48   0.001    -.0889741   -.0248501
     hsizesq |   .0046864   .0022498     2.08   0.037     .0002757    .0090972
      hsperc |   -.013225   .0005737   -23.05   0.000    -.0143497   -.0121003
         sat |   .0016255   .0000852    19.09   0.000     .0014585    .0017924
      female |   .1023066   .1338023     0.76   0.445    -.1600179    .3646311
     athlete |   .1677568   .0425334     3.94   0.000     .0843684    .2511452
   satfemale |   .0000512   .0001291     0.40   0.692     -.000202    .0003044
       _cons |   1.263743   .0974952    12.96   0.000       1.0726    1.454887
------------------------------------------------------------------------------
*/
* Le coefficient du terme d'interaction n'est pas statistiquement différent de 0. L'impact de SAT sur GPA n'est donc pas différent entre hommes et femmes. 


