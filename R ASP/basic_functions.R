# Ce fichier contient les fonctions générales utilisées dans les differents modeles


# MATRICE DE REGRESSION
# creation d'une matrice dont les colonnes sont le vecteur entre en imput decale de l'indice de colonne

create_mat = function(var) {
  m = matrix(c(rep(c(rep(0,237)),12)),nrow=237,byrow=TRUE)
  for (j in 1:12) {
    for(i in (j+1):237)
      m[i,j]=var[i-j]
  }
  res = m[13:237,1:12] # on supprime les mois ou l'on a pas toutes les donnees à cause du decallage
  return(res)
}