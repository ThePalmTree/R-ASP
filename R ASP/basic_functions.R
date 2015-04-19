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

# la première ligne de m contient les 12 valeurs de IN pour l'année 1995
# la dernière ligne de m contient les 12 valeurs de IN de sept 2013 à sept 2014
# en cas d'utilisation de séries désaisonnalisées, il faut supprimer les 6 premières 
# et les 6 dernières lignes en raison des valeurs manquantes résultant de la désaisonnalisation