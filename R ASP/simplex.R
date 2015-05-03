# Ce fichier contient les fonctions nécessaires à la projection d'un vecteur de Rn sur le simplexe de dimension n

# Cette fonction teste si le point P appartient au simplexe
is_within <- function(p)
{
  sum_p <- 0
  for (i in 1:length(p))
    {
      if (p[i] < 0) return(FALSE)
      sum_p = sum_p + p[i]
    }
  return(sum_p <= 1)
}


# Fonction de projection sur la face diagonale du simplexe
# Ref : Projection Onto A Simplex, Yunmei Chen and Xiaojing Ye, February 11, 2011
proj_diag<-function(p)
{
  n = length(p)
  p_sort = sort(p)
  i = n
  t = -1/0  # On fixe t égal à moins l'infini au départ
  eval<-function(j,x)
  {
    if(j<=0)
    {
      return(j==0)
    }
    else
    {
      return(x < p_sort[j])
    }
  }
  while (eval(i,t))
  {
    i = i - 1
    t = (sum(p_sort[(i+1):n])-1)/(n-i)
  }
  t_vect = c(rep(t,n))
  vect_zero = c(rep(0,n))
  return(pmax(p-t_vect,vect_zero))
}

# Algorithme général permettant de projeter sur tout le simplexe
# Ref : Edwin et nous, à détailler/justifier/démontrer dans le rapport
proj<-function(p)
{
  n = length(p)
  vect_zero = c(rep(0,n))
  p_pos = pmax(p,vect_zero)
  if (is_within(p_pos))
  {
    return(p_pos)
  }
  else
  {
    return(proj_diag(p_pos))
  }
}


# Test
p=c(1,1.5,1,0)

is_within(p)
proj(p)
