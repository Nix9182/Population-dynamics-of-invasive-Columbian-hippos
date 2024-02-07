library(expm)

#Nos paramètres initiaux: à trouver

#nos facteurs démographiques:
s0 <- 0.88
s1 <- 0.94
s210 <- 0.97
s11 <- 0.972 #refaire avec la moyenne des valeurs



a <- 0.25 #pourcentage de femelles repro à 3 ans, arbitraire
b <- 0.5
g <- 1

f2 <- 0.009
f3 <- 0.06
f4 <- 0.25
f5 <- 0.44
f6 <- 0.491
f7 <- 0.499
f8 <- 0.5 #refaire avec la moyenne des valeurs

#matrice de leslie étendue
A <- matrix(c(0,0,s0*a*f2/2,s0*a*f3/2, s0*b*f4/2, s0*b*f5/2, s0*b*f6/2, s0*b*f7/2, s0*g*f8/2, s0*g*f8/2, s0*g*f8/2,
              s0,0,0,0,0,0,0,0,0,0,0,
              0,s1,0,0,0,0,0,0,0,0,0,
              0,0,s210,0,0,0,0,0,0,0,0,
              0,0,0,s210,0,0,0,0,0,0,0,
              0,0,0,0,s210,0,0,0,0,0,0,
              0,0,0,0,0,s210,0,0,0,0,0,
              0,0,0,0,0,0,s210,0,0,0,0,
              0,0,0,0,0,0,0,s210,0,0,0,
              0,0,0,0,0,0,0,0,s210,0,0,
              0,0,0,0,0,0,0,0,0,s210,s11),nr=11, byrow = TRUE)
eigen(A)

#on souhaite un lambda réel et non complexe:

lambda <- Re(eigen(A)$values[1])

#elasticité
sens_elas <- function(A) {
  lambda <- eigen(A)$values[1]   # valeur propre
  r <- eigen(A)$vectors  # vecteurs propres de droite
  l <- Conj(solve(r))    # conjugué complexe du vecteur propre de gauche
  
  #Calcul de la sensibilité
  s = l[1,] %*% t(r[,1])
  #Calcul de l'elasticité
  e = (A/lambda) * s
  #Sortie donnée sous forme de liste
  res = list(s,e)
  names(res)<- c("Sensitivity", "Elasticity")
  return(res)
}

sens_elas(A)$Elasticity
#Globalement, même sur premières générations, on voit qu'il vaut mieux 
#agir sur les paramètres de survie que de fécondité, car élasticité plus grande pour les facteurs 
#où que des par de survie, cohérent avec littérature


