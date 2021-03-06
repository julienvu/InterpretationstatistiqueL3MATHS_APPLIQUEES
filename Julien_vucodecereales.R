# Julien VU L3 MINT MODELISATION

rm(list=objects())
graphics.off()

##############
# QUESTION 1 #
##############

# on charge le fichier ou on voit la hauteurs des c�r�ales : 
cereales <- read.table("cereales.txt", header = FALSE)
c <- cereales$V1        # on garde la colonne des observations

# on v�rifie qu'il contient 30 observations :
dim(cereales)
n <- length(c)
# c'est bien un tableau de taille 1x30, il y a 30 observations

# on stocke la moyenne, la variance et l'écart-type dans des variable :
moy <- mean(c)
var <- var(c)     # variance non biaisée
ect <- sd(c)      # �cart type sd(c)^2 = var(c)

# on affiche l'histogramme et on superpose la densité gaussienne:
hist(c, proba = TRUE, main = "")
curve(dnorm(x, moy, ect), add = TRUE, col = 2)
legend("topleft", "gaussienne", col = 2, lty = 1)
title(main = "Histogramme superpos� par la densit� gaussienne",
      sub = paste("moy = ", round(moy, 4), "; var = ", round(var, 4)))

# on affiche le graphe quantile/quantile gaussien :
plot(qnorm((1:n)/(n+1), moy, ect), sort(c), col = 2, pch = 8,   # = qqnorm((x - moy)/ect)
     main = "Quantiles gaussiens",                              # graphe quantile/quantile gaussien 
     xlab = "qexp(i/(n+1), 1/moy)", ylab = "x[i]")
abline(0, 1, col = 3, lwd = 3)
legend("bottomright", c("Vitesse", "y = x"), col = 2:3, 
       pch = c(8,NaN), lty = c(0,1), lwd = c(1, 3))

# Globalement, il y a une bonne adéquation / bonne représentation 
# de l'�chantillon par une gaussienne dont on a estimé moy et var. 
# En revanche, il y a quelques points mal repr�sent�s aux extr�mit�s
# et quelques uns au centre.

##############
# QUESTION 2 #
##############

# a) #
######

# Mod�le : Soient (c1, ..., c30) un n=30-�chantillon de variables al�atoires iid et de loi 
#          normale de param�tres (tau, sigma2)
# Hypoth�ses : on souhaite tester H0 : "tau = 0.9" contre H1 : "tau > 0.9"
# Variable de test : cbarre suit une N(tau, sigma2/n)
#                    d'ou� : (cbarre - tau)/(sigma/sqrt(n)) suit une N(0, 1)
#                    or sigma étant inconnue on l'estime et on obtient :
#                    T = (cbarre - tau)/(S/sqrt(n)) avec S = sd(c) et T suit une Student(n - 1)
# Zone de rejet : R = {T > c} on rejette H0 pour les grandes valeurs de T la statistique de test
#                 où c = quantile d'ordre 1 - alpha d'une loi de Student de ddl n-1

# b) #
######

tau0 <- 0.9           # la valeur test�e sous H0
tobs <- (moy - tau0)*sqrt(n)/ect
# la valeur de la statistique du test vaut : 0.4396
print(round(tobs, 4))

# c) #
######

pval <- 1 - pt(abs(tobs), n-1)
# on calcule la p-valeur et on remarque qu'elle est plus grande que alpha 

# EN PLUS :
alpha <- 0.05
t1 <- qt(1 - alpha, n-1)       # quantile d'ordre 0.95 d'une loi de Student de ddl n-1
abs(tobs) > t1  
# on teste si tobs en valeur absolue est dans la zone de rejet ou pas
# tobs n'est pas dans la zone de rejet, donc on conserve H0

# d) #
######

# On décide de conserver H0, donc le risque de conserver H0 � tort vaut : 
# P_{H1}(R^c) = beta = le risque de deuxième espèce qui est inconnu.

# e) #
######

t.test(c, mu = tau0, alternative = "greater") 

# La p-valeur = 0.3317 est plus grande que alpha = 0.05 donc on conserve H0.
# On retrouve la même p-valeur avec la fonction t.test et donc on conclut la même chose.
# On retrouve aussi la même valeur pour tobs qui est nommée t dans les sorties de t.test.

##############
# QUESTION 3 #
##############

# Selon la conclusion de notre test, on conserve H0 ce qui signifie que le producteur part 
# du principe que le taux d'OGM présent dans ses céréales est de 0.9%. Selon la réglémentation
# européenne, il n'a pas obligation d'étiqueter ses c�r�ales. Donc le producteur respecte la
# r�glementation.

# Cependant, le risque statistique de cette d�cision est le risque de seconde espèce qui est
# inconnu et non contr�l�e. Ce risque peut tr�s bien �tre tr�s �lev�e, auquel cas, le producteur
# s'exposerait � des probl�mes.

##############
# QUESTION 4 #
##############

# Pour r�pondre � cette question, il est pr�f�rable que le producteur refasse un test en modifiant
# les hypoth�ses et en testant : H0 : "tau = 0.8" contre H1 : "tau > 0.8"

tau0bis <- 0.8
t.test(c, mu = tau0bis, alternative = "greater") 

# Dans ce nouveau test, on remarque que la p-valeur vaut 0.00107 qui est bien plus petite que 
# alpha = 0.05.
# Donc la d�cision est diff�rente car on d�cide de rejeter H0. Auquel cas le producteur devra
# �tiquetter ses c�r�ales.

# Le risque de prendre cette d�cision est le risque de premi�re esp�ce qui est contr�l�e et qui
# vaut exactement alpha = 0.05. Du coup, cette fois-ci, le risque de se tromper est assez petit.
