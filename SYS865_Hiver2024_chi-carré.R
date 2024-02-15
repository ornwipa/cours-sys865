library(dplyr)
library(knitr)

recherche <- read.csv("BBS.csv")
#kable(recherche)

balance_vision <- table(recherche$Trouble_de_vision, recherche$Balance)
kable(balance_vision, caption = "Trouble de vision et Équilibre")
chisq.test(balance_vision)

balance_chute <- table(recherche$Historique_de_chute, recherche$Balance)
kable(balance_chute, caption = "Historique de chute et Équilibre")
chisq.test(balance_chute)
