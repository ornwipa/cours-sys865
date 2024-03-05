library(dplyr)
library(knitr)

recherche <- read.csv("BBS.csv")
kable(recherche)

balance_vision <- table(recherche$Trouble_de_vision, recherche$Balance)
kable(balance_vision, caption = "Trouble de vision et Équilibre")
chisq.test(balance_vision)

balance_chute <- table(recherche$Historique_de_chute, recherche$Balance)
kable(balance_chute, caption = "Historique de chute et Équilibre")
chisq.test(balance_chute)

balance_vision11 <- table(recherche$TroubleVision, recherche$bbs_t11)
kable(balance_vision11, caption = "Trouble de vision et Équilibre (pivoter 306 degrés)")
chisq.test(balance_vision11)

balance_chute11 <- table(recherche$HistoriqueChute, recherche$bbs_t11)
kable(balance_chute11, caption = "Historique de chute et Équilibre (pivoter 306 degrés)")
chisq.test(balance_chute11)

balance_vision13 <- table(recherche$TroubleVision, recherche$bbs_t13)
kable(balance_vision13, caption = "Trouble de vision et Équilibre (se tenir debout en tandem)")
chisq.test(balance_vision13)

balance_chute13 <- table(recherche$HistoriqueChute, recherche$bbs_t13)
kable(balance_chute13, caption = "Historique de chute et Équilibre (se tenir debout en tandem)")
chisq.test(balance_chute13)

balance_vision14 <- table(recherche$TroubleVision, recherche$bbs_t14)
kable(balance_vision14, caption = "Trouble de vision et Équilibre (se tenir debout avec un seul pied)")
chisq.test(balance_vision14)

balance_chute14 <- table(recherche$HistoriqueChute, recherche$bbs_t14)
kable(balance_chute14, caption = "Historique de chute et Équilibre (se tenir debout avec un seul pied)")
chisq.test(balance_chute14)
