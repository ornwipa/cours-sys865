# Configurer les marges pour réduire l'espace autour du tracé
par(mar=c(2, 2, 2, 2)) 

# Configurer un tracé vide
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), xaxt='n', yaxt='n')

# Dessiner les noeuds
points(5, 8, pch=21, bg='lightblue', cex=3) # Noeud Insuline
text(5, 8, "Insuline")
points(2, 4, pch=21, bg='lightblue', cex=3) # Noeud Glucose
text(2, 4, "Glucose")
points(8, 4, pch=21, bg='lightblue', cex=3) # Noeud Diabète
text(8, 4, "Diabète")

# Dessiner les flèches
arrows(5, 7.5, 8, 4.5, length = 0.1) # Insuline à Diabète
arrows(2, 4.5, 5, 7.5, length = 0.1) # Glucose à Insuline
arrows(5, 7.5, 2, 4.5, length = 0.1) # Insuline à Glucose
arrows(2, 3.5, 8, 3.5, length = 0.1) # Glucose à Diabète
arrows(8, 3.5, 2, 3.5, length = 0.1) # Diabète à Glucose

# Ajouter du texte pour expliquer les flèches
text(6.5, 6, "Affecte")
text(3.5, 6, "Affecte")
text(5, 3, "Facteur Confondant Potentiel")
