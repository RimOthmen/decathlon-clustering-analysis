########################################################
# 1. Chargement des bibliothèques et des données
########################################################

# Charger les packages nécessaires
library(FactoMineR)    # Pour ACP, HCPC, catdes
library(factoextra)    # Pour visualisation des clusters
library(clValid)       # Pour validation interne des clusters
library(dendextend)    # Pour personnalisation du dendrogramme
library(ggplot2)       # Pour les graphiques

# Charger les données 'decathlon'
data("decathlon")

# Sélectionner uniquement les variables quantitatives actives
X = decathlon[, 1:10]

########################################################
# 2. Calcul de la matrice de distance et CAH
########################################################

# Normaliser les données et calculer la matrice de distances euclidienne
d = dist(scale(X), method = "euclidean")

# Appliquer l’algorithme CAH avec la méthode de Ward
hc1 = hclust(d, method = "ward.D")

# Afficher le dendrogramme
plot(hc1, hang = -1, main = "Dendrogramme - CAH")

########################################################
# 3. Déterminer le nombre optimal de clusters (méthode du coude)
########################################################

# Afficher les hauteurs (distances de fusion)
hc1$height

# Trier les hauteurs en ordre décroissant
sort(hc1$height, decreasing = TRUE)

# Représenter graphiquement la chute des hauteurs pour identifier le coude
plot(sort(hc1$height, decreasing = TRUE), type = "b",
     xlab = "Nombre de clusters", ylab = "Hauteurs",
     main = "Méthode du coude")

# Observation : chutes visibles pour k = 2, 4, 6

########################################################
# 4. Validation interne pour choix du nombre de clusters
########################################################

# Calculer les indices internes de validité pour k entre 2 et 6
intern = clValid(X, nClust = 2:6,
                 clMethods = c("hierarchical"),
                 validation = c("internal"))

# Résumé des résultats : silhouette, connectivité, Dunn
summary(intern)

# Interprétation :
# - Connectivité doit être MINIMISÉE
# - Silhouette doit être MAXIMISÉE
# - Dunn doit être MAXIMISÉ
# → k = 2 est optimal selon ces indices

########################################################
# 5. Découper l'arbre en k = 2 clusters et ajouter au dataset
########################################################

# Affecter les individus à chaque cluster
classes = cutree(hc1, k = 2)

# Vérifier la répartition des individus par cluster
table(classes)

# Ajouter la colonne 'classes' au jeu de données
decathlon_cah = cbind.data.frame(decathlon, classes = as.factor(classes))

# Vérifier la structure
str(decathlon_cah)

########################################################
# 6. Visualisation des clusters
########################################################

# Dendrogramme coloré par clusters
plot(color_branches(hc1, k = 2), main = "Dendrogramme coloré par clusters")

# Visualisation sur le plan factoriel (ACP)
fviz_cluster(list(data = X, cluster = classes),
             main = "Visualisation des clusters (CAH)")

########################################################
# 7. Description des classes (catdes)
########################################################

# Décrire les classes obtenues à partir des variables quantitatives
classedescr = catdes(decathlon_cah, num.var = 14)  # 14 = variable 'classes'
classedescr
# Interprétation :
# - v.test > 0 : moyenne dans la classe > moyenne globale
# - v.test < 0 : moyenne dans la classe < moyenne globale

########################################################
# 8. ACP + CAH (méthode HCPC)
########################################################

# Réaliser l'ACP (avec variables supplémentaires)
res.pca = PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)

# Appliquer HCPC (Hierarchical Clustering on Principal Components)
reshcpc = HCPC(res.pca, nb.clust = 2)

# Visualiser le dendrogramme et carte 3D
plot(reshcpc, choice = "tree")
plot(reshcpc, choice = "3D.map")

########################################################
# 9. Méthode des k-means (comparaison)
########################################################

# Appliquer k-means avec k = 2 (même nombre que CAH)
set.seed(123)
km = kmeans(scale(X), centers = 2, nstart = 25)

# Afficher les résultats du k-means
km

# Ajouter la colonne cluster k-means au dataset
decathlon_kmeans = cbind.data.frame(decathlon, cluster_kmeans = as.factor(km$cluster))

# Visualiser les clusters k-means
fviz_cluster(km, data = scale(X),
             main = "Clusters obtenus par k-means")

########################################################
# 10. Valider le choix du k avec les indices internes (optionnel)
########################################################

# Comparer silhouette moyenne pour différents k
fviz_nbclust(scale(X), kmeans, method = "silhouette")

########################################################
# 11. Interprétation finale
########################################################

# Comparer CAH vs k-means et confirmer la cohérence des partitions
