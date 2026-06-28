# Modulo di Modellazione Statistica
cat("--- Step 3: MODELLAZIONE STATISTICA AVANZATA ---\n")

# Modello di Regressione Lineare Semplice
cat("\n[MODELLO DI REGRESSIONE LINEARE OLS]:\n")
mod_lineare <- lm(`Anomaly Scores` ~ `Packet Length`, data = df_real)
print(summary(mod_lineare))

# Grafico dei Residui Campionari
plot(mod_lineare$residuals, main="Analisi dei Residui Campionari", 
     ylab="Residui (E_i)", xlab="Indice Osservazione", col="dimgray")
abline(h=0, col="red", lwd=2)

# 3.2 Clustering K-Means
cat("\n[CLUSTERING K-MEANS E SELEZIONE METRICA]:\n")
df_cluster <- na.omit(df_real[, .(`Packet Length`, `Anomaly Scores`)])
df_scaled <- scale(df_cluster)

# Scree Plot (Metodo del Gomito)
set.seed(123)
wcss <- sapply(1:10, function(k) kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss)
plot(1:10, wcss, type="b", pch=19, col="darkblue",
     xlab="Numero di Cluster (k)", ylab="WCSS", main="Scree Plot per la Scelta di K")

# Esecuzione Ottimale
kmeans_res <- kmeans(df_scaled, centers = 3, nstart = 25, iter.max = 50)
df_cluster[, Cluster := as.factor(kmeans_res$cluster)]

# Metriche di Non Omogeneità Statistica
cat("Somma dei Quadrati Entro i Cluster (WCSS Totale):", sum(kmeans_res$withinss), "\n")
cat("Somma dei Quadrati Tra i Cluster (BCSS):", kmeans_res$betweenss, "\n\n")

# Scatterplot del Cluster
print(ggplot(df_cluster, aes(x = `Packet Length`, y = `Anomaly Scores`, color = Cluster)) +
        geom_point(alpha = 0.5) +
        labs(title = "Segmentazione dello Spazio Bivariato via K-Means",
             x = "Packet Length", y = "Anomaly Score") + theme_minimal())