cat("--- Step 3: MODELLAZIONE STATISTICA AVANZATA ---\n")

# --- 3.1 MODELLO DI REGRESSIONE LINEARE SEMPLICE ---
cat("\n[MODELLO DI REGRESSIONE LINEARE OLS]:\n")
# Analizziamo la dipendenza lineare dei Byte Ricevuti (Y) in funzione dei Byte Inviati (X)
mod_lineare <- lm(rcvdbyte ~ sentbyte, data = df_real)
print(summary(mod_lineare))

# Grafico dei Residui Campionari Sperimentali
plot(mod_lineare$residuals, main="Analisi dei Residui Campionari", 
     ylab="Residui (E_i)", xlab="Indice Osservazione", col="dimgray")
abline(h=0, col="red", lwd=2)


# --- 3.2 CLUSTERING NON SUPERVISIONATO K-MEANS ---
cat("\n[CLUSTERING K-MEANS E SELEZIONE METRICA]:\n")

# Isoliamo e scaliamo lo spazio bivariato basato sui volumi scambiati (sentbyte e rcvdbyte)
df_cluster <- na.omit(df_real[, .(sentbyte, rcvdbyte)])
df_scaled  <- scale(df_cluster)

# Scree Plot per la scelta ottimale del parametro K
set.seed(123)
wcss <- sapply(1:10, function(k) kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss)
plot(1:10, wcss, type="b", pch=19, col="darkblue",
     xlab="Numero di Cluster (k)", ylab="Within-Cluster Sum of Squares (WCSS)", 
     main="Scree Plot per la Scelta Ottimale di K")

# Esecuzione finale dell'algoritmo con k=3 e iterazioni ottimizzate per la convergenza
kmeans_res <- kmeans(df_scaled, centers = 3, nstart = 25, iter.max = 50)
df_cluster[, Cluster := as.factor(kmeans_res$cluster)]

# Analisi dei Centroidi nello spazio originale non scalato
cat("\nCentroidi dei Cluster (Medie di Volume):\n")
print(df_cluster[, .(Media_Sent_Bytes = mean(sentbyte), Media_Rcvd_Bytes = mean(rcvdbyte)), by = Cluster])

# Metriche di Non Omogeneità Statistica (Diagnostica del Partizionamento)
cat("\nMisure di Non Omogeneità Statistica:\n")
cat("Somma dei Quadrati Entro i Cluster (WCSS Totale):", sum(kmeans_res$withinss), "\n")
cat("Somma dei Quadrati Tra i Cluster (BCSS):", kmeans_res$betweenss, "\n\n")

# Scatterplot del Cluster nello Spazio Bivariato Reale
print(ggplot(df_cluster, aes(x = sentbyte, y = rcvdbyte, color = Cluster)) +
        geom_point(alpha = 0.5) +
        labs(title = "Segmentazione dello Spazio Bivariato via K-Means",
             x = "Sent Bytes (Byte Inviati)", y = "Received Bytes (Byte Ricevuti)") + 
        theme_minimal())