# Analisi Descrittiva ed Inferenziale
cat("--- Step 2: STATISTICHE DESCRITTIVE E DISTRIBUZIONI ---\n")

print(summary(df_real[, .(`Packet Length`, `Anomaly Scores`)]))

# Indici di Posizione Completi (Moda)
calcola_moda <- function(x) {
  uniqv <- unique(na.omit(x))
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
cat("Moda di Packet Length:", calcola_moda(df_real$`Packet Length`), "\n")
cat("Moda di Anomaly Scores:", calcola_moda(df_real$`Anomaly Scores`), "\n\n")

# Statistica Descrittiva Bivariata
cat("--- FASE 2.1: COVARIANZA E CORRELAZIONE CAMPIONARIA ---\n")
v_cov <- cov(df_real$`Packet Length`, df_real$`Anomaly Scores`, use = "complete.obs")
v_cor <- cor(df_real$`Packet Length`, df_real$`Anomaly Scores`, use = "complete.obs")
cat("Covarianza Campionaria:", v_cov, "\n")
cat("Coefficiente di Correlazione Campionario (r_xy):", v_cor, "\n\n")

# Visualizzazioni Univariate e Bivariate
print(ggplot(df_real, aes(x = `Packet Length`)) +
        geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
        labs(title = "Distribuzione Multimodale di Packet Length (Dati Reali)",
             x = "Packet Length (Bytes)", y = "Frequenza") + theme_minimal())

print(ggplot(df_real, aes(y = `Anomaly Scores`)) +
        geom_boxplot(fill = "coral", alpha = 0.7) +
        labs(title = "Boxplot di Anomaly Scores (Dati Reali)", y = "Punteggio") + theme_minimal())

print(ggplot(df_real, aes(x = `Packet Length`, y = `Anomaly Scores`)) +
        geom_point(alpha = 0.4, color = "darkgray") +
        labs(title = "Scatterplot: Relazione tra Packet Length e Anomaly Scores",
             x = "Packet Length (Bytes)", y = "Anomaly Scores") + theme_minimal())

# Test Inferenziali
cat("\n--- TEST CHI-QUADRATO (INDIPENDENZA) ---\n")
tab_contingenza <- table(df_real$Protocol, df_real$`Action Taken`)
print(chisq.test(tab_contingenza))

# Confronto Descrittivo Reale vs Sintetico
cat("\n--- CONFRONTO STATISTICO REALE VS SINTETICO ---\n")
cat("\n[DATI REALI - ANOMALY SCORES]:\n"); print(summary(df_real$`Anomaly Scores`))
cat("\n[DATI SINTETICI - ANOMALY SCORES]:\n"); print(summary(df_synth$`Anomaly Scores`))

# Boxplot di Confronto
df_real_subset <- data.table(Score = df_real$`Anomaly Scores`, Tipo = "Reale")
df_synth_subset <- data.table(Score = df_synth$`Anomaly Scores`, Tipo = "Sintetico")
df_confronto <- rbind(df_real_subset, df_synth_subset)

print(ggplot(df_confronto, aes(x = Tipo, y = Score, fill = Tipo)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Confronto delle Distribuzioni degli Anomaly Scores", x = "Tipologia", y = "Punteggio") +
        theme_minimal() + scale_fill_manual(values = c("steelblue", "coral")))

# Test Chi-Quadrato di Buon Adattamento
cat("\n--- TEST CHI-QUADRATO DI BUON ADATTAMENTO ---\n")
classi_intervallo <- seq(0, 100, length.out = 6)
frequenze_reali <- table(cut(df_real$`Anomaly Scores`, breaks = classi_intervallo, include.lowest = TRUE))
frequenze_sintetiche <- table(cut(df_synth$`Anomaly Scores`, breaks = classi_intervallo, include.lowest = TRUE))
probabilita_attese <- as.vector(frequenze_reali) / sum(frequenze_reali)

print(chisq.test(as.vector(frequenze_sintetiche), p = probabilita_attese))