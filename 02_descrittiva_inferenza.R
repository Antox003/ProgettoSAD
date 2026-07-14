# ==============================================================================
# FASE 2: ANALISI ESPLORATIVA DEI DATI (EDA COMPLETA SU TUTTE LE COLONNE)
# ==============================================================================
cat("\n=========================================================\n")
cat("--- STEP 2: EDA COMPLETA E TEST INFERENZIALI ---\n")
cat("=========================================================\n\n")

# Funzione ausiliaria per il calcolo della Moda
calcola_moda <- function(x) {
  uniqv <- unique(na.omit(x))
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# ------------------------------------------------------------------------------
# 2.1 EDA GLOBALE E UNIVARIATA: COLONNE QUANTITATIVE (NUMERICHE)
# ------------------------------------------------------------------------------
cat("--- 2.1 EDA COMPLETA: PANORAMICA GENERALE E VARIABILI NUMERICHE ---\n\n")

# 1. Summary Generale di TUTTE le colonne presenti nel dataset reale
cat("[SUMMARY COMPLETO DI TUTTE LE COLONNE DEL DATASET]:\n")
print(summary(df_real))
cat("\n---------------------------------------------------------\n\n")

# 2. Indici di posizione e dispersione per Packet Length
cat("[DETTAGLIO STATISTICO: PACKET LENGTH]:\n")
cat("  - Moda:", calcola_moda(df_real$`Packet Length`), "\n")
cat("  - Deviazione Standard:", sd(df_real$`Packet Length`, na.rm = TRUE), "\n")
cat("  - Range Interquartilico (IQR):", IQR(df_real$`Packet Length`, na.rm = TRUE), "\n\n")

# 3. Indici di posizione e dispersione per Anomaly Scores
cat("[DETTAGLIO STATISTICO: ANOMALY SCORES]:\n")
cat("  - Moda:", calcola_moda(df_real$`Anomaly Scores`), "\n")
cat("  - Deviazione Standard:", sd(df_real$`Anomaly Scores`, na.rm = TRUE), "\n")
cat("  - Range Interquartilico (IQR):", IQR(df_real$`Anomaly Scores`, na.rm = TRUE), "\n\n")

# --- GRAFICI UNIVARIATI PER PACKET LENGTH ---
# Grafico 1A: Istogramma Packet Length
print(ggplot(df_real, aes(x = `Packet Length`)) +
        geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
        labs(title = "Distribuzione Multimodale di Packet Length (Dati Reali)",
             x = "Packet Length (Bytes)", y = "Frequenza") + 
        theme_minimal())

# Grafico 1B: Boxplot Packet Length (Nuovo: Identificazione Outlier)
print(ggplot(df_real, aes(y = `Packet Length`)) +
        geom_boxplot(fill = "steelblue", alpha = 0.7) +
        labs(title = "Boxplot di Packet Length (Dati Reali)",
             y = "Packet Length (Bytes)") + 
        theme_minimal())

# --- GRAFICI UNIVARIATI PER ANOMALY SCORES ---
# Grafico 2A: Istogramma Anomaly Scores (Nuovo)
print(ggplot(df_real, aes(x = `Anomaly Scores`)) +
        geom_histogram(bins = 30, fill = "coral", color = "white", alpha = 0.7) +
        labs(title = "Distribuzione degli Anomaly Scores (Dati Reali)",
             x = "Anomaly Score", y = "Frequenza") + 
        theme_minimal())

# Grafico 2B: Boxplot Anomaly Scores
print(ggplot(df_real, aes(y = `Anomaly Scores`)) +
        geom_boxplot(fill = "coral", alpha = 0.7) +
        labs(title = "Boxplot di Anomaly Scores (Dati Reali)",
             y = "Punteggio") + 
        theme_minimal())


# ------------------------------------------------------------------------------
# 2.2 EDA UNIVARIATA: COLONNE QUALITATIVE / CATEGORIALI
# ------------------------------------------------------------------------------
cat("--- 2.2 EDA COMPLETA: VARIABILI CATEGORIALI ---\n\n")

# Tabella Frequenze Assolute e Relative per 'Protocol'
cat("[FREQUENZE PROTOCOLLO]:\n")
freq_proto <- table(df_real$Protocol)
prop_proto <- prop.table(freq_proto) * 100
print(cbind(Frequenza = freq_proto, Percentuale_Percento = round(prop_proto, 2)))
cat("\n")

# Tabella Frequenze Assolute e Relative per 'Action Taken'
cat("[FREQUENZE AZIONI FIREWALL]:\n")
freq_action <- table(df_real$`Action Taken`)
prop_action <- prop.table(freq_action) * 100
print(cbind(Frequenza = freq_action, Percentuale_Percento = round(prop_action, 2)))
cat("\n---------------------------------------------------------\n\n")

# Grafico 3A: Barplot Protocol
print(ggplot(df_real, aes(x = Protocol, fill = Protocol)) +
        geom_bar(alpha = 0.8, show.legend = FALSE) +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, fontface = "bold") +
        labs(title = "Distribuzione delle Frequenze per Protocollo",
             x = "Protocollo", y = "Conteggio Log") +
        theme_minimal() + 
        scale_fill_brewer(palette = "Set2"))

# Grafico 3B: Barplot Action Taken
print(ggplot(df_real, aes(x = `Action Taken`, fill = `Action Taken`)) +
        geom_bar(alpha = 0.8, show.legend = FALSE) +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, fontface = "bold") +
        labs(title = "Distribuzione delle Azioni del Firewall (Action Taken)",
             x = "Azione Intrapresa", y = "Conteggio Log") +
        theme_minimal() + 
        scale_fill_brewer(palette = "Pastel1"))


# ------------------------------------------------------------------------------
# 2.3 STATISTICA DESCRITTIVA BIVARIATA
# ------------------------------------------------------------------------------
cat("--- 2.3 STATISTICA DESCRITTIVA BIVARIATA ---\n")
v_cov <- cov(df_real$`Packet Length`, df_real$`Anomaly Scores`, use = "complete.obs")
v_cor <- cor(df_real$`Packet Length`, df_real$`Anomaly Scores`, use = "complete.obs")
cat("Covarianza Campionaria:", v_cov, "\n")
cat("Coefficiente di Correlazione Campionario (r_xy):", v_cor, "\n\n")

# Grafico 4: Scatterplot Bivariato
print(ggplot(df_real, aes(x = `Packet Length`, y = `Anomaly Scores`)) +
        geom_point(alpha = 0.4, color = "darkgray") +
        labs(title = "Scatterplot: Relazione tra Packet Length e Anomaly Scores",
             x = "Packet Length (Bytes)", y = "Anomaly Scores") + 
        theme_minimal())


# ------------------------------------------------------------------------------
# 2.4 TEST INFERENZIALE DI INDIPENDENZA (CHI-QUADRATO)
# ------------------------------------------------------------------------------
cat("\n--- 2.4 TEST CHI-QUADRATO DI INDIPENDENZA ---\n")
tab_contingenza <- table(df_real$Protocol, df_real$`Action Taken`)
cat("Tabella di Contingenza (Protocollo vs Azione):\n")
print(tab_contingenza)
cat("\nEsito del Test del Chi-Quadrato:\n")
test_chi2_ind <- chisq.test(tab_contingenza)
print(test_chi2_ind)


# ------------------------------------------------------------------------------
# 2.5 CONFRONTO STATISTICO E VALIDAZIONE DATASET SINTETICO (LLM)
# ------------------------------------------------------------------------------
cat("\n--- 2.5 CONFRONTO STATISTICO REALE VS SINTETICO ---\n")
cat("\n[DATI REALI - ANOMALY SCORES]:\n"); print(summary(df_real$`Anomaly Scores`))
cat("\n[DATI SINTETICI - ANOMALY SCORES]:\n"); print(summary(df_synth$`Anomaly Scores`))

# Grafico 5: Boxplot di Confronto Reale vs Sintetico
df_real_subset  <- data.table(Score = df_real$`Anomaly Scores`, Tipo = "Reale")
df_synth_subset <- data.table(Score = df_synth$`Anomaly Scores`, Tipo = "Sintetico")
df_confronto    <- rbind(df_real_subset, df_synth_subset)

print(ggplot(df_confronto, aes(x = Tipo, y = Score, fill = Tipo)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Confronto delle Distribuzioni degli Anomaly Scores",
             x = "Tipologia di Dataset", y = "Punteggio") +
        theme_minimal() + 
        scale_fill_manual(values = c("steelblue", "coral")))


# ------------------------------------------------------------------------------
# 2.6 TEST CHI-QUADRATO DI BUON ADATTAMENTO (GOODNESS OF FIT)
# ------------------------------------------------------------------------------
cat("\n--- 2.6 TEST CHI-QUADRATO DI BUON ADATTAMENTO ---\n")

# Discretizzazione basata sui quantili empirici del dataset reale per garantire frequenze attese > 5
classi_intervallo <- unique(quantile(df_real$`Anomaly Scores`, probs = seq(0, 1, length.out = 6), na.rm = TRUE))

frequenze_reali      <- table(cut(df_real$`Anomaly Scores`, breaks = classi_intervallo, include.lowest = TRUE))
frequenze_sintetiche <- table(cut(df_synth$`Anomaly Scores`, breaks = classi_intervallo, include.lowest = TRUE))
probabilita_attese   <- as.vector(frequenze_reali) / sum(frequenze_reali)

test_buon_adattamento <- chisq.test(as.vector(frequenze_sintetiche), p = probabilita_attese)
print(test_buon_adattamento)