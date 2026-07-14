cat("--- Step 2: STATISTICHE DESCRITTIVE E DISTRIBUZIONI ---\n")

# Statistiche descrittive generali
print(summary(df_real[, .(sentbyte, rcvdbyte)]))

# --- INDICI DI POSIZIONE COMPLETI (MODA) ---
calcola_moda <- function(x) {
  uniqv <- unique(na.omit(x))
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
cat("Moda di Sent Bytes (Byte Inviati):", calcola_moda(df_real$sentbyte), "\n")
cat("Moda di Rcvd Bytes (Byte Ricevuti):", calcola_moda(df_real$rcvdbyte), "\n\n")

# --- STATISTICA DESCRITTIVA BIVARIATA ---
cat("--- FASE 2.1: COVARIANZA E CORRELAZIONE CAMPIONARIA ---\n")
v_cov <- cov(df_real$sentbyte, df_real$rcvdbyte, use = "complete.obs")
v_cor <- cor(df_real$sentbyte, df_real$rcvdbyte, use = "complete.obs")
cat("Covarianza Campionaria:", v_cov, "\n")
cat("Coefficiente di Correlazione Campionario (r_xy):", v_cor, "\n\n")

# Grafico 1: Istogramma per sentbyte (Distribuzione dei volumi in uscita)
print(ggplot(df_real, aes(x = sentbyte)) +
        geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.7) +
        labs(title = "Distribuzione Empirica dei Byte Inviati (sentbyte)",
             x = "Sent Bytes (Bytes)", y = "Frequenza") + 
        theme_minimal())

# Grafico 2: Boxplot per rcvdbyte (Distribuzione dei volumi in ingresso)
print(ggplot(df_real, aes(y = rcvdbyte)) +
        geom_boxplot(fill = "coral", alpha = 0.7) +
        labs(title = "Boxplot di Rcvd Bytes (Dati Reali)", y = "Received Bytes") + 
        theme_minimal())

# Grafico 3: Scatterplot Bivariato tra Volumi in Entrata ed Uscita
print(ggplot(df_real, aes(x = sentbyte, y = rcvdbyte)) +
        geom_point(alpha = 0.4, color = "darkgray") +
        labs(title = "Scatterplot: Relazione bivariata tra Sent Bytes e Rcvd Bytes",
             x = "Sent Bytes (Bytes)", y = "Received Bytes (Bytes)") + 
        theme_minimal())

# --- TEST INFERENZIALI: CHI-QUADRATO DI INDIPENDENZA ---
cat("\n--- TEST CHI-QUADRATO (INDIPENDENZA TRA PROTOCOLLO E AZIONE) ---\n")
tab_contingenza <- table(df_real$proto, df_real$action)
test_chi2_ind <- chisq.test(tab_contingenza, simulate.p.value = TRUE, B = 2000)
print(test_chi2_ind)

# --- CONFRONTO DESCRITTIVO REALE VS SINTETICO ---
cat("\n--- CONFRONTO STATISTICO REALE VS SINTETICO: RECEIVED BYTES ---\n")
cat("\n[DATI REALI - RCVD BYTES]:\n"); print(summary(df_real$rcvdbyte))
cat("\n[DATI SINTETICI - RCVD BYTES]:\n"); print(summary(df_synth$rcvdbyte))

# Grafico 7: Boxplot di Confronto Reale vs Sintetico
df_real_subset  <- data.table(Byte = df_real$rcvdbyte, Tipo = "Reale")
df_synth_subset <- data.table(Byte = df_synth$rcvdbyte, Tipo = "Sintetico")
df_confronto    <- rbind(df_real_subset, df_synth_subset)

print(ggplot(df_confronto, aes(x = Tipo, y = Byte, fill = Tipo)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = "Confronto delle Distribuzioni dei Received Bytes", 
             x = "Tipologia di Dataset", y = "Received Bytes") +
        theme_minimal() + 
        scale_fill_manual(values = c("steelblue", "coral")))

# --- TEST CHI-QUADRATO DI BUON ADATTAMENTO (GOODNESS OF FIT) ---
cat("\n--- TEST CHI-QUADRATO DI BUON ADATTAMENTO ---\n")

# Utilizziamo i quantili dei dati reali per definire 5 classi di intervallo.
# Questo garantisce l'assenza di frequenze attese inferiori a 5, preservando il rigore formale del test.
classi_intervallo <- unique(quantile(df_real$rcvdbyte, probs = seq(0, 1, length.out = 6), na.rm = TRUE))

# Eseguiamo la discretizzazione basata sulle classi empiriche reali
frequenze_reali      <- table(cut(df_real$rcvdbyte, breaks = classi_intervallo, include.lowest = TRUE))
frequenze_sintetiche <- table(cut(df_synth$rcvdbyte, breaks = classi_intervallo, include.lowest = TRUE))

# Calcolo del vettore delle probabilità attese basato sulla popolazione reale
probabilita_attese <- as.vector(frequenze_reali) / sum(frequenze_reali)

# Esecuzione e stampa del test di buon adattamento sulle frequenze del dataset sintetico
test_buon_adattamento <- chisq.test(as.vector(frequenze_sintetiche), p = probabilita_attese)
print(test_buon_adattamento)