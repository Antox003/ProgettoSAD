#Caricamento dei dati e Pulizia
cat("--- Step 1: CARICAMENTO E PREPROCESSING ---\n")

# Caricamento dei dataset tramite data.table
df_real <- fread("cybersecurity_attacks.csv") 
df_synth <- fread("cybersecurity_sintetico.csv", sep = ",", dec = ".", strip = TRUE)

cat("--- ISPEZIONE VALORI MANCANTI (Dataset reale) ---\n")
na_summary <- colSums(is.na(df_real))
print(na_summary)
cat("Nota: La presenza di NA nei log di sicurezza rappresenta una mancata segnalazione strutturale.\n\n")

# Pulizia e conversione manuale dei decimali per il dataset generato da LLM (ho usato Gemini come LLM)
df_synth[, `Anomaly Scores` := as.numeric(gsub(",", ".", as.character(`Anomaly Scores`)))]
df_synth[, `Packet Length` := as.numeric(gsub(",", ".", as.character(`Packet Length`)))]

# Allineamento del Timestamp
df_synth[, Timestamp := as.POSIXct(as.character(Timestamp), format = "%Y-%m-%d %H:%M:%S")]

cat("Pre-elaborazione dei dati completata.\n\n")