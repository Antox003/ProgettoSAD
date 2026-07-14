cat("--- Step 1: CARICAMENTO E PREPROCESSING ---\n")

# 1. Caricamento del dataset principale reale (Log scaricato con la pem)
df_real <- fread("part-20260701T083657Z.csv") 

# Ho selezionato solo le colonne core necessarie all'analisi per ottimizzare la memoria
df_real <- df_real[, .(sentbyte, rcvdbyte, proto, action, duration)]

cat("--- ISPEZIONE VALORI MANCANTI (Dataset reale) ---\n")
na_summary <- colSums(is.na(df_real))
print(na_summary)
cat("Nota: La presenza di NA nei log di sicurezza rappresenta una mancata segnalazione strutturale.\n\n")

# Rimozione delle righe con valori mancanti per garantire la stabilità dei test
df_real <- na.omit(df_real)


# 2. Caricamento del nuovo dataset sintetico
df_synth <- fread("cybersecurity_sintetico.csv", sep = "auto", strip = TRUE)

# Rinominiamo esplicitamente le colonne per eliminare eventuali spazi vuoti o caratteri nascosti
setnames(df_synth, old = names(df_synth), new = trimws(names(df_synth)))

# Selezioniamo le stesse identiche colonne anche per il sintetico
df_synth <- df_synth[, .(sentbyte, rcvdbyte, proto, action)]
df_synth <- na.omit(df_synth)

cat("Pre-elaborazione e allineamento dei dataset completati con successo.\n\n")