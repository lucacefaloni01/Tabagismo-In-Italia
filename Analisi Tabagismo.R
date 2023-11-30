########LIBRERIE##########
library(readxl) #leggere file excel 
library(lubridate) #calcolare l'eta in base alle date
library(Factoshiny)
# Carica la libreria plotrix
library(plotrix)#Grafici
###############DATI###############
library(readxl)
library(googledrive)
file_id <- "1dUH9TWNWI2OYNs7FcO1KLxW5efSkuzeIEQ2C6FUYzjc" 
file_path <- drive_download(as_id(file_id), overwrite = TRUE)
data <- readxl::read_excel("Tabagismo in Italia e impatto dei dispositivi senza combustione (Risposte).xlsx",col_types = c("numeric", "date", "text",
                                                                                                                           "text", "text", "text", "text", "numeric",
                                                                                                                           "numeric", "text", "text", "text", 
                                                                                                                           "numeric", "text", "text", "numeric", 
                                                                                                                           "numeric", "text", "text", "text", 
                                                                                                                           "text", "numeric", "text", "text", 
                                                                                                                           "text", "text", "text", "text", "text", 
                                                                                                                           "text"))


# Rimuovere apici speciali, spazi e punti di domanda e sostituire con underscore
nomi_colonne <- colnames(data)
# Rimuovere gli apici speciali e sostituire con underscore
nomi_colonne <- gsub("`", "", nomi_colonne)  # Rimuovi gli apici speciali
nomi_colonne <- gsub(" ", "_", nomi_colonne)  # Sostituisci gli spazi con underscore
nomi_colonne <- gsub("`|\\?|\\s", "_", nomi_colonne)
nomi_colonne <- gsub("\\(|\\)", "", nomi_colonne)
nomi_colonne <- gsub("'", "_", nomi_colonne)
# Rimuovi punti e virgole dai nomi delle colonne
nomi_colonne <- gsub("[.,]", "", nomi_colonne)
colnames(data) <- nomi_colonne
#Calcolo dell' età
data$Età <- year(Sys.Date()) - year(data$Data_di_nascita)# Calcolo e Aggiunta età attuale individui 



####Pulizia dei dati domanda 10####
valori_da_sostituire <- c("camel blu", "Camel blu", "Camel Blu")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare <- "Camel Blu"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire, valore_da_assegnare, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)



valori_da_sostituire2 <- c("Cesterfield blue", "Chesterfield blu", "Chesterfield blu naturale", "Chesterfield blu, winston blu e rothmans blu")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare2 <- "Chesterfield Blu"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire2, valore_da_assegnare2, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)



valori_da_sostituire3 <- c("Lucky strike", "Lucky strike amber", "Lucky strike blue", "Lucky strike o winston blue")
# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare3 <- "Lucky Strike Amber"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire3, valore_da_assegnare3, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)


valori_da_sostituire4 <- c("Malboro", "Marlboro", "Marlboro bianche", "Marlboro gold", "Marlboro Sigarellos","Marlboro/Winston", "Marlboro light")
# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare4 <- "Marlboro Gold"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire4, valore_da_assegnare4, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)




valori_da_sostituire5 <- c("Malboro rosse", "Marlboro rosse")
# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare5 <- "Marlboro Rosse"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire5, valore_da_assegnare5, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)


valori_da_sostituire6 <- c("winston blu ","winston blu", "Winston blu", " Winston Blu", "Winston blue","Wiston", "Wiston Blu", "Winston  sigarette e tabacco","sigarette winston blu")
# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare6 <- "Winston Blu"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire6, valore_da_assegnare6, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)

#SOSTITUZIONE VALORI MANCANTI 
# Definisci i valori da sostituire con NA
valori_da_sostituire8 <- c("Heets sienna", "Heets Teak")

# Sostituisci i valori nella colonna desiderata con NA
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc[data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc%in% valori_da_sostituire8] <- NA


valori_da_sostituire7 <- c("Camel- pueblo giallo","Tabacco")
# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare7 <- "Pueblo Giallo"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc<- ifelse(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc %in% valori_da_sostituire7, valore_da_assegnare7, data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)

table(data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc)



####Pulizia dei dati domanda 18####
# Definisci i valori da sostituire con NA
valori_da_sostituire10 <- c("chiara valle")

# Sostituisci i valori nella colonna desiderata con NA
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco[data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire10] <- NA



valori_da_sostituire9 <- c("Heets amber","Heets Amber / Heets Silver")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare9 <- "Heets Amber"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire9, valore_da_assegnare9, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire11 <- c("Blu","Blue","Heets blu","Heets blue", "Heets Blue", "heets blu")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare11 <- "Heets Blue"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire11, valore_da_assegnare11, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )



valori_da_sostituire12 <- c("heets bronze", "Heets Bronze")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare12 <- "Heets Bronze"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire12, valore_da_assegnare12, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire13 <- c("heets mauve","Heets mauve","heets mauve wave","Heets mauwe","Heets muoave","Heets wave","Mauve wave", "Heets meuve")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare13 <- "Heets Mauve Wave"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire13, valore_da_assegnare13, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )





valori_da_sostituire14 <- c("heets sienna caps","Heets sienna caps","Heets Sienna caps","Heets Sienna Caps","heets sienna cups","Heets Sienna Cups","Sienna caps","Sienna Caps", "Heets sienna cups")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare14 <- "Heets Sienna Caps"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire14, valore_da_assegnare14, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )



valori_da_sostituire15 <- c("heets silver","Heets silver")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare15 <- "Heets Silver"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire15, valore_da_assegnare15, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )






valori_da_sostituire16 <- c("Heets teack","heets teak", "Heets teak","heets teek", "Heets terea team")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare16 <- "Heets Teak"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire16, valore_da_assegnare16, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )



valori_da_sostituire17 <- c("Sienna", "Heets sienna", "Heets Sienna", "Heets sienna, heets turchesi")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare17 <- "Heets Sienna"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire17, valore_da_assegnare17, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire18 <- c("heets turchesi", "Heets turchesi")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare18 <- "Heets Turquoise"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire18, valore_da_assegnare18, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire19 <- c("heetze russet")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare19 <- "Heets Russet"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire19, valore_da_assegnare19, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire20 <- c("Liquidi", "Liquido con nicotina", "liquido tabaccoso 9 nicotina", "Sigaretta elettronica con liquido")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare20 <- "Liquido con nicotina"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire20, valore_da_assegnare20, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

valori_da_sostituire21 <- c("Classic Tobacco", "Lucky strike glo Neo", "Neo", "Neo Classic", "Neo Classic tobacco")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare21 <- "Neo Classic Tobacco"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire21, valore_da_assegnare21, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

valori_da_sostituire22 <- c("Scarlett", "Scarlet Click", "Neo Glo scarlet", "Neo frutti rossi")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare22 <- "Neo Scarlet Click"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire22, valore_da_assegnare22, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

valori_da_sostituire23 <- c("Beryl Click")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare23 <- "Neo Beryl Click"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire23, valore_da_assegnare23, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

valori_da_sostituire24 <- c("E-cig Kiwi Vapor", "Sigaretta elettronica KIWI", "E liquid tabacco latakia")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare24 <- "Liquido per Kiwi"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire24, valore_da_assegnare24, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire25 <- c("Terea", "Terea amber", "Terea Amber")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare25 <- "Terea Amber"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire25, valore_da_assegnare25, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

valori_da_sostituire26 <- c("Terea blu","Terea Blu","Terea menta forte", "terea blu")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare26 <- "Terea Blue"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire26, valore_da_assegnare26, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire27 <- c("Terea mauve", "terea")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare27 <- "Terea Mauve Wave"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire27, valore_da_assegnare27, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )


valori_da_sostituire28 <- c("Terea teak", "Terea Teak")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare28 <- "Terea Teak"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire28, valore_da_assegnare28, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

valori_da_sostituire29 <- c("terea turchesi", "Terea turchesi")

# Definisci il valore con cui sostituire i valori della lista
valore_da_assegnare29 <- "Terea Turquoise"

# Sostituisci i valori della lista con il valore nuovo nella colonna desiderata
data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco <- ifelse(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco %in% valori_da_sostituire29, valore_da_assegnare29, data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco )

table(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco)



####Pulizia dei dati domanda 21####
#Pulizia dati colonna 21 
parole_da_rimuover3 <- c(" anno",
                         " anni",
                         "/2 anni e mezzo",
                         "/4 anni")

# Rimozione delle parole dalla colonna "testo" utilizzando gsub
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub(paste(parole_da_rimuover3, collapse = "|"), "", data$Da_quanto_fumi_sigarette_senza_combustione__, ignore.case = TRUE)
table(data$Da_quanto_fumi_sigarette_senza_combustione__)

# Sostituzione del valore "5 mesi" con "0.41"
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("1 mese", as.character("0.08"), data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("10 mesi", "0.83", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("4 mesi", "0.33", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("5 mesi", "0.42", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("6 mesi", "0.50", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("7 mesi", "0.58", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("8 mesi", "0.66", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("9 mesi", "0.75", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("qualche mese", "0.33", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("2 settimane", "0.01", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("2021", "2", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Da quest’estate", "0.83", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Due", "2", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Fumo entrambe", "1", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Poco", "0.33", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Ottobre 2021", "2", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Quasi 2", "2", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("quasi due", "2", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("un", "1", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Un circa", "1", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("un e qualche mese", "1", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Un paio di", "2", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("circa ", "", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Ottobre ", "", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub(" e 0.33", "", data$Da_quanto_fumi_sigarette_senza_combustione__)
data$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("15|13", "9", data$Da_quanto_fumi_sigarette_senza_combustione__)

data$Da_quanto_fumi_sigarette_senza_combustione__ <-  as.numeric(data$Da_quanto_fumi_sigarette_senza_combustione__)
####Pulizia dati colonna 23#### 
parole_da_rimuovereee <- c("(Alla domanda precedente ho messo un numero a caso perché in realtà non lo so ma Comunque fumavo poco). Un pacchetto di Lucky Strike costa €",
                           " euro a settimana",
                           " euro",
                           " euro al mese",
                           "€ ogni 3 settimane",
                           " euro al mese",
                           "euro a settimana",
                           "/20€",
                           " euro al mese",
                           "-30 euro a settimana",
                           "/25",
                           "/25 euro a settimana",
                           "€ a al mese",
                           " euro al mese",
                           " euro a settimana",
                           " un pacchetto al giorno",
                           " a settimana",
                           "€ a settimana",
                           "/10€",
                           "€ a settimana",
                           "almeno ",
                           "Meno di ",
                           " euro a settimana",
                           "Uguale")

# Rimozione delle parole dalla colonna "testo" utilizzando gsub
data$Prima_quanto_spendevi__ <- gsub(paste(parole_da_rimuovereee, collapse = "|"), "", data$Prima_quanto_spendevi__, ignore.case = TRUE)
data$Prima_quanto_spendevi__ <- as.numeric(data$Prima_quanto_spendevi__)
table(data$Prima_quanto_spendevi__)


####Tavole categorie####
table(data$`Sei un fumatore? (Se si specificare la tipologia)`)
table(data$`Cosa fumi?...7`)
table(data$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`)
table(data$`Da quanto tempo fumi ? (scrivere il valore in anni)`)
table(data$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`)
table(data$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...11`)
table(data$`Hai mai avuto problemi di salute relativi all'apparato circolatorio? (Es.ipertensione, ictus e infarto)`)
table(data$`Quanto spendi in media a settimana per fumare ?`)
table(data$`I tuoi genitori fumano? (ne basta uno dei due per il sì)`)
table(data$`Cosa fumi?...15`)
table(data$`Quante sigarette fumi al giorno?`)
table(data$`Quanto spendi a settimana?`)
table(data$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco)
table(data$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...19`)
table(data$`Prima di fumare le sigarette senza combustione fumavi le sigarette tradizionali ?
(se No non rispondere alle domande successive)`)
table(data$Da_quanto_fumi_sigarette_senza_combustione__)
table(data$`Prima quante sigarette fumavi?`)
table(data$Prima_quanto_spendevi__)
table(data$`Hai avuto miglioramenti dal punto di vista salutare?`)
table(data$`Hai mai fumato?`)
table(data$`Ti da fastidio l'odore di fumo?`)
table(data$`I tuoi genitori fumano?`)
table(data$`Hai qualche dipendenza?`)
table(data$Età)

#####PAGINA GENERALE #######
# 1 Numero degli individui che hanno partecipato 
Individui <-  length(data$Informazioni_cronologiche)
# 2 Età media delle degli individui interrogati 
Età_Media <- median(na.omit(data$Età))
# 3 Grafico a torta Sesso 
Tavola_sesso <-  table(data$Sesso)
data_sesso <-  data.frame(Tavola_sesso)
pct<-round((Tavola_sesso/margin.table(Tavola_sesso)*100),1)
lbls<-paste(pct,"%",sep="")
cols<-c("pink","blue")
labs<-c("Femmina", "Maschio")
pie3D(data_sesso$Freq, , labels = lbls, col=cols, main = "Percentuali del Sesso degli individui")
legend("topright", cex =1, legend = labs, fill = cols)
# 4 Grafico a torta Occupazione 
Tavola_occupazione <-  table(data$Cosa_fai_nella_vita_)
data_occupazione <-  data.frame(Tavola_occupazione)
pct_c<-round((Tavola_occupazione/margin.table(Tavola_occupazione)*100),1)
lbls_c<-paste(pct_c,"%",sep="")
cols_c<-c("red","green","blue","yellow")
labs_c<-data_occupazione$Var1
pie3D(data_occupazione$Freq, , labels = lbls_c, col=cols_c, main = "Percentuali dell'Occupazione degli individui")
legend("topright", cex =1, legend = labs_c, fill = cols_c)
# 5 Percentuale Tradizonali
classe <-  table(data$Sei_un_fumatore__Se_si_specificare_la_tipologia)
classe_perc<-round((classe/margin.table(classe)*100),1)
tipo_individuo <-  data.frame(classe_perc)
Perc_Trad <- tipo_individuo[3,]
# 6 Percentuale Senza Combustione
Perc_Sens_Comb <- tipo_individuo[2,]
#7 Non fumatori 
Perc_Non_Fum <-  tipo_individuo[1,]



#####NON FUMATORI####
No <- subset(data,Sei_un_fumatore__Se_si_specificare_la_tipologia == "No", )
No_C <- No[,-c(7:24)]
#
table(No_C$`Hai mai fumato?`)
# ANALISI TESI 
median(na.omit(No_C$Età))
table(No_C$Sesso)
table(No_C$Hai_mai_fumato_)
#Statistiche descrittive Singole 
summary(No_C)
table(No_C$`Hai mai fumato?`)
table(No_C$Ti_da_fastidio_l_odore_di_fumo_)
table(No_C$I_tuoi_genitori_fumano_)
table(No_C$Hai_qualche_dipendenza_)
table(No_C$Età)
TableNo_C$
#Statistiche descrittive Incorciate 
table(No_C$`Hai mai fumato?`, No_C$Età)
table(No_C$`Hai mai fumato?`, No_C$`I tuoi genitori fumano?`)
table(No_C$`Hai mai fumato?`, No_C$`Hai qualche dipendenza?`)
table(No_C$`Hai mai fumato?`, No_C$Sesso)
table(No_C$`Hai mai fumato?`, No_C$`I tuoi genitori fumano?`)
table(No_C$`Hai mai fumato?`, No_C$`Hai qualche dipendenza?`)
#####SIgarette tradizionali####
Tradizonali <- subset(data,Sei_un_fumatore__Se_si_specificare_la_tipologia == "Sigarette Tradizionali", )
Trad_c <- Tradizonali[,c(1:14,31)]

####ANALISI PER TESI 
Individui_trad <- length(Trad_c$Informazioni_cronologiche)
Sex_trad <- table(Trad_c$Sesso)
media_età_trad <- mean(na.omit(Trad_c$Età))
mediana_età_trad <- median(na.omit(Trad_c$Età))
freq_cosafumi <- table(Trad_c$Cosa_fumi_7)
max_sig_t_al_giorno <- max(na.omit(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`))
mediana_sig_t_al_giorno <- median(na.omit(Trad_c$Quante_sigarette_fumi_al_giorno___esprimere_il_valore_numerico))
media_sig_t_al_giorno <- mean(na.omit(Trad_c$Quante_sigarette_fumi_al_giorno___esprimere_il_valore_numerico))
min_sig_t_al_giorno <- min(na.omit(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`))

median(Trad_c$Da_quanto_tempo_fumi___scrivere_il_valore_in_anni)
sort(table(Trad_c$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc))
table(Trad_c$Hai_problemi_di_salute_relativi_all_apparato_respiratorio__Estosse_catarro_bronchiti_ricorrenti_asma11)
table(Trad_c$Hai_mai_avuto_problemi_di_salute_relativi_all_apparato_circolatorio__Esipertensione_ictus_e_infarto)




median(na.omit(Trad_c$Quanto_spendi_in_media_a_settimana_per_fumare__))
table(Trad_c$I_tuoi_genitori_fumano__ne_basta_uno_dei_due_per_il_sì)
#Statistiche descrittive Singole##### 
summary(Trad_c)
table(Trad_c$`Cosa fumi?...7`)
table(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`)
table(Trad_c$`Da quanto tempo fumi ? (scrivere il valore in anni)`)
table(Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`)
table(Trad_c$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...11`)
table(Trad_c$`Hai mai avuto problemi di salute relativi all'apparato circolatorio? (Es.ipertensione, ictus e infarto)`)
table(Trad_c$`Quanto spendi in media a settimana per fumare ?`)
table(Trad_c$`I tuoi genitori fumano? (ne basta uno dei due per il sì)`)
#Statistiche descrittive Incorciate 
table(Trad_c$`Cosa fumi?...7`, Trad_c$Età)
table(Trad_c$`Cosa fumi?...7`, Trad_c$Sesso)
table(Trad_c$`Cosa fumi?...7`, Trad_c$`Cosa fai nella vita?`)
#
table(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`, Trad_c$Età)
table(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`, Trad_c$Sesso)
table(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`, Trad_c$`Cosa fai nella vita?`)
table(Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`, Trad_c$`Cosa fumi?...7`)
#
table(Trad_c$`Da quanto tempo fumi ? (scrivere il valore in anni)`, Trad_c$Età)
table(Trad_c$`Da quanto tempo fumi ? (scrivere il valore in anni)`, Trad_c$Sesso)
table(Trad_c$`Da quanto tempo fumi ? (scrivere il valore in anni)`, Trad_c$`Cosa fai nella vita?`)
table(Trad_c$`Da quanto tempo fumi ? (scrivere il valore in anni)`, Trad_c$`Cosa fumi?...7`)
table(Trad_c$`Da quanto tempo fumi ? (scrivere il valore in anni)`, Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`)
#
table(Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`, Trad_c$Età)
table(Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`, Trad_c$Sesso)
table(Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`, Trad_c$`Cosa fai nella vita?`)
table(Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`, Trad_c$`Cosa fumi?...7`)
#
table(Trad_c$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...11`, Trad_c$Età)
table(Trad_c$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...11`, Trad_c$Sesso)
table(Trad_c$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...11`, Trad_c$`Cosa fumi?...7`)#
table(Trad_c$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...11`, Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`)
# 
table(Trad_c$`Hai mai avuto problemi di salute relativi all'apparato circolatorio? (Es.ipertensione, ictus e infarto)`, Trad_c$`Cosa fumi?...7`)
table(Trad_c$`Hai mai avuto problemi di salute relativi all'apparato circolatorio? (Es.ipertensione, ictus e infarto)`, Trad_c$Sesso)
table(Trad_c$`Hai mai avuto problemi di salute relativi all'apparato circolatorio? (Es.ipertensione, ictus e infarto)`, Trad_c$Età)
table(Trad_c$`Hai mai avuto problemi di salute relativi all'apparato circolatorio? (Es.ipertensione, ictus e infarto)`,Trad_c$`Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es._Heets_sienna,_Camel_blu_ecc._`)
#
table(Trad_c$`Quanto spendi in media a settimana per fumare ?`, Trad_c$Età)
table(Trad_c$`Quanto spendi in media a settimana per fumare ?`, Trad_c$Sesso)
table(Trad_c$`Quanto spendi in media a settimana per fumare ?`, Trad_c$`Cosa fumi?...7`)#
table(Trad_c$`Quanto spendi in media a settimana per fumare ?`, Trad_c$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`)#
#
table(Trad_c$`I tuoi genitori fumano? (ne basta uno dei due per il sì)`, Trad_c$Età)
table(Trad_c$`I tuoi genitori fumano? (ne basta uno dei due per il sì)`, Trad_c$Sesso)
table(Trad_c$`I tuoi genitori fumano? (ne basta uno dei due per il sì)`, Trad_c$`Cosa fumi?...7`)
table(Trad_c$`I tuoi genitori fumano? (ne basta uno dei due per il sì)`, Trad_c$Età)
#######Sig Senza combustione#######
S_C <-  subset(data,Sei_un_fumatore__Se_si_specificare_la_tipologia == "Sigarette Senza Combustione" )
S_C_c <- S_C[,c(1,2,3,4,5,6,15,16,17,18,19,20,21,22,23,24,31)]

#Pulizia e manipolazione dati#######
#install.packages("mice")
library(mice)
# Sostituisci con il nome della tua variabile target
target_variable <- "`Prima di fumare le sigarette senza combustione fumavi le sigarette tradizionali ?
(se No non rispondere alle domande successive)`"

# Elenca tutte le variabili tranne la variabile target (features)
features <- setdiff(names(S_C_c[,-c(1,2,3,4,5,6,7,10,11,12,16)]), target_variable)
# Imposta il numero di iterazioni di imputazione (puoi modificarlo)
num_imputations <- 5

# Esegui l'imputazione
imp_model <- mice(S_C_c[,-c(1,2,3,4,5,6,7,10,11,12,16)], method = c("pmm", "logreg"))

# Utilizza l'operatore di backtick per citare il nome della variabile con uno spazio
imp_model <- mice(S_C_c[, -c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 16)], method = c("pmm", "logreg"))

rf 
imputed_data <- complete(imp_model, action = 1:5)  # Restituisce le prime 5 imputazioni

#PROVA DUE PER IMPUTAZIONE
# Crea un modello di imputazione
imp_model <- mice(S_C_c[,-c(1,2,3,4,5,6,7,10,11,12,16)], 
                  method = c("pmm", "polyreg"),
                  m = 5)  # Numero di imputazioni desiderate


imp_model <- mice(S_C_c, 
                  method = c("pmm", "polyreg"),
                  m = 17)  # Numero di imputazioni desiderate

#ANALISI PER TESI ########
Individui <- length(S_C_c$Informazioni_cronologiche)
Sex <- table(S_C_c$Sesso)
media_età <- mean(na.omit(S_C_c$Età))
mediana_età <- median(na.omit(S_C_c$Età))
#
media_sig_fum <- mean(na.omit(S_C_c$`Quante sigarette fumi al giorno?`))
mediana_sig_fum <- median(na.omit(S_C_c$`Quante sigarette fumi al giorno?`))
#
media_spesa_sett <- mean(na.omit(S_C_c$`Quanto spendi a settimana?`))
mediana_spesa_sett <- median(na.omit(S_C_c$`Quanto spendi a settimana?`))
#
tip_dispositivi <- sort(table(S_C_c$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco))
#
table(S_C_c$Hai_problemi_di_salute_relativi_all_apparato_respiratorio__Estosse_catarro_bronchiti_ricorrenti_asma19)
#Subset sigarette senza combustione
S_C_ex <-  subset(S_C_c,Prima_di_fumare_le_sigarette_senza_combustione_fumavi_le_sigarette_tradizionali___se_No_non_rispondere_alle_domande_successive == "Si" )
table(S_C_c$Prima_di_fumare_le_sigarette_senza_combustione_fumavi_le_sigarette_tradizionali___se_No_non_rispondere_alle_domande_successive)
#
table(S_C_c$Da_quanto_fumi_sigarette_senza_combustione__)
hjymax_temp_s_c <- max(na.omit(S_C_ex$Da_quanto_fumi_sigarette_senza_combustione__))
mediana_temp_s_c <- median(na.omit(S_C_ex$Da_quanto_fumi_sigarette_senza_combustione__))
min_temp_s_c <- min(na.omit(S_C_c$Da_quanto_fumi_sigarette_senza_combustione__))
#
table(S_C_c$Prima_quante_sigarette_fumavi_)
max_ex_sig_fum_s_c <- max(na.omit(S_C_c$Prima_quante_sigarette_fumavi_))
mediana_ex_sig_fum_s_c <- median(na.omit(S_C_ex$Prima_quante_sigarette_fumavi_))
media_ex_sig_fum_s_c <- mean(na.omit(S_C_c$Prima_quante_sigarette_fumavi_))
min_ex_sig_fum_s_c <- min(na.omit(S_C_c$Prima_quante_sigarette_fumavi_))
#
table(S_C_c$Prima_quanto_spendevi__)
max_ex_spe_s_c <- max(na.omit(S_C_c$Prima_quanto_spendevi__))
mediana_ex_spe_s_c <- median(na.omit(S_C_ex$Prima_quanto_spendevi__))
media_ex_spe_s_c <- mean(na.omit(S_C_c$Prima_quanto_spendevi__))
min_ex_spe_s_c <- min(na.omit(S_C_c$Prima_quanto_spendevi__))
#
table(S_C_ex$Hai_avuto_miglioramenti_dal_punto_di_vista_salutare_)
#Statistiche descrittive Singole##### 
summary(S_C_c)
table(S_C_c$Età)
table(S_C_c$Sesso)
table(S_C_c$`Cosa fumi?...15`)
table(S_C_c$`Quante sigarette fumi al giorno?`)
table(S_C_c$`Quanto spendi a settimana?`)
k <- table(S_C_c$Che_tipologia_fumi__EsHeets_sienna_Neo_Classic_Tobacco)
table(S_C_c$`Hai problemi di salute relativi all'apparato respiratorio? (Es.tosse, catarro, bronchiti ricorrenti, asma)...19`)
table(S_C_c$`Prima di fumare le sigarette senza combustione fumavi le sigarette tradizionali ?
(se No non rispondere alle domande successive)`)
table(S_C_c$Da_quanto_fumi_sigarette_senza_combustione__)
table(S_C_c$`Prima quante sigarette fumavi?`)
table(S_C_c$Prima_quanto_spendevi__)
table(S_C_c$`Hai avuto miglioramenti dal punto di vista salutare?`)
#Statistiche descrittive incorciate
table(S_C_c$`Quante sigarette fumi al giorno?`,S_C_c$`Prima quante sigarette fumavi?`)
table(S_C_c$`Cosa fumi?...15`, S_C_c$`Hai avuto miglioramenti dal punto di vista salutare?`)
table(S_C_c$`Quanto spendi a settimana?`,S_C_c$Prima_quanto_spendevi__)
table(S_C_c$Età, S_C_c$`Quante sigarette fumi al giorno?`)
######GRAFICI PER TESI#####
#grafico tipologia di individuo dataset generale  
tavola_tip_ind <-  as.data.frame(table(data$Sei_un_fumatore__Se_si_specificare_la_tipologia))
valoriG <- tavola_tip_ind$Freq
labelG <- tavola_tip_ind$Var1
perc<- round(valoriG/sum(valoriG)*100)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie(valoriG, labels = labelG,col=rainbow(length(labelG)), main="Tipologia di individui intervistati")
# grafico sottogruppo Senza combustione 
tavola_fsc_tip <-  as.data.frame(table(data$Cosa_fumi_15,data$Sesso))
# Crea il grafico a barre multiplo
ggplot(tavola_fsc_tip, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grafico a Barre Multiplo",
       x = "Variabile 1",
       y = "Frequenza") +
  scale_fill_discrete(name = "Variabile 2") +
  theme_minimal()
#tradizionali
tavola_fsc_tipdue <-  as.data.frame(table(data$Cosa_fumi_7,data$Cosa_fumi__esprimere_prima_la_marca_e_poi_la_tipologia_es_Heets_sienna_Camel_blu_ecc))
# Crea il grafico a barre multiplo
ggplot(tavola_fsc_tipdue, aes(x = Var1, y = Freq, fill = factor(Var2))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Grafico a Barre Multiplo",
       x = "Variabile 1",
       y = "Frequenza") +
  scale_fill_discrete(name = "Variabile 2") +
  theme_minimal()


###MCA####
#####Categorizzazione data per MCA####
datamca <- data

#Pulizia dati colonna 23 
parole_da_rimuovereee <- c("(Alla domanda precedente ho messo un numero a caso perché in realtà non lo so ma Comunque fumavo poco). Un pacchetto di Lucky Strike costa €",
                           " euro a settimana",
                           " euro",
                           " euro al mese",
                           "€ ogni 3 settimane",
                           " euro al mese",
                           "euro a settimana",
                           "/20€",
                           " euro al mese",
                           "-30 euro a settimana",
                           "/25",
                           "/25 euro a settimana",
                           "€ a al mese",
                           " euro al mese",
                           " euro a settimana",
                           " un pacchetto al giorno",
                           " a settimana",
                           "€ a settimana",
                           "/10€",
                           "€ a settimana",
                           "almeno ",
                           "Meno di ",
                           " euro a settimana",
                           "Uguale")

# Rimozione delle parole dalla colonna "testo" utilizzando gsub
datamca$Prima_quanto_spendevi__ <- gsub(paste(parole_da_rimuovereee, collapse = "|"), "", datamca$Prima_quanto_spendevi__, ignore.case = TRUE)
datamca$Prima_quanto_spendevi__ <- as.numeric(datamca$Prima_quanto_spendevi__)
table(datamca$Prima_quanto_spendevi__)
#Pulizia dati colonna 21 
parole_da_rimuover3 <- c(" anno",
                           " anni",
                           "/2 anni e mezzo",
                           "/4 anni")

# Rimozione delle parole dalla colonna "testo" utilizzando gsub
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub(paste(parole_da_rimuover3, collapse = "|"), "", datamca$Da_quanto_fumi_sigarette_senza_combustione__, ignore.case = TRUE)
#datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- as.numeric(datamca$Da_quanto_fumi_sigarette_senza_combustione__)
table(datamca$Da_quanto_fumi_sigarette_senza_combustione__)

# Sostituzione del valore "5 mesi" con "0.41"
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("1 mese", as.character("0.08"), datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("10 mesi", "0.83", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("4 mesi", "0.33", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("5 mesi", "0.42", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("6 mesi", "0.50", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("7 mesi", "0.58", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("8 mesi", "0.66", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("9 mesi", "0.75", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("qualche mese", "0.33", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("2 settimane", "0.01", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("2021", "2", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Da quest’estate", "0.83", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Due", "2", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Fumo entrambe", "1", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Poco", "0.33", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Ottobre 2021", "2", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Quasi 2", "2", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("quasi due", "2", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("un", "1", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Un circa", "1", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("un e qualche mese", "1", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Un paio di", "2", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("circa ", "", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub("Ottobre ", "", datamca$Da_quanto_fumi_sigarette_senza_combustione__)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- gsub(" e 0.33", "", datamca$Da_quanto_fumi_sigarette_senza_combustione__)

datamca$Da_quanto_fumi_sigarette_senza_combustione__ <-  as.numeric(datamca$Da_quanto_fumi_sigarette_senza_combustione__)
#Definizione
# Definizione degli intervalli per la categorizzazione
intervalli <- c(0, 5, 10, 15, 20, 25, 30, 35, 40,50)
# Applicazione della categorizzazione utilizzando la funzione cut
datamca$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)` <- cut(datamca$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`, breaks = intervalli)

# Definizione degli intervalli per la categorizzazione
intervalli2 <- c(0, 3, 6, 9,12, 15, 18, 30)
# Applicazione della categorizzazione utilizzando la funzione cut
datamca$`Da quanto tempo fumi ? (scrivere il valore in anni)` <- cut(datamca$`Da quanto tempo fumi ? (scrivere il valore in anni)`, breaks = intervalli2)

intervalli3 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40,50)
datamca$`Quanto spendi in media a settimana per fumare ?` <- cut(datamca$`Quanto spendi in media a settimana per fumare ?`, breaks = intervalli3)

intervalli4 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40,50)
datamca$`Quante sigarette fumi al giorno?` <- cut(datamca$`Quante sigarette fumi al giorno?`, breaks = intervalli4)


intervalli5 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40,50)
datamca$`Quanto spendi a settimana?` <- cut(datamca$`Quanto spendi a settimana?`, breaks = intervalli5)

intervalli6 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40,50)
datamca$`Prima quante sigarette fumavi?` <- cut(datamca$`Prima quante sigarette fumavi?`, breaks = intervalli6)

intervalli7 <- c(0, 5, 10, 15, 20, 25, 30, 35, 40,50)
datamca$Prima_quanto_spendevi__ <- cut(datamca$Prima_quanto_spendevi__, breaks = intervalli7)

intervalli8 <- c(15, 20, 25, 30, 35,40, 45, 50, 55, 60, 65,70, 80)# da rifae la cat ogni 5 
datamca$Età <- cut(datamca$Età, breaks = intervalli8)

intervalli9 <- c(0,0.5,1,3,6,9,12,15)
datamca$Da_quanto_fumi_sigarette_senza_combustione__ <- cut(datamca$Da_quanto_fumi_sigarette_senza_combustione__, breaks = intervalli9)

#####Analisi Totale####
#install.packages("FactoMineR")  # Installa il pacchetto se non è già installato
library(FactoMineR)  # Carica il pacchetto
library("factoextra")
library("corrplot")
#MCA DEL TOTALE DATASET 
c <- MCA(datamca[,-c(1,2,5,29,30)])
summary(c)


#ordine delle variabili per dimensioni 
corrplot(c$var$cos2, is.corr=FALSE)#ordine delle dimensioni 

# Rapp. dimensioni dati 
fviz_eig(c, addlabels = TRUE)#Rapp. Dimensioni 

#Visulizzazione Dimensione 1 e 2 All dataset
fviz_mca_biplot(c, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

#Varibili con piu variazione 
fviz_cos2(c, choice = "var", axes = 1:2)

#Qualità dalle rappresentazioni 
fviz_cos2(c, choice = "var") #Qualita delle rappresentazioni 



fviz_pca_var(c, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlappin
)#Importanza aper cos2 

#Clustering delle varibili 
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(c$var$coord, centers = 3, nstart = 75)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(c, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
#

# Esempio con due dimensioni
plot(c, type = "points", dim = c(5, 6))
  
  
  
  
# Esempio con due dimensioni e una variabile aggiuntiva per il colore
plot(c, type = "points", dim = c(1, 2), color = "variabile_aggiuntiva")
# Esempio con due dimensioni
plot(c, type = "variables", dim = c(1, 2))
# Esempio con le prime 10 dimensioni
plot(c, type = "inertia", dims = 1:10)
# Esempio con due dimensioni e una variabile aggiuntiva per le ellissi
plot(c, type = "ellipses", dim = c(1, 2), color = "variabile_aggiuntiva")
#####MCA Sigarette tradizionali####
#Pulizia dati 
Tradizonalimca <- subset(datamca,`Sei un fumatore? (Se si specificare la tipologia)` == "Sigarette Tradizionali", )
Trad_mca <- Tradizonalimca[,c(1:14,31)]
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calcolo della moda dei valori non mancanti
moda <- Mode(Trad_mca$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`)
# Sostituzione dei valori NA con la moda
Trad_mca$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`[is.na(Trad_mca$`Quante sigarette fumi al giorno ? (esprimere il valore numerico)`)] <- moda

# Calcolo della moda dei valori non mancanti
moda2 <- Mode(Trad_mca$`Da quanto tempo fumi ? (scrivere il valore in anni)`)
# Sostituzione dei valori NA con la moda
Trad_mca$`Da quanto tempo fumi ? (scrivere il valore in anni)`[is.na(Trad_mca$`Da quanto tempo fumi ? (scrivere il valore in anni)`)] <- moda2

# Calcolo della moda dei valori non mancanti
moda3 <- Mode(Trad_mca$`Quanto spendi in media a settimana per fumare ?`)
# Sostituzione dei valori NA con la moda
Trad_mca$`Quanto spendi in media a settimana per fumare ?`[is.na(Trad_mca$`Quanto spendi in media a settimana per fumare ?`)] <- moda3


mcatrad <- MCA(Trad_mca[-c(1,4,56),-c(1,2,5)])
summary(mcatrad)



# Rapp. dimensioni dati 
fviz_eig(mcatrad, addlabels = TRUE)#Rapp. Dimensioni 

#Visulizzazione Dimensione 1 e 2 All dataset
fviz_mca_biplot(mcatrad, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

#Varibili mcatradon piu variazione 
fviz_cos2(mcatrad, choice = "var", axes = 1:2)

#Qualità dalle rappresentazioni 
fviz_cos2(mcatrad, choice = "var") #Qualita delle rappresentazioni 

#ordine delle variabili per dimensioni 
corrplot(mcatrad$var$cos2, is.corr=FALSE)#ordine delle dimensioni 

fviz_pca_var(mcatrad, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlappin
)#Importanza aper cos2 

#Clustering delle varibili 
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(1234)
res.km <- kmeans(mcatrad$var$coord, centers = 6, nstart = 40)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(mcatrad, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF", "#FC4E07", "red", "green"),
             legend.title = "Cluster")
#
plotellipses(mcatrad,keepvar=c(1:6))
plotellipses(mcatrad,keepvar=c(7:12))
# Esempio con due dimensioni
plot(mcatrad, type = "points", dim = c(4, 5))
# Esempio con due dimensioni e una variabile aggiuntiva per il colore
plot(mcatrad, type = "points", dim = c(1, 2), color = "variabile_aggiuntiva")
# Esempio con due dimensioni
plot(mcatrad, type = "variables", dim = c(1, 2))
# Esempio con le prime 10 dimensioni
plot(mcatrad, type = "inertia", dims = 1:10)
# Esempio con due dimensioni e una variabile aggiuntiva per le ellissi
plot(mcatrad, type = "ellipses", dim = c(1, 2), color = "variabile_aggiuntiva")


#####MCA Sigarette senza combustione####
#Pulizia dati 
S_C <-  subset(datamca,`Sei un fumatore? (Se si specificare la tipologia)` == "Sigarette Senza Combustione" )
S_C_c <- S_C[,c(1,2,3,4,5,6,15,16,17,18,19,20,21,22,23,24,31)]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



# Calcolo della moda dei valori non mancanti
moda4 <- Mode(S_C_c$`Quante sigarette fumi al giorno?`)
# Sostituzione dei valori NA con la moda
S_C_c$`Quante sigarette fumi al giorno?`[is.na(S_C_c$`Quante sigarette fumi al giorno?`)] <- moda4

# Calcolo della moda dei valori non mancanti
moda5 <- Mode(S_C_c$`Quanto spendi a settimana?`)
# Sostituzione dei valori NA con la moda
S_C_c$`Quanto spendi a settimana?`[is.na(S_C_c$`Quanto spendi a settimana?`)] <- moda5

# Calcolo della moda dei valori non mancanti
moda6 <- Mode(S_C_c$`Quanto spendi in media a settimana per fumare ?`)
# Sostituzione dei valori NA con la moda
S_C_c$`Quanto spendi in media a settimana per fumare ?`[is.na(S_C_c$`Quanto spendi in media a settimana per fumare ?`)] <- moda6


mcatrad <- MCA(S_C_c[-c(1,4,56),-c(1,2,5)])
summary(mcatrad)

#Visulizzazione Dimensione 1 e 2 All dataset
fviz_mca_biplot(mcatrad, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

# Rapp. dimensioni dati 
fviz_eig(mcatrad, addlabels = TRUE)#Rapp. Dimensioni 

#Varibili mcatradon piu variazione 
fviz_cos2(mcatrad, choice = "var", axes = 1:2)

#Qualità dalle rappresentazioni 
fviz_cos2(mcatrad, choice = "var") #Qualita delle rappresentazioni 

#ordine delle variabili per dimensioni 
corrplot(mcatrad$var$cos2, is.corr=FALSE)#ordine delle dimensioni 

fviz_pca_var(mcatrad, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlappin
)#Importanza aper cos2 

#Clustering delle varibili 
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(mcatrad$var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(mcatrad, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF","#008000"),
             legend.title = "Cluster")
#

# Esempio con due dimensioni
plot(mcatrad, type = "points", dim = c(1, 2))
# Esempio con due dimensioni e una variabile aggiuntiva per il colore
plot(mcatrad, type = "points", dim = c(1, 2), color = "variabile_aggiuntiva")
# Esempio con due dimensioni
plot(mcatrad, type = "variables", dim = c(1, 2))
# Esempio con le prime 10 dimensioni
plot(mcatrad, type = "inertia", dims = 1:10)
# Esempio con due dimensioni e una variabile aggiuntiva per le ellissi
plot(mcatrad, type = "ellipses", dim = c(1, 2), color = "variabile_aggiuntiva")



#####MCA Non fumatori####
#Pulizia dati 
No <- subset(datamca,`Sei un fumatore? (Se si specificare la tipologia)` == "No")
No_C <- No[,-c(7:24)]

mcanon <- MCA(No_C[-c(150),-c(1,2,5,11,12)])
summary(mcanon)

#Visulizzazione Dimensione 1 e 2 All dataset
fviz_mca_biplot(mcanon, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

# Rapp. dimensioni dati 
fviz_eig(mcanon, addlabels = TRUE)#Rapp. Dimensioni 

#Varibili mcatradon piu variazione 
fviz_cos2(mcanon, choice = "var", axes = 1:2)

#Qualità dalle rappresentazioni 
fviz_cos2(mcanon, choice = "var") #Qualita delle rappresentazioni 

#ordine delle variabili per dimensioni 
corrplot(mcanon$var$cos2, is.corr=FALSE)#ordine delle dimensioni 

fviz_pca_var(mcanon, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlappin
)#Importanza aper cos2 

#Clustering delle varibili 
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(mcanon$var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(mcanon, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF","#008000"),
             legend.title = "Cluster")
#

# Esempio con due dimensioni
plot(mcanon, type = "points", dim = c(1, 2))
# Esempio con due dimensioni e una variabile aggiuntiva per il colore
plot(mcanon, type = "points", dim = c(1, 2), color = "variabile_aggiuntiva")
# Esempio con due dimensioni
plot(mcanon, type = "variables", dim = c(1, 2))
# Esempio con le prime 10 dimensioni
plot(mcanon, type = "inertia", dims = 1:10)
# Esempio con due dimensioni e una variabile aggiuntiva per le ellissi
plot(mcanon, type = "ellipses", dim = c(1, 2), color = "variabile_aggiuntiva")


