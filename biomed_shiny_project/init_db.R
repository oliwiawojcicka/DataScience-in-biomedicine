library(DBI)
library(RPostgres)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

source("R/quality_functions.R")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME", "biomed_db"),
  host = Sys.getenv("DB_HOST", "localhost"),
  port = as.integer(Sys.getenv("DB_PORT", "5432")),
  user = Sys.getenv("DB_USER", "biomed_user"),
  password = Sys.getenv("DB_PASSWORD", "biomed_pass")
)

# Czyścimy stare tabele, żeby baza była tworzona od zera
dbExecute(con, "DROP TABLE IF EXISTS audit_logs;")
dbExecute(con, "DROP TABLE IF EXISTS patients;")

# Tworzymy tabelę pacjentów
dbExecute(con, "
CREATE TABLE patients (
  patient_id VARCHAR(30) PRIMARY KEY,
  date_of_birth DATE,
  age INTEGER,
  sex VARCHAR(20),
  weight NUMERIC,
  height NUMERIC,
  blood_type VARCHAR(20),
  diagnosis_code VARCHAR(30),
  dosage_mg INTEGER,
  smoker BOOLEAN,
  doctor_name VARCHAR(100),
  created_by VARCHAR(50) DEFAULT 'import',
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  modified_by VARCHAR(50),
  modified_at TIMESTAMP
);
")

# Tworzymy tabelę logów audytu
dbExecute(con, "
CREATE TABLE audit_logs (
  log_id SERIAL PRIMARY KEY,
  patient_id VARCHAR(30),
  action VARCHAR(20),
  username VARCHAR(50),
  action_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  old_value TEXT,
  new_value TEXT
);
")

# Wczytujemy dane z CSV
raw_data <- read_csv("data/Hospital_Admissions_ULTIMATE.csv", show_col_types = FALSE)

# Czyścimy dane funkcją z pliku R/quality_functions.R
clean_data <- clean_patient_data(raw_data)

# Poprawiamy ewentualne duplikaty ID, aby import nie przerwał się przez PRIMARY KEY
clean_data <- clean_data %>%
  group_by(patient_id) %>%
  mutate(
    patient_id = ifelse(
      row_number() == 1,
      patient_id,
      paste0(patient_id, "_DUP", row_number())
    )
  ) %>%
  ungroup() %>%
  mutate(
    created_by = "csv_import",
    created_at = Sys.time(),
    modified_by = NA_character_,
    modified_at = as.POSIXct(NA)
  )

# Import danych do tabeli patients
dbWriteTable(con, "patients", clean_data, append = TRUE, row.names = FALSE)

# Dodajemy log importu dla każdego pacjenta
dbExecute(con, "
INSERT INTO audit_logs (patient_id, action, username, old_value, new_value)
SELECT patient_id, 'IMPORT', 'csv_import', NULL, 'Imported from CSV'
FROM patients;
")

cat("Baza danych została przygotowana, a CSV zaimportowany.\n")

dbDisconnect(con)
