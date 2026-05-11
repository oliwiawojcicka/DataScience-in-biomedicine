create_db_pool <- function() {
  dbPool(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME", "biomed_db"),
    host = Sys.getenv("DB_HOST", "localhost"),
    port = as.integer(Sys.getenv("DB_PORT", "5432")),
    user = Sys.getenv("DB_USER", "biomed_user"),
    password = Sys.getenv("DB_PASSWORD", "biomed_pass")
  )
}

get_patients <- function(pool) {
  DBI::dbReadTable(pool, "patients")
}

write_audit_log <- function(pool, patient_id, action, username, old_value = NA, new_value = NA) {
  DBI::dbExecute(
    pool,
    "INSERT INTO audit_logs (patient_id, action, username, old_value, new_value)
     VALUES ($1, $2, $3, $4, $5)",
    params = list(patient_id, action, username, old_value, new_value)
  )
}
