library(jsonlite)
library(mongolite)
library(lubridate)

mongo_conn <- mongo(
  collection = "provenance_logs",
  db = "genomics",
  url = "mongodb://localhost:27017"
)

mongo_conn$drop()

json_files <- list.files(
  path = "data/json",
  pattern = "\\.json$",
  full.names = TRUE
)

for (file in json_files) {
  
  record <- fromJSON(file, simplifyVector = FALSE)
  
  sha_value <- record$generated[[1]]$value
  seqfu_value <- record$generated[[2]]$value
  fastq_info <- record$generated[[3]]
  
  start_time <- ymd_hms(record$startTime)
  end_time <- ymd_hms(record$endTime)
  duration_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  record$record_id <- record$`@id`
  record$durationMinutes <- round(duration_minutes, 2)
  record$sha256_status <- ifelse(grepl("ERROR", sha_value), "FAILED", "OK")
  record$seqfu_status <- ifelse(grepl("ERROR", seqfu_value), "FAILED", "OK")
  record$totalSizeGB <- round(as.numeric(fastq_info$totalSizeBytes) / 1024^3, 2)
  record$fileCount <- as.numeric(fastq_info$fileCount)
  record$category <- fastq_info$category
  record$sourceFile <- file
  
  mongo_conn$insert(record)
}

cat("Imported", length(json_files), "JSON files into MongoDB.\n")
cat("MongoDB records count:", mongo_conn$count(), "\n")