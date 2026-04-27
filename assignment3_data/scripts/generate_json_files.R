library(jsonlite)

dir.create("data/json", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

nodes <- c("cresselia", "node01", "node02", "node03", "node04")
users <- c("salle_alumni", "student_01", "student_02", "bio_user")
categories <- c("Genet", "Cancer", "RNAseq", "WGS")

for (i in 1:100) {
  
  sample_id <- paste0("SAMPLE_", sprintf("%03d", i))
  
  start_time <- as.POSIXct("2026-03-31 08:00:00", tz = "UTC") + sample(1:50000, 1)
  duration_seconds <- sample(120:1800, 1)
  end_time <- start_time + duration_seconds
  
  node <- sample(nodes, 1)
  user <- sample(users, 1)
  
  sha_ok <- sample(c(TRUE, FALSE), 1, prob = c(0.9, 0.1))
  seqfu_ok <- sample(c(TRUE, FALSE), 1, prob = c(0.92, 0.08))
  
  size_bytes <- sample(1000000000:8000000000, 1)
  file_count <- sample(2:6, 1)
  
  sha_value <- if (sha_ok) {
    paste0(sample_id, "_R1.fastq.gz: La suma coincide ",
           sample_id, "_R2.fastq.gz: La suma coincide")
  } else {
    paste0(sample_id, "_R1.fastq.gz: ERROR checksum mismatch")
  }
  
  seqfu_value <- if (seqfu_ok) {
    paste0("OK PE ", sample_id, "_R1.fastq.gz 0 0 0")
  } else {
    paste0("ERROR FASTQ format problem in ", sample_id)
  }
  
  record <- list(
    "@context" = "http://www.w3.org/ns/prov#",
    "@id" = paste0("urn:uuid:", i),
    "@type" = "Activity",
    label = paste0("Complete processing of ", sample_id),
    startTime = format(start_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    endTime = format(end_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    executionNode = node,
    sourceDirectory = "/data/input/",
    destinationDirectory = paste0("/data/output/", sample_id),
    wasAssociatedWith = list(
      list(
        "@type" = "SoftwareAgent",
        label = "seqfu",
        version = "1.22.3"
      ),
      list(
        "@type" = "SoftwareAgent",
        label = "sha256sum",
        version = "sha256sum (GNU coreutils) 8.32"
      ),
      list(
        "@type" = "SoftwareAgent",
        label = "Pipeline Nextflow fastq_prov",
        repository = "local",
        commitId = "N/A",
        revision = "N/A"
      ),
      list(
        "@id" = paste0("urn:person:", user),
        "@type" = "Person",
        label = paste0("Executor user: ", user),
        actedOnBehalfOf = list(
          "@id" = "https://ror.org/01y990p52",
          "@type" = "Organization",
          label = "La Salle"
        )
      )
    ),
    generated = list(
      list(
        "@type" = "Entity",
        label = "Verificació SHA256",
        description = "Checksum verification result at destination",
        value = sha_value
      ),
      list(
        "@type" = "Entity",
        label = "Verificació Seqfu",
        description = "FASTQ format integrity check result",
        value = seqfu_value
      ),
      list(
        "@type" = "Entity",
        label = "FASTQ Files",
        totalSizeBytes = as.character(size_bytes),
        category = sample(categories, 1),
        fileCount = as.character(file_count)
      )
    )
  )
  
  write_json(
    record,
    path = paste0("data/json/provenance_", sprintf("%03d", i), ".json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )
}

cat("Generated 100 JSON files in data/json\n")