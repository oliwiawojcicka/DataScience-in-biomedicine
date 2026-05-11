clean_patient_data <- function(df) {
  df %>%
    mutate(
      patient_id = as.character(Patient_ID),
      
      date_of_birth = suppressWarnings(
        parse_date_time(
          Date_of_Birth,
          orders = c("d-m-Y", "Y/m/d", "Y-m-d", "m/d/Y", "d/m/Y")
        )
      ),
      date_of_birth = as.Date(date_of_birth),
      
      age = suppressWarnings(as.integer(Age)),
      
      sex = case_when(
        str_to_lower(str_trim(as.character(Sex))) %in% c("m", "male") ~ "Male",
        str_to_lower(str_trim(as.character(Sex))) %in% c("f", "female") ~ "Female",
        TRUE ~ NA_character_
      ),
      
      weight = suppressWarnings(
        as.numeric(str_replace_all(as.character(Weight), "[^0-9.]", ""))
      ),
      
      height = suppressWarnings(
        as.numeric(str_replace_all(as.character(Height), "[^0-9.]", ""))
      ),
      height = ifelse(!is.na(height) & height > 3, height / 100, height),
      
      blood_type = case_when(
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("a+", "a positive", "apositive") ~ "A+",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("a-", "a negative", "anegative") ~ "A-",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("b+", "b positive", "bpositive") ~ "B+",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("b-", "b negative", "bnegative") ~ "B-",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("ab+", "ab positive", "abpositive") ~ "AB+",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("ab-", "ab negative", "abnegative") ~ "AB-",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("o+", "o positive", "opositive") ~ "O+",
        str_to_lower(str_trim(as.character(Blood_Type))) %in% c("o-", "o negative", "onegative") ~ "O-",
        TRUE ~ NA_character_
      ),
      
      diagnosis_code = as.character(Diagnosis_Code),
      
      dosage_mg = suppressWarnings(as.integer(Dosage_mg)),
      
      smoker = case_when(
        str_to_lower(str_trim(as.character(Smoker))) %in% c("yes", "y", "true", "1", "smoker") ~ TRUE,
        str_to_lower(str_trim(as.character(Smoker))) %in% c("no", "n", "false", "0", "non-smoker", "nonsmoker") ~ FALSE,
        TRUE ~ NA
      ),
      
      doctor_name = as.character(Doctor_Name)
    ) %>%
    select(
      patient_id,
      date_of_birth,
      age,
      sex,
      weight,
      height,
      blood_type,
      diagnosis_code,
      dosage_mg,
      smoker,
      doctor_name
    )
}


completeness_metrics <- function(df) {
  data.frame(
    variable = names(df),
    missing_count = sapply(df, function(x) sum(is.na(x))),
    missing_percent = round(sapply(df, function(x) mean(is.na(x)) * 100), 2),
    row.names = NULL
  )
}


quality_flags <- function(df) {
  df %>%
    mutate(
      invalid_patient_id = is.na(patient_id) | !str_detect(patient_id, "^P-?[0-9]{4,}"),
      
      invalid_age = is.na(age) | age < 0 | age > 120,
      
      invalid_dob = is.na(date_of_birth) | date_of_birth > Sys.Date(),
      
      invalid_sex = is.na(sex) | !(sex %in% c("Male", "Female")),
      
      invalid_weight = is.na(weight) | weight < 2 | weight > 300,
      
      invalid_height = is.na(height) | height < 0.5 | height > 2.5,
      
      invalid_blood_type = is.na(blood_type) |
        !(blood_type %in% c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")),
      
      missing_diagnosis = is.na(diagnosis_code) | diagnosis_code == "",
      
      invalid_dosage = is.na(dosage_mg) | dosage_mg <= 0 | dosage_mg > 5000,
      
      calculated_age = ifelse(
        !is.na(date_of_birth),
        as.integer(floor(time_length(interval(date_of_birth, Sys.Date()), "years"))),
        NA_integer_
      ),
      
      age_dob_mismatch = !is.na(calculated_age) & !is.na(age) &
        abs(calculated_age - age) > 2,
      
      has_problem = invalid_patient_id |
        invalid_age |
        invalid_dob |
        invalid_sex |
        invalid_weight |
        invalid_height |
        invalid_blood_type |
        missing_diagnosis |
        invalid_dosage |
        age_dob_mismatch
    )
}


validate_new_patient <- function(patient, existing_ids) {
  errors <- c()
  
  if (is.na(patient$patient_id) || patient$patient_id == "" ||
      !str_detect(patient$patient_id, "^P-?[0-9]{4,}$")) {
    errors <- c(errors, "Patient ID must have a valid format, for example P-1001 or P1001.")
  }
  
  if (!is.na(patient$patient_id) && patient$patient_id %in% existing_ids) {
    errors <- c(errors, "A patient with this ID already exists in the database.")
  }
  
  if (is.na(patient$date_of_birth)) {
    errors <- c(errors, "Date of birth is required.")
  } else if (patient$date_of_birth > Sys.Date()) {
    errors <- c(errors, "Date of birth cannot be in the future.")
  }
  
  if (is.na(patient$age) || patient$age < 0 || patient$age > 120) {
    errors <- c(errors, "Age must be between 0 and 120.")
  }
  
  if (is.na(patient$sex) || !(patient$sex %in% c("Male", "Female"))) {
    errors <- c(errors, "Sex must be either Male or Female.")
  }
  
  if (is.na(patient$weight) || patient$weight < 2 || patient$weight > 300) {
    errors <- c(errors, "Weight must be between 2 and 300 kg.")
  }
  
  if (is.na(patient$height) || patient$height < 0.5 || patient$height > 2.5) {
    errors <- c(errors, "Height must be between 0.5 and 2.5 meters.")
  }
  
  if (is.na(patient$blood_type) ||
      !(patient$blood_type %in% c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"))) {
    errors <- c(errors, "Blood type must be one of: A+, A-, B+, B-, AB+, AB-, O+, O-.")
  }
  
  if (is.na(patient$diagnosis_code) || patient$diagnosis_code == "") {
    errors <- c(errors, "Diagnosis code is required.")
  }
  
  if (is.na(patient$dosage_mg) || patient$dosage_mg <= 0 || patient$dosage_mg > 5000) {
    errors <- c(errors, "Dosage must be greater than 0 and not greater than 5000 mg.")
  }
  
  errors
}
