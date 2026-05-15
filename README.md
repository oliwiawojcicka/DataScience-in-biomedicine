# Data Science Applied to Biomedicine

This repository contains projects completed as part of the **Data Science Applied to Biomedicine** course, which I am taking during my Erasmus+ exchange semester in Spain.

The course focuses on applying data science methods to biomedical and clinical data, including data preprocessing, visualization, quality control, database integration, and building interactive applications for biomedical data management.

## Biomedical Shiny Application

The main project in this repository is a biomedical data management application built in **R Shiny**.

The aim of the project was to create an interactive application based on hospital admission data. The original dataset was provided as a CSV file, but instead of using the file directly throughout the workflow, the data was migrated into a **PostgreSQL database**. This made the data persistent and allowed the application to work more like a real biomedical information system.

The application includes patient data management, data visualization, quality control checks, validated forms for adding new records, audit logs, and report generation.

## Project Structure

The project was implemented using a modular structure, with the main parts of the application separated into different files. This makes the code easier to maintain, read, and extend.

The repository includes:

- `biomed_shiny_project/` – main Shiny application files,
- `report-2.pdf` – project report describing the biomedical Shiny application.

The remaining files in this repository are smaller tasks and assignments completed during the semester as part of the course.



