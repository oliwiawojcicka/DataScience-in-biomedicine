# Biomedical Shiny Application

This project is a biomedical data management application built with R Shiny and PostgreSQL.

## Features

- CSV data import into PostgreSQL
- Modular Shiny application
- Patient record browsing, filtering, editing and deleting
- Data quality dashboard
- Completeness, consistency and accuracy checks
- Defensive validation for new patient records
- Audit logs for traceability
- HTML report generation using RMarkdown

## Technologies

- R Shiny
- PostgreSQL
- RMarkdown
- Docker

## How to run

1. Start PostgreSQL using Docker Compose:

```bash
docker compose up -d
