# Decarbonization Scenarios

This repository contains data pipelines, models, and a Shiny application for exploring decarbonization scenarios using utility-scale and public energy datasets. The focus is on reproducible analysis, transparent assumptions, and scenario-based exploration.

---

## Project Structure
decarbonization_scenarios/
* setup/ # Dependency management and environment setup
* etl/ # Data ingestion, cleaning, and transformation
* app/ # Shiny application
  * global.R # Global options, libraries, helper functions, preprocessing
  * server.R # Server logic
  * ui.R # User interface
* data/ # Raw and processed datasets (gitignored as appropriate)
* models/ # Modeling code and scenario definitions
* output/ # Tables, figures, and exported results
* README.md


---

## Getting Started

### Prerequisites
- R (≥ 4.x)
- RStudio
- RSConnect account (for deployment)

### Dependency Management
This project uses a dependency management tool compatible with RSConnect (e.g. `packrat` or `renv`) to ensure reproducibility across local and deployed environments.

Initialize dependencies:
```r
# Example
packrat::init()

setwd("app")
shiny::runApp()
```

---

## Deployment

The application is intended to be deployed via RSConnect. All package dependencies must be captured prior to deployment.

---

## Branching & Workflow

main – stable, deployable code

dev – active development

feature branches – scoped work (ETL, modeling, UI changes)

---

## Notes

Large datasets should not be committed to GitHub.

All transformations should be reproducible from raw inputs.

Modeling assumptions and scenario definitions should be documented inline or in /models.
