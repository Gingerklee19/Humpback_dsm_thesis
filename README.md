# Humpback_dsm_thesis
This repository contains code and supporting materials for my Master's thesis in Marine and Antarctic Science.   The project applies **distance sampling** and **density surface modeling (DSM)** to estimate humpback whale (*Megaptera novaeangliae*) abundance and distribution in the Gerlache Strait  
Original data is not included due to restrictions, but the repository contains scripts, workflow documentation, and example outputs.

## Repository Contents
- **scripts/** – R scripts for data cleaning, detection functions, DSMs, and visualisation  
- **results/** – figures and model summaries  
- **docs/** – extended methods, results, and project notes  
- **data/** – placeholder folder for input data (not included)  

## Methods Summary
The analysis follows five main stages:
1. Data preparation and quality control of survey effort and sightings  
2. Detection function fitting (`Distance`, `mrds`)  
3. DSM fitting with environmental covariates (`dsm`, `mgcv`)  
4. Abundance prediction across spatial grids  
5. Visualisation of seasonal distribution and hotspots  

## Requirements
Developed in **R (≥ 4.3)**.  
Main packages:
```r
install.packages(c("Distance", "dsm", "ggplot2", "sf", "mgcv", "dplyr"))
