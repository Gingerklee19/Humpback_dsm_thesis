# Humpback_dsm_thesis
This repository contains code and supporting materials for my Master's thesis in Marine and Antarctic Science.   The project applies **distance sampling** and **density surface modeling (DSM)** to estimate humpback whale (*Megaptera novaeangliae*) abundance and distribution in the Gerlache Strait  
The repository contains scripts for detection function, dsm, grid projection and abundance plot code. Main thesis figures are included and supplementary materials

## Repository Contents
- **Scripts/** – R scripts for data cleaning, detection functions, DSMs, and visualisation  
- **Results/** – Thesis figures
- **Supplementary/** – supplementary Materials to support results and methods 
 

## Methods Summary
The analysis follows 10 main stages:
1. Data preparation and quality control of survey effort and sightings  
2. Detection function fitting (`Distance`, `mrds`)  
3. DSM fitting (`dsm`, `mgcv`)
4. grid projection
5. Abundance prediction across spatial grids and with added temporal covariates
6. Grid reprojection
7. Reprojection of dsm models
8. Adding environmental covariates
9. Creating environmental dsms
10. Abundance predictions across spatial grids with temporal covariates and added environmental covariates

## Requirements
Developed in **R (≥ 4.3)**.  
Main packages:
```r
install.packages(c("Distance", "dsm", "ggplot2", "sf", "mgcv", "dplyr"))
