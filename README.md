# Rodent-borne disease Machine Learning

## Project Overview
Many rodent-borne diseases show seasonal patterns of infection, likely driven by climate factors impacting the population dynamics of the rodent host or hosts. Using machine learning approaches, we plan to classify seasonal curves in human infection of rodent-borne diseases to see how predict years of high or low outbreak potential.

Diseases Included:
- Hantavirus (HPS and HFRS)
- Lassa fever
- Bolivian hemorrhagic fever

### Datasets
1. Human Case data
2. Climate data 
3. Rodent host data 

## Getting Started

### Code overview

_Preprocessing_/:
1. `01_ProcessCaseData.R`: aligns case data to same format and adds geo information
2. `02_ProcessHostData.R`: formats host distribution and aligns with case data geo information
3. `03_GetClimateData.ipynb`: pulls and summarizes climate data from Google Earth Engine
4. `04_CreateViz.R`: creates summary figures for data
5. `05_FormatData.py`

_Model Code_

model/ directory

_Postprocessing_

