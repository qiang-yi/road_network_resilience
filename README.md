# Resilience assessment for road network

This repository stores programs for data collection and analysis for empirical assessment of road network resilience in natural hazards. The methodology and results of the case study are documented in the manuscript "_Empirical assessment of road network resilience in natural hazards_" accepted by International Journal of Geographical Information Science.

## Data collection
This folder contains Python programs for collecting real-time and benchmark travel times from Google Maps API. Specifically:
- **searc_nearby_facilities.py**: search for nearby facilities around each spatial units (e.g. census tract). Coordinates of the nearest n facilities will be saved in the input csv file.

- **realtime_accessibility_clv.py**: stream the real time travel time from origin to destinations. It calls the 'direction' function in dur_in_traffic.py to retrieve the O-D travel time from Google Maps Direction API.

- **dur_in_traffic.py**: stores the 'direction' function, which will be called in realtime_accessibility_clv.py.

## Data analyses
This folder contains R programs for pre-processing, statistical analysis, plotting and mapping using the acquired travel time data. Specifically:

- **load_data.R**: reads the collected data of travel and of O-D pairs and preprocesses the data.

- **table_processing.R**:
  - Combines the tables of data collected at different times into a single table
  - Converts the time to UTC
  - Join the normal travel times with the real-time travel times
  - all_data.csv contains the processed data at this step.

- **analysis.R**:
  - conducting statistical analysis and creating plots about the relationship between travel times and other variables

- **overall_accessibility.R**
  - Aggregates travel times of O-D pairs into Hansen accessibility index
  - Create plots and maps for the accessibility index
  - Calculate resilience from accumulated accessibility
