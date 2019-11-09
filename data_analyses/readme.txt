load_data.R: 
# reads the collected data of travel and of O-D pairs and preprocesses the data.

table_processing.R: 
# Combines the tables of data collected at different times into a single table
# Converts the time to UTC
# Join the normal travel times with the real-time travel times
# all_data.csv contains the processed data at this step.

analysis.R
# conducting statistical analysis and creating plots about the relationship between travel times and other variables

overall_accessibility.R
# Aggregates travel times of O-D pairs into Hansen accessibility index
# Create plots and maps for the accessibility index
# Calculate resilience from accumulated accessibility
