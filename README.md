# Inventory Forestier - Data Analysis

Welcome to the Forest Inventory Data Analysis repository! This project focuses on analyzing and processing forest inventory data using R. Below is an overview of the code and its functionalities.

## Table of Contents

1. [Initiating](#initiating)
2. [Calculating Variables](#calculating-variables)
3. [Tranches Histograms](#tranches-histograms)
4. [Statistical Analysis](#statistical-analysis)
5. [Exporting Results](#exporting-results)

## 1. Initiating

- **Clear Memory**: Removes all existing variables.
- **Load Libraries**: Loads necessary R libraries.
- **Set Working Directory**: Defines the working directory.
- **Import Data**: Reads data from the Excel file.

## 2. Calculating Variables

Various variables are calculated based on the forest inventory data, including:
- Grouping trees based on inventory method.
- Creating a species column by combining "Genre" and "EPITHETE."
- Calculating tree height based on diameter and altitude.
- Computing tree volume, basal area, wood density, and biomass.

## 3. Tranches Histograms

Histograms are created to visualize tree diameters in different inventory groups ("Transect" and "Placeau"). The histograms are faceted by specific methods within each group.

## 4. Statistical Analysis

Statistical analyses include:
- ANOVA for the number of species and individuals per group.
- Creating lists of unique species and genres.
- Aggregating data based on inventory methods.
- Calculating density of genres and species per hectare.
- Computing mean variables per hectare.

## 5. Exporting Results

Results are exported to an Excel file named "myData.xlsx." The exported sheets include raw data, aggregated data, density information, mean variables, presence/absence table, and occurrence table.

Feel free to explore, modify, and contribute to this project! For more details, refer to the code documentation and comments.

**Note:** Ensure that the required libraries are installed before running the code. You can install them using the `install.packages()` function in R.

Happy analyzing! 🌲📊

René-Jubilé MABILA (@mabilarene)
