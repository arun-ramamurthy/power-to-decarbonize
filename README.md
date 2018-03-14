Author: Arun Ramamurthy  
Organization: Environmental Progress  
Release Date: 2017.11.15  

This is the associated data, scripts, plots, and analysis documents for Environmental Progress' "Power to Decarbonize" report, which evaluates the historic effectiveness of governmental policies for decarbonization, especially in consideration to various electricity technologies. 

The full report is contained in this repository, but can also be viewed on the [EP website](http://environmentalprogress.org/big-news/2017/11/7/the-power-to-decarbonize). In November 2017, this report was the subject of Eduardo Porter's New York Times piece, ["Wind and Solar Power Advance, but Carbon Refuses to Retreat"](https://www.nytimes.com/2017/11/07/business/climate-carbon-renewables.html).

All scripts were written by Arun Ramamurthy in Summer/Fall 2017, and the cleaned data was extracted from publicly available datasets. 

## Directory 1: SCRIPTS
    reader.R - contains full, commented script to compile and clean BP and global BP datasets directly from sources
## Directory 2: DATA
    worldbank - contains relevant data directly from WorldBank DataBank
    taiwan-population.csv - Time series of Taiwan's population
    lru-nuclear.csv - Time series of nuclear generation data from Lithuania, Russia, and Ukraine prior to 1985
    bp.csv, BP - National BP dataset used in "Power to Decarbonize" report in both CSV and RDS formats
    bp_global.csv, BP_GLOBAL - Global BP dataset in both CSV and RDS formats
## Directory 3: PLOTS
    (All the plots contained in the "Power to Decarbonize" report)
## Directory 4: ANALYSIS
    model_response_simple - evaluates the validity of the linear regression model as applied to the relationships characterized in the "Power to Decarbonize" report
