## Repo used for the Data Visualization Course ##

__Dataset__ : tus_00age from the [Time Use Surveys](https://ec.europa.eu/eurostat/web/time-use-surveys/data/database) present on [Eurostat](https://ec.europa.eu/eurostat/web/main/home)

__Dataset Description__:
Dataset contains the amount of time people spend doing certain activities.   
Data is grouped in two batches:  
- year 2000 batch, containing data between _1998_ and _2006_ for 15 european countries
- year 2010 batch, containing data between _2008_ and _2015_ for 18 european countries
  
Data is on individuals aged _15_ to _74_, of _Male_ and _Female_ sex, tracking _56_ __main__ activities

## Project description 
The project is web page containing a few findings about the data about and an interactive map where data can be seen and compared more easily for the countries that participated in the survey 

## What you need for the project

### Files:
- app.R
- Data.R
- Misc.R
- static.Rmd
- period_2000_rearranged.csv
- period_2010_rearranged.csv
- countries.json
- CodeLists/ESTAT_ACL00_en.tsv
- CodeLists/ESTAT_AGE_en.tsv
- CodeLists/ESTAT_SEX_en.tsv
- countries.geojson
### R libraries:
- ggplot2
- rjson
- tidyverse
- dplyr
- shiny
- leaflet
- geojsonio
- sp
- tidyr
- lubridate
- shinyjs
- rmarkdown
- knitr
### How to run:
 Simply start app.R 