# B.A. Economics Thesis: The Economic Cycle and Inequality - An Empirical Study for Mexican Households

This repository has two modules whose aim are:

* `time-series-preprocessing.R`: 

   A module that extracts time-series data from:
     - Mexican Central Bank (https://www.banxico.org.mx/SieInternet/)
     - National Household Income and Expenditure Survery (https://www.inegi.org.mx/programas/enigh/nc/2020/)
     - National Economic Datawarehouse: https://www.inegi.org.mx/sistemas/bie/
   
   After extracting the data via API, the data is concatenated for all available years for mexican households in order to transform raw-data to equal format time-series
   for every data source in order to get a final dataset where it would be possible to follow mexican household features for each mexican household across time, in a period that    goes from 1984 to 2020.  
   
     
* `economic-cycles-analysis.R`:

