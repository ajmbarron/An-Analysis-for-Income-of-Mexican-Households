# B.A. Economics Thesis: The Economic Cycle and Inequality - An Empirical Study for Mexican Households

**Context**: A repository to extract, transform and load data from external sources with the aim to answer the hyphotesis, does there exist an homogenous procyclic relationship impact between income and inequality for all Mexican households? 

This repository has two modules:

* `time-series-preprocessing.R`: 

   A module that extracts time-series data from:
     - Mexican Central Bank (https://www.banxico.org.mx/SieInternet/)
     - National Household Income and Expenditure Survery (https://www.inegi.org.mx/programas/enigh/nc/2020/)
     - National Economic Datawarehouse: https://www.inegi.org.mx/sistemas/bie/
   
   After extracting the data via API, the data is concatenated for all available years for mexican households in order to transform raw-data to equal format time-series
   for every data source in order to get a final dataset where it would be possible to follow mexican household features for each mexican household across time, in a period that goes from 1984 to 2020.  
   
     
* `economic-cycles-analysis.R`:

To measure household income, per-capita income was used; whilst to measure inequality Atkinson Index was developed. 
A cross-sectional panel database, where `t=1984,...,2020` and `i=I,II,...X` where `i` was formulated as population deciles. 

By using Kolmogorv-Smirnov Test it was found that the income-cycle followed a Cauchy distribution, as there wasn't evidence to reject the null hypthosis, were both samples came from same distribution. In order to fit Cauchy parameters the next methodologies were tried:

* Maximum Likelihood
* Nelder-Meade Algoritm
* Newton-Raphson Algorithm

As Nelder-Meade and Newton-Raphson algorithms never converged, maximum-likelihood method was preferred.

As series had a non-normal behavior it was necessary to extract the time-series noise, he cycle was developed for both indicatores following the next methodologies:

* Hodrick-Prescott
* Trigonometric Regression
* Baxter-King
* Differences
* Christiano-Fitzgerald

It was found that the cycle with least oscillations per time-period was the one that followed a Baxter-King methodology, therefore, by following this methodology pearson correlation coefficients were computed in order to know the intrinsec linear relationship between inequality and income across time for mexican-households, as both series had a non-normal behavior and following central limit theorem, bootstrapped samples were used  in order to calibrate robustness for Kolmogorv-Smirnov statistic, after this the statistic was found robust. 

It was found that the income and inequality cycles have a procyclic relationship, meaning that when household income increases the country inequality increases. 













#

**Bibliography**

   - Atkinson A. (1970). On the Measurement of Inequality. Journal of Economic
   Theory 2.pp. 244-263.

   - Auclert A. y Rognlie M. (2018). Inequality and Aggregate Demand. NBER
   Working Paper (No. 24280).

   - Bahadur R. (1967). An optimal property of the likelihood ratio statistic. Berkeley
   Symposium on Mathematical Statistics and Probability, University of California
   Press. Vol. 1. pp. 13-26.

   - Bai Z. y Fu. J. (1987). On the maximum-likelihood estimator for the location
   parameter of a Cauchy distribution. Canadian Journal of Statistics. Vol. 15. pp.
   137-146

   - Baxter M. y King R. (1995). Meausring Business Cycles Approximate Band-Pass
   Filters for Economic Time Series. NBER. Working Paper (No. 5022).
   33
   - Benhabib J., Bisin A. y Luo M. (2019). Wealth distribution and social mobility in
   the US: A quantitative approach. American Economic Review. 109 (5), pp. 1623-47.

   - Burns A. y Mitchell W. (1946.) Measuring Business Cycles. NBER. pp.56-114.

   - Christiano L. y Fitzgerald T. (1999). The Band Pass Filter. NBER. Working Paper
   (No. 7527).

   - Costas A. y Smith B. (1998). Finacial Intermediation and regime switching in
   business cycles. The American Economic Review 88, pp. 516-536.

   - Deaton A. (2016). Measuring and understanding Behavior, Welfare and Poverty.
   American Economic Review. Vol. 106, No.6, pp.1221-1243.

   - Debraj R. (2002). Economía del Desarrollo. Antonio Bosch Editor.

   - Fawaz, F., Rahnamamoghadam, M., & Valcarcel, V. (2012). Fluctuations,
   Uncertainty and Income Inequality in Developing Countries. Eastern Economic
   Journal, 38(4), 495-511.

   - Friedman. M. (1993). The “plucking model” of business fluctuations revisited.
   Economic Inquiry 31, pp.171-177.

   - Gabaix X., Lasry J., Lions P. y Moll B. (2016). The dynamics of inequality.
   Econometrica. Vol. 84, No.6, pp. 2071-2111.

   - Hicks J. R. (1937). Mr. Keynes and the “Classics”; A Suggested Interpretation.
   Econometrica. Vol. 5, No. 2, pp. 147.159.

   - Hodrick R. y Prescott E. (1997). U.S. Business Cycles: An Empircal Investigation.
   Journal of Money, Credit and Banking. Vol.29, No.1, pp. 1-16.

   - Instituto Nacional de Geografìa y Estadistica (INEGI). (2019). Producto Interno
   Bruto a Precios de Mercado. Banco de Información Económica. Sistema de Cuentas
   Nacionales.

   - Kim C. y Nelson C. (1999). Friedman´s plucking model of business fluctuations
   tests and estimates of permanent and transitory component. Journal of Money,
   Credit and Banking 31, pp. 317-334.

   - Krueger D., Mitman K., Perri F. (2018). Macroeconomics and Household
   Heterogeneity.NBER Working Paper (No. 22319).

   - Kydland F. y Prescott E. (1998). Business Cycles: Real Facts and a Monetary Myth.
   Quarterly Review from Federal Reserve Bank of Minneapolis, p.3.
   34

   - Kolmogorv A. (1933). Sulla determinazione empírica di una legge di distribuzione.
   G. Ist. Ital. Attuari. Vol.4. pp. 83-91

   - Kuznets, Simon (1955). Economic Growth and Income. The American Economic
   Review, Vol. 45, No. 1. (Mar., 1955), pp. 1-28.

   - Lucas R. (1977). Understanding business cycle. Carnegie-Rochester Series on
   Public Policy 5, 7-29.

   - Lucas R. (2003). Macroeconomic Priorities. The American Economic Review.
   Volume 93, No. 1, pp. 1-14.

   - Nelder J. y Mead R. (1965). A Simplex Method for Function Minimization. The
   Computer Journal. Vol. 7. pp. 308-313.

   - Newey W. y West K. A Simple, Positive Semi-Definite, Heteroskedasticity and
   Autocorrelation Consistent Covariance Matrix. (1987). Econometrica. Vol. 55,
   No.3, pp. 703-708.

   - Rothenberg T., Fisher F., Tilanus C.B. (1964). A Note on Estimation from a Cauchy
   Sample. Journal of the American Statistical Association. Volume 59, pp. 460-463.

   - Said E. y Dickey D. (1984). Testing for unit roots in autoregressive-moving average
   models of unkwnow order. Biometrika. Vol.71, Issue 3, pp. 599-607.

   - Schultze C. (1964). Short-run movements of income shares. The Behavior of
   Income Shares: Selected Theoretical and Empirical Issues. NBER.

   - Sichel D. (1993). Business cycle asymmetry: a deeper look. Economic Inquiry 31,
   224-236.

   - Smirnov N. (1948). Table for estimating the goodness of fit of empirical
   distributions. Annals of Mathematical Statistics. Vol 19. pp. 279-281.

   - Solow R. (1956). A Contribution to the Theory of Economic Growth. The Quarterly
   Journal of Economics¸Vol. 70, No.1, pp. 65-94. 
