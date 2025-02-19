# Quantitative Analysis of Airbnb Prices in Tokyo, Japan

This project explores the impact of local attractions on Airbnb rental prices in Tokyo, Japan. Using geospatial data, we built a regression model to quantify the relationship between Airbnb rental prices and the proximity to major tourist spots in the city. The analysis incorporates control variables such as property features and review ratings to better understand pricing dynamics.

Key Features:
- Data Collection: Utilized a dataset containing Airbnb listings in Tokyo, with variables such as price, location, number of bedrooms, and review ratings.
- Geospatial Analysis: Calculated the distance between Airbnb locations and major tourist attractions using the Haversine formula.
- Regression Model: Built an Ordinary Least Squares (OLS) regression model to quantify the impact of proximity to tourist attractions on rental prices.
- Robust Standard Errors: Corrected for heteroskedasticity using robust standard errors.
- Tourist Spot Analysis: Identified significant effects of proximity to popular attractions on Airbnb prices, with varying relationships based on the specific site.


Prerequisites:
Before running the script, ensure the following libraries are installed:
- stringr
- lmtest
- sandwich
- stargazer
- corrplot
- dplyr


Methodology:
1. Data Cleaning: Processed the raw dataset to remove missing values and irrelevant variables.
2. Distance Calculation: Used the Haversine formula to compute the distance between Airbnb locations and key tourist attractions.
3. OLS Regression: Ran an OLS regression with control variables (bedrooms, review scores) and proximity to tourist attractions.
4. Gauss-Markov Assumptions Testing: Tested and validated the assumptions for the OLS model.
5. Limitations: Considered the limitations of the model, including challenges with the "ceteris paribus" assumption and contradictory results when running regressions by neighborhood.


Results:
- Significant Impact: Most tourist attractions showed a significant effect on Airbnb prices.
- Contrasting Effects: The closer to some attractions (e.g., Tokyo Skytree, Meiji Jingu Shrine), the higher the prices, while proximity to other spots (e.g., Sensoji Temple, Shinjuku Golden Kai) showed the opposite trend.
- Model Limitations: Results were inconsistent when analyzing specific neighborhoods, and the OLS model had limitations in maintaining the ceteris paribus assumption.


Conclusion:
While proximity to tourist sites in Tokyo does influence Airbnb prices, the model has limitations that must be considered. Further research could explore alternative models and incorporate additional factors to improve the analysis.
