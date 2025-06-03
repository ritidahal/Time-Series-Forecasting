# Toyota Motors Stock Price Prediction 
This project presents a time series forecasting analysis of Toyota Motors Corporation’s daily stock closing prices using historical data from 2021 to 2025. The analysis was conducted as part of a graduate-level data mining course and aims to support strategic financial planning by identifying trends, seasonality, and volatility in Toyota’s stock performance.

# Data Source
The dataset was retrieved using the GOOGLEFINANCE function in Google Sheets:

=GOOGLEFINANCE("TM", "all", DATE(2021,1,4), TODAY(), "DAILY")
This function extracted daily trading data for Toyota Motors (ticker: TM), including:

Date

Open

High

Low

Close (used as the primary variable for forecasting)

Volume

The dataset spans from January 2021 to early 2025, covering over 1,000 trading days. No missing values were found, and the data required minimal cleaning.

# Analytical Approach

The forecasting pipeline included:

- Exploratory Data Analysis (EDA): Identified trends, seasonal patterns, and distribution shifts.

- Stationarity Testing: Augmented Dickey-Fuller (ADF) test confirmed non-stationarity; first-order differencing was applied.

# Modeling Techniques

- Naïve Forecasting: Used as a baseline.

- STL + ETS: Captured trend and seasonality.

- ARIMA: Automatically selected via auto.arima(), but underperformed.

- Random Walk with Drift (RWF): Outperformed all other models in test accuracy.

# Validation Strategy

- Training Set: Jan 2021 – Dec 2024

- Test Set: Jan 2025 onward

- Metrics: RMSE, MAPE, MASE

# Key Findings

- Seasonality: Monthly patterns were observed, with notable price increases in mid-year and declines in late 2024.

- Volatility: 2024 showed increased unpredictability, likely due to external market shocks.

- Best Model: Random Walk with Drift (RWF) achieved the lowest test RMSE (10.53), MAPE (3.78%), and MASE (0.27).

- Forecast: Toyota’s stock price is projected to steadily increase through 2025, with December 2025 forecasted at approximately $247.36 (95% CI: $163.87–$330.85).

# Strategic Implications
- Investment Planning: The forecast supports gradual stock appreciation, useful for portfolio allocation.

- Risk Management: Confidence intervals highlight potential volatility, suggesting the need for buffer strategies.

# Model Limitations

- Does not account for macroeconomic variables (e.g., interest rates, inflation).

- May not capture sudden market shocks or black swan events.

# Recommendations

- Use RWF model outputs for short-term planning and scenario analysis.

- Consider integrating external regressors (e.g., oil prices, economic indicators) in future iterations.

- Monitor residual autocorrelation to refine model assumptions.
