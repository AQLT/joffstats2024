This folder contains all the programs used to analyse the real data from FRED.

- `0_download_files.R`: to download the series and select only those used
- `1_infobyseries.R`: computes statistics on all series (I/C ratio, ‘optimal’ length)
- `1A_Est_final_slope.R`: estimates the final slope and concavity  (for local parametrisation)
- `1B_Est_real_time_slope.R`: estimates the slope and concavity in real-time (for local parametrisation)
- `2A_est_lp.R`, `2B_est_local_param_final.R`, `2C_est_local_param_real_time.R`, `2D_est_arima.R`: estimation of trend-cycle at each date for the different methods in the paper and detection of turning points. The results are stored in a `results_fredm` folder
- `3A_summary_of_tp.R`: calculation of the phase shift for all methods: number of months required to detect a turning point **without future revision** (usual definition but not the one used in the paper)
- `3B_summary_of_tp_norev.R`: calculation of the phase shift for all methods but using a different definition: number of months required to detect a turning point **without future revision**.
- `4_utils.R`: functions used in the following programs to summarise all outputs and produce graphs
- `4A_Analyse_est.R`: analysis of successive estimates of the cycle_trend around certain turning points
- `4B_Analyse_implicit_forecast.R`: analysis of implicit forecasts 