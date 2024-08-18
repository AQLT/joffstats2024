This folder contains all the programs used to analyse results on **monthly** simulated data

- `1A_data_creation.R`: creation of simulated data
- `1B_plot_data.R`: graph of simulated data
- `1C_Est_final_slope.R`, `1C_Est_final_slope_nn.R`, `1D_Est_real_time_slope.R`: estimates of the slope and the concavity, final estimates (usual filters and nearest neighbour) and real-time estimates 
- `1E_Example_slope_concavity.R`: example of slope and concavity estimates (figure 3)
- `2A_est_lp_nn.R`, `2A_est_lp.R`, `2B_est_local_param_final_nn.R`, `2B_est_local_param_final.R`, `2C_est_local_param_real_time.R`, `2D_est_arima_robust.R` (different estimates span), `2D_est_arima.R` (full span), `2E_est_nn_hen.R`, `2F_est_nnr_lp.R`: estimation of the trend-cycle at each date for the different methods in the working document and detection of turning points. The results are stored in a `results_simul` folder. 
- `3A_summary_of_tp.R`: calculation of the phase shift for all methods: number of months required to detect a turning point (usual definition but **not the one used in the paper**)
- `3B_summary_of_tp_norev.R`: calculation of the phase shift for all methods but using a different definition: number of months required to detect a turning point **without future revision** (**definition used in the paper**).
- `3C_summary_of_rev.R`: analysis of statistics on revisions
- `4_utils.R`: functions used in the following programs to summarise all outputs and produce graphs 
- `4B_Analyse_tp.R`: analysis of statistics on turning points defined as the number of months required to detect a turning point (usual definition but **not the one used in the paper**)
- `4A_Analyse_tp_rev.R`: analysis of statistics on turning points defined as the number of months required to detect a turning point **without future revision** (**definition used in the paper**).
