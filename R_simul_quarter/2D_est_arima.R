if(!dir.exists("results_simul_quarter"))
  dir.create("results_simul_quarter")
if(!dir.exists("results_simul_quarter/arima"))
  dir.create("results_simul_quarter/arima")
library(rjd3filters)
library(AQLThesis)
library(future)
library(forecast)
plan(multisession)

list_series <- list.files("data_simul_quarter/byseries", full.names = TRUE)
s = list_series[1]
l = 5
henderson <- lp_filter(horizon = (l - 1) / 2)@sfilter
fs <- list()
i <- 0
for(s in list_series){
  i <- i+1
  name_file <- gsub(".RDS$", "", basename(s))
  print(name_file)
  fs[[i]] <- future({
    print(s)
    data <- readRDS(s)
    data_info <- readRDS(sub("byseries", "byseriesinfo", s))
    nom_f_s <- sprintf("results_simul_quarter/arima/%s.RDS",
                       name_file)
    nom_f_s_tp <- 
      sprintf("results_simul_quarter/arima/%s_tp.RDS",
              name_file)
    
    nom_f_s_rev_fe <- sprintf("results_simul_quarter/arima/%s_fe_rev.RDS",
                              name_file)
    nom_f_s_rev_ce <- sprintf("results_simul_quarter/arima/%s_ce_rev.RDS",
                              name_file)
    
    if(all(file.exists(nom_f_s_tp),
           file.exists(nom_f_s_rev_fe),
           file.exists(nom_f_s_rev_ce)))
      next;
    
    series_s <- lapply(names(data), function(nom_d){
      x <- data[[nom_d]]
      # l = data_info[[nom_d]]["optimal_length"]
      prevs = auto.arima(x, max.Q = 0, max.D = 0, max.P = 0)
      prevs = forecast(prevs, h=(l-1)/2)
      y_prevs = ts(c(x, prevs$mean), start = start(x), frequency = frequency(x))
      window(henderson * y_prevs,
             start = start(x), end = end(x))
    })
    names(series_s) <- names(data)
    
    # saveRDS(series_s, nom_f_s)
    
    print("turning points")
    tp <- lapply(series_s, turning_points)
    
    saveRDS(tp, nom_f_s_tp)
    revisions_firstest <- first_est_revisions(series_s)
    revisions_consest <- consecutive_est_revisions(series_s)
    
    saveRDS(revisions_firstest,
            nom_f_s_rev_fe)
    saveRDS(revisions_consest,
            nom_f_s_rev_ce)
    TRUE
  })
}


vs <- lapply(fs, value)
