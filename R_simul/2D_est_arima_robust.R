if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/arima"))
  dir.create("results_simul/arima")
library(rjd3filters)
library(AQLThesis)
library(future)
library(forecast)
plan(multisession)

list_series <- list.files("data_simul/byseries", full.names = TRUE)
l = 13
henderson <- lp_filter(horizon = (l - 1) / 2)@sfilter
s = list_series[1]
fs <- list()
i <- 0
nyears <- c(4, 6, 8, 10, 12)
for(y in nyears){
  for(s in list_series){
    i <- i+1
    name_file <- gsub(".RDS$", "", basename(s))
    print(sprintf("%s nyears = %s", name_file, y))
    fs[[i]] <- future({
      print(s)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results_simul/arima/%s_ny%s.RDS",
                         name_file, y)
      nom_f_s_tp <- 
        sprintf("results_simul/arima/%s_ny%s_tp.RDS",
                name_file, y)
      
      nom_f_s_rev_fe <- sprintf("results_simul/arima/%s_ny%s_fe_rev.RDS",
                                name_file, y)
      nom_f_s_rev_ce <- sprintf("results_simul/arima/%s_ny%s_ce_rev.RDS",
                                name_file, y)
      
      if(all(file.exists(nom_f_s_tp),
             file.exists(nom_f_s_rev_fe),
             file.exists(nom_f_s_rev_ce)))
        next;
      
      series_s <- lapply(names(data), function(nom_d){
        x <- data[[nom_d]]
        # l = data_info[[nom_d]]["optimal_length"]
        first_date <- 1 + max(0, length(x) - y * frequency(x))
        x_arima <- window(x, start = time(x)[first_date])
        prevs = auto.arima(x_arima, max.Q = 0, max.D = 0, max.P = 0)
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
  
}

vs <- lapply(fs, value)

