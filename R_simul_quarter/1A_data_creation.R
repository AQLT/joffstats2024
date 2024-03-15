if(!dir.exists("data_simul_quarter"))
  dir.create("data_simul_quarter")
if(!dir.exists("data_simul_quarter/byseries"))
  dir.create("data_simul_quarter/byseries")
if(!dir.exists("data_simul_quarter/byseriesinfo"))
  dir.create("data_simul_quarter/byseriesinfo")

library(AQLThesis)
library(ggplot2)
set.seed(100)
start = 1960
frequency = 4
time = seq_along(seq(start, 2019+3/4, by = 1/4))
series_q = list(
  highvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.60,lambda = 72/4,rho = 0.5),
  highvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.60,lambda = 72/4,rho = 0.7),
  highvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.60,lambda = 72/4,rho = 1.0),
  mediumvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.50,lambda = 72/4,rho = 1.5),
  mediumvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.50,lambda = 72/4,rho = 2),
  mediumvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.50,lambda = 72/4,rho = 3),
  lowvariability1 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 72/4,rho = 3),
  lowvariability2 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 72/4,rho = 3.5),
  lowvariability3 = simulated_tci(time,sigma_nu = 0.08,sigma_e = 0.40,lambda = 72/4,rho = 4)
)
series_q = lapply(series_q,ts, start = start, frequency = frequency)
do.call(rbind, lapply(series_q, function(x){
  x <- x[,"tc"]
  res = rjd3x11plus::select_trend_filter(x, length = 5)
  res = c(res[c("length", "icr")],
          rjd3x11plus::select_trend_filter(x, length = 7)[1])
  names(res) = c("optimal_length", "icr-5", "icr-7")
  res
}))
forecast::autoplot(do.call(cbind, lapply(series_q[c(2,5,8)],`[`, ,"tc")))  +
  theme_bw() +theme(legend.background = element_rect(fill = alpha('gray99', 0.4),
                                                     colour = "gray80", linetype = "solid"),
                    legend.justification = c(0,0),
                    legend.position = c(0,0),
                    legend.key = element_blank(),
                    legend.title = element_blank()) +
  labs(x = NULL, y = NULL)

tp = turning_points(series_q[[1]][,"cycle"])
first_date = time(series_q[[9]][,"cycle"])[25]
tp = lapply(tp, function(x)x[x>=first_date])
saveRDS(tp, "data_simul_quarter/tp_simul1.RDS")

for(nom_series in names(series_q)){
  all_series <- lapply(time[-(1:8)], function(i){
    ts(series_q[[nom_series]][1:i,"tc"], start = start, frequency = frequency)
  })
  names(all_series) <- sapply(all_series, function(x) time(x)[length(x)])
  saveRDS(all_series, file = sprintf("data_simul_quarter/byseries/%s.RDS", nom_series))
}


library(future)
plan(multisession)

fs <- list()
i <- 0
s = list.files("data_simul_quarter/byseries",full.names = TRUE)[1]
for(s in list.files("data_simul_quarter/byseries",full.names = TRUE)){
  i <- i+1
  print(s)
  fs[[i]] <- future({
    data <- readRDS(s)
    info <- lapply(data, function(x){
      res = rjd3x11plus::select_trend_filter(x, length = 5)
      res = c(res[c("length", "icr")],
              rjd3x11plus::select_trend_filter(x, length = 7)[1])
      names(res) = c("optimal_length", "icr-5", "icr-7")
      # ic-ratio where multiplied by 3 for quarterly series
      res[-1] <- res[-1] / 3
      res
    })
    saveRDS(info, sprintf("data_simul_quarter/byseriesinfo/%s", basename(s)))
    s
  })
}
vs <- lapply(fs, value)


last_icr_m <- do.call(rbind, lapply(lapply(list.files("data_simul/byseriesinfo",full.names = TRUE), readRDS),
                                  function(x) x[[length(x)]]))
rownames(last_icr_m) <- list.files("data_simul/byseriesinfo",full.names = FALSE)
round(last_icr_m,1)
last_icr_q <- do.call(rbind, lapply(lapply(list.files("data_simul_quarter/byseriesinfo",full.names = TRUE), readRDS),
                                  function(x) x[[length(x)]]))
rownames(last_icr_q) <- list.files("data_simul_quarter/byseriesinfo",full.names = FALSE)
round(last_icr_q,1)



