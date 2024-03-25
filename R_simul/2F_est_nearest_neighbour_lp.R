if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/ner_neigh_lp"))
  dir.create("results_simul/ner_neigh_lp")
library(rjd3filters)
library(AQLThesis)
library(future)
library(forecast)
plan(multisession)

list_series <- list.files("data_simul/byseries", full.names = TRUE)
l = 13
h_filter <- lp_filter(horizon = 6)@sfilter
lp_nn <- list(
  `d=2` = finite_filters(h_filter, lapply(1:6, function(i){
    lp_filter(horizon=6+i, endpoints = "DAF", degree = 2)[,2*i+1]
  })),
  `d=3` = finite_filters(h_filter, lapply(1:6, function(i){
    lp_filter(horizon=6+i, endpoints = "DAF", degree = 3)[,2*i+1]
  }))
)
s = list_series[1]
fs <- list()
i <- 0
for(s in list_series){
  i <- i+1
  for(d in 2:3){
    name_file <- gsub(".RDS$", "", basename(s))
    print(name_file)
    fs[[i]] <- future({
      print(s)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results_simul/ner_neigh_lp/%s_d%s.RDS",
                         name_file, d)
      nom_f_s_tp <- 
        sprintf("results_simul/ner_neigh_lp/%s_d%s_tp.RDS",
                name_file, d)
      
      nom_f_s_rev_fe <- sprintf("results_simul/ner_neigh_lp/%s_d%s_fe_rev.RDS",
                                name_file, d)
      nom_f_s_rev_ce <- sprintf("results_simul/ner_neigh_lp/%s_d%s_ce_rev.RDS",
                                name_file, d)
      
      if(all(file.exists(nom_f_s_tp),
             file.exists(nom_f_s_rev_fe),
             file.exists(nom_f_s_rev_ce)))
        next;
      
      series_s <- lapply(names(data), function(nom_d){
        lp_nn[[sprintf("d=%i", d)]] * data[[nom_d]]
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

