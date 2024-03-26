if(!dir.exists("results_simul_quarter"))
  dir.create("results_simul_quarter")
if(!dir.exists("results_simul_quarter/ner_neigh"))
  dir.create("results_simul_quarter/ner_neigh")
library(rjd3filters)
library(AQLThesis)
library(future)
library(forecast)
plan(multisession)

X_gen <- function(d = 1, p = 2, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}

gen_MM <- function(p=2, q=p, d=2){
  k = rjd3filters::get_kernel("Henderson", h = p)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M1
}

list_series <- list.files("data_simul_quarter/byseries", full.names = TRUE)
l = 5
filter <- lapply(2:0, function(i){
  fst_filter(lags = 5-i-1, leads = i,pdegree = 2)
})
hend_ner_nb <-finite_filters(filter[[1]], filter[-1])
s = list_series[1]
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
    nom_f_s <- sprintf("results_simul_quarter/ner_neigh/%s.RDS",
                       name_file)
    nom_f_s_tp <- 
      sprintf("results_simul_quarter/ner_neigh/%s_tp.RDS",
              name_file)
    
    nom_f_s_rev_fe <- sprintf("results_simul_quarter/ner_neigh/%s_fe_rev.RDS",
                              name_file)
    nom_f_s_rev_ce <- sprintf("results_simul_quarter/ner_neigh/%s_ce_rev.RDS",
                              name_file)
    
    if(all(file.exists(nom_f_s_tp),
           file.exists(nom_f_s_rev_fe),
           file.exists(nom_f_s_rev_ce)))
      next;
    
    series_s <- lapply(names(data), function(nom_d){
      hend_ner_nb * data[[nom_d]]
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

