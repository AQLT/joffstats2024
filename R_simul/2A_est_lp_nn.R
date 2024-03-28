if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/lp_nn"))
  dir.create("results_simul/lp_nn")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)

list_series <- list.files("data_simul/byseries", full.names = TRUE)
list_kernel <- c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube",
                 "Gaussian", "Triangular", "Parabolic")[1]

h_filter <- lp_filter(horizon = 6, kernel = kernel)@sfilter
lp_filter2_nn <- function(icr, method = "LC", h = 6, kernel = "Henderson"){
  rfilters = lapply(seq_along(icr), function(i){
    lp_filter(horizon=h+i, 
              endpoints = method,
              kernel = kernel,
              ic = icr[i])[,2*i+1]
  })
  finite_filters(h_filter, rfilters = rfilters)
}
fs <- list()
j <- 1
reload <- FALSE
for(kernel in list_kernel){
  print(kernel)
  h_filter <- lp_filter(horizon = 6, kernel = kernel)@sfilter
  for (method in c("LC","QL", "CQ", "DAF")){
    print(method)
    for(s in list_series){
      name_file <- gsub(".RDS$", "", basename(s))
      print(name_file)
      data <- readRDS(s)
      data_info <- readRDS(sub("byseries", "byseriesinfo", s))
      nom_f_s <- sprintf("results_simul/lp_nn/%s_%s_%s.RDS",
                         name_file,
                         tolower(kernel), tolower(method))
      nom_f_s_tp <- 
        sprintf("results_simul/lp_nn/%s_%s_%s_tp.RDS",
                name_file,
                tolower(kernel), tolower(method))
      
      nom_f_s_rev_fe <- sprintf("results_simul/lp_nn/%s_%s_%s_fe_rev.RDS",
                                name_file,
                                tolower(kernel), tolower(method))
      nom_f_s_rev_ce <- sprintf("results_simul/lp_nn/%s_%s_%s_ce_rev.RDS",
                                name_file,
                                tolower(kernel), tolower(method))
      
      if(all(file.exists(nom_f_s_tp),
             file.exists(nom_f_s_rev_fe),
             file.exists(nom_f_s_rev_ce)))
        next;
      
      reload <- TRUE
      fs[[j]] <- future({
        print(s)
        series_s <- lapply(names(data), function(nom_d){
          x <- data[[nom_d]]
          l = 13
          icr = data_info[[nom_d]][sprintf("icr-%s", l)]
          lp_coef = finite_filters(h_filter, lapply(1:6, function(i){
            lp_filter(horizon=6+i, endpoints = method,
                      kernel = kernel,
                      ic = icr)[,2*i+1]
          }))
          res <- rjd3filters::filter(x, lp_coef)
          
          res[1:((l-1)/2)] <- NA
          res
        })
        names(series_s) <- names(data)
        
        # saveRDS(series_s, nom_f_s)
        
        print("turning points")
        tp <- lapply(series_s, turning_points)
        saveRDS(tp,
                nom_f_s_tp)
        
        revisions_firstest <- first_est_revisions(series_s)
        revisions_consest <- consecutive_est_revisions(series_s)
        
        saveRDS(revisions_firstest,
                nom_f_s_rev_fe)
        saveRDS(revisions_consest,
                nom_f_s_rev_ce)
        TRUE
      })
      j <- j+1
    }
  }
  if(reload){
    vs <- lapply(fs, value)
    fs <- list()
    j <- 1
  }
}
