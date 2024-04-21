if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/localic_final_nn"))
  dir.create("results_simul/localic_final_nn")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)

kernel = "Henderson"

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
for (method in c("LC","QL")){
  print(method)
  for(s in list.files("data_simul/byseries", full.names = TRUE)){
    for(d in 2:3){
      for(h in 6) {
        name_file <- gsub(".RDS$", "", basename(s))
        print(name_file)
        data <- readRDS(s)
        complement = sprintf("_d%i", d)
        
        nom_f_s <- sprintf("results_simul/localic_final_nn/%s_%s_h%i%s.RDS",
                           name_file, tolower(method), h, complement)
        nom_f_s_tp <- 
          sprintf("results_simul/localic_final_nn/%s_%s_h%i%s_tp.RDS",
                  name_file,
                  tolower(method), h, complement)
        
        nom_f_s_rev_fe <- sprintf("results_simul/localic_final_nn/%s_%s_h%i%s_fe_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        nom_f_s_rev_ce <- sprintf("results_simul/localic_final_nn/%s_%s_h%i%s_ce_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        data_info <- readRDS(sprintf("data_simul/byseriespente_nn/%s_h%i.RDS",
                                     gsub(".RDS", "",basename(s)),h))
        
        if(all(file.exists(nom_f_s_tp),
               file.exists(nom_f_s_rev_fe),
               file.exists(nom_f_s_rev_ce)))
          next;
        
        reload <- TRUE
        fs[[j]] <- future({
          print(s)
          series_s <- lapply(names(data), function(nom_d){
            x <- data[[nom_d]]
            data_t = data_info[[nom_d]][[method]]
            ratio = data_t[[sprintf("d=%i", d)]] / sqrt(data_t[["sigma2"]])
            icr = 2/(sqrt(pi) * ratio)
            lp_coef = lp_filter2_nn(icr = icr, method = method, h = 6, kernel = kernel)
            res <- rjd3filters::filter(x, lp_coef)
            
            res[1:h] <- NA
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
  }
}
if(reload){
  vs <- lapply(fs, value)
  fs <- list()
  j <- 1
}

