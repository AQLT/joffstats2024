if(!dir.exists("results_simul"))
  dir.create("results_simul")
if(!dir.exists("results_simul/localic_daf_trunc"))
  dir.create("results_simul/localic_daf_trunc")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)

kernel = "Henderson"
method = "LC"
s = list.files("data_simul/byseries",full.names = TRUE)[1]

d = 2
h = 6
lp_filter2 <- function(icr, method = "LC", h = 6, kernel = "Henderson"){
  all_coef = lapply(as.numeric(icr), function(ic){
    lp_filter(horizon = h,
              kernel = kernel,
              endpoints = method,
              ic = ic)
  })
  sym = all_coef[[1]]@sfilter
  rfilters = lapply(1:h, function(i){
    q=h -i
    all_coef[[i]][,sprintf("q=%i", q)]
  })
  finite_filters(sym, rfilters = rfilters)
}
fs <- list()
j <- 1
reload <- FALSE
l <- 6
for (method in c("LC","QL")){
  print(method)
  for(s in list.files("data_simul/byseries", full.names = TRUE)){
    for(d in 2:3){
      for(h in 3:6) {
        name_file <- gsub(".RDS$", "", basename(s))
        print(name_file)
        data <- readRDS(s)
        complement = sprintf("_d%i", d)
        
        nom_f_s <- sprintf("results_simul/localic_daf_trunc/%s_%s_h%i%s.RDS",
                           name_file, tolower(method), h, complement)
        nom_f_s_tp <- 
          sprintf("results_simul/localic_daf_trunc/%s_%s_h%i%s_tp.RDS",
                  name_file,
                  tolower(method), h, complement)
        
        nom_f_s_rev_fe <- sprintf("results_simul/localic_daf_trunc/%s_%s_h%i%s_fe_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        nom_f_s_rev_ce <- sprintf("results_simul/localic_daf_trunc/%s_%s_h%i%s_ce_rev.RDS",
                                  name_file,
                                  tolower(method), h, complement)
        data_info <- readRDS(sprintf("data_simul/byseriespente_daf/%s_h%i.RDS",
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
            icr[abs(icr) > 12] <- 12
            lp_coef = lp_filter2(icr = icr, method = method, h = 6, kernel = kernel)
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

