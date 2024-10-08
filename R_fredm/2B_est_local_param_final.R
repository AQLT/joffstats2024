if(!dir.exists("results_fredm"))
  dir.create("results_fredm")
if(!dir.exists("results_fredm/localic_final"))
  dir.create("results_fredm/localic_final")
library(rjd3filters)
library(AQLThesis)
library(future)
plan(multisession)

kernel = "Henderson"
fs <- list()
j <- 1
reload <- FALSE
lp_filter2 <- function(icr, method = "LC", h = 6, kernel = "Henderson"){
  all_coef = lapply(icr, function(ic){
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
h_retain <- 3:6
kernel = "Henderson"
d <- 2
s = list.files("data_fredm/byseries", full.names = TRUE)[1]
h <- 6
method <- "LC"
for (method in c("LC","QL")){
  print(method)
  for(s in list.files("data_fredm/byseries", full.names = TRUE)){
    for(d in 2:3){
      for(h in h_retain) {
        name_file <- gsub(".RDS$", "", basename(s))
        print(name_file)
        data <- readRDS(s)
        complement = sprintf("_d%i", d)
        
        nom_f_s <- sprintf("results_fredm/localic_final/%s_%s_h%i%s.RDS",
                           name_file, tolower(method), h, complement)
        nom_f_s_tp <- 
          sprintf("results_fredm/localic_final/%s_%s_h%i%s_tp.RDS",
                  name_file,
                  tolower(method), h, complement)
        
        data_info <- readRDS(sprintf("data_fredm/byseriespente_final_nber/%s_h%i.RDS",
                                     gsub(".RDS", "",basename(s)),h))
        if(all(file.exists(nom_f_s_tp),
               file.exists(nom_f_s)))
          next;
        
        reload <- TRUE
        fs[[j]] <- future({
          print(s)
          series_s <- lapply(names(data), function(nom_d){
            x <- data[[nom_d]]
            data_t = data_info[[nom_d]][[method]]
            ratio = data_t[[sprintf("d=%i", d)]] / sqrt(data_t[["sigma2"]])
            icr = 2/(sqrt(pi) * ratio)
            lp_coef = lp_filter2(ic = icr, method = method, h = 6, kernel = kernel)
            rjd3filters::filter(x, lp_coef)
          })
          names(series_s) <- names(data)
          
          saveRDS(series_s, nom_f_s)
          
          print("turning points")
          tp <- lapply(series_s, turning_points)
          saveRDS(tp,
                  nom_f_s_tp)
          
          TRUE
        })
        j <- j+1 
      }
      
    }
  }
}
if (reload) {
  vs <- lapply(fs, value)
  fs <- list()
  j <- 1
}
