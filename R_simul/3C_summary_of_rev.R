library(AQLThesis)
if(!dir.exists("results_simul/compile_revisions"))
  dir.create("results_simul/compile_revisions")

tp = readRDS("data_simul/tp_simul1.RDS")
suffix = "_fe_rev"
for(suffix in c("_fe_rev", "_ce_rev")){
  
  all_files <- list.files("results_simul/lp/",pattern = suffix,full.names = TRUE)

  all_rev <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    data = readRDS(f)
    data = summary_revisions(data,
                             peaks = tp$downturn,
                             troughs = tp$upturn)

    full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
    split <- strsplit(full_names, "_")
    series <- sapply(split, `[`, 1)
    kernel <- sapply(split, `[`, 2)
    method <- sapply(split, `[`, 3)
    data$series <- series
    data$kernel <- kernel
    data$method <- method
    data
  })
  all_rev = do.call(rbind, all_rev)

  saveRDS(all_rev, sprintf("results_simul/compile_revisions/lp%s.RDS", suffix))
  
  
  all_files <- list.files("results_simul/lp_nn/",pattern = suffix,full.names = TRUE)
  
  all_rev <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    data = readRDS(f)
    data = summary_revisions(data,
                             peaks = tp$downturn,
                             troughs = tp$upturn)
    
    full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
    split <- strsplit(full_names, "_")
    series <- sapply(split, `[`, 1)
    kernel <- sapply(split, `[`, 2)
    method <- sapply(split, `[`, 3)
    data$series <- series
    data$kernel <- kernel
    data$method <- method
    data
  })
  all_rev = do.call(rbind, all_rev)
  
  saveRDS(all_rev, sprintf("results_simul/compile_revisions/lp_nn%s.RDS", suffix))


  all_files <- list.files("results_simul/arima/",pattern = suffix,full.names = TRUE)

  all_rev <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]

    data = readRDS(f)
    data = summary_revisions(data,
                             peaks = tp$downturn,
                             troughs = tp$upturn)

    full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
    split <- strsplit(full_names, "_")
    series <- sapply(split, `[`, 1)
    ny <- gsub("ny", "", sapply(split, `[`, 2))
    ny[is.na(ny)] <- "All"
    data$series <- series
    data$kernel <- "henderson"
    data$method <- "auto_arima"
    data$ny <- ny
    data
  })
  all_rev = do.call(rbind, all_rev)

  saveRDS(all_rev, sprintf("results_simul/compile_revisions/arima%s.RDS", suffix))

  all_files <- list.files("results_simul/ner_neigh/",pattern = suffix,full.names = TRUE)
  
  all_rev <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    
    data = readRDS(f)
    data = summary_revisions(data,                    
                             peaks = tp$downturn,
                             troughs = tp$upturn)
    
    full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
    split <- strsplit(full_names, "_")
    series <- sapply(split, `[`, 1)
    data$series <- series
    data$kernel <- "henderson"
    data$method <- "ner_neigh"
    data
  })
  all_rev = do.call(rbind, all_rev)
  
  saveRDS(all_rev, sprintf("results_simul/compile_revisions/ner_neigh%s.RDS", suffix))
  
}


for(dir in c("localic_daf_trunc", "localic_final")){
  
  for(suffix in c("_fe_rev", "_ce_rev")){
    
    all_files <- list.files(sprintf("results_simul/%s/", dir),pattern = suffix,full.names = TRUE)
    
    all_rev <- lapply(seq_along(all_files), function(i){
      print(i)
      f = all_files[i]
      data = readRDS(f)
      data = summary_revisions(data,                    
                               peaks = tp$downturn,
                               troughs = tp$upturn)
      
      full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
      split <- strsplit(full_names, "_")
      split <- strsplit(full_names, "_")
      series <- sapply(split, `[`, 1)
      method <- sapply(split, `[`, 2)
      
      h <- sapply(split, `[`, 3)
      degree <- sapply(split, `[`, 4)
      # if(length(grep("localic_daf", dir)) >0){
      #   h <- "h6"
      #   degree <- sapply(split, `[`, 3)
      # } else {
      #   h <- sapply(split, `[`, 3)
      #   degree <- sapply(split, `[`, 4)
      # }
      degree[is.na(degree)] <- "d3"
      
      data$series <- series
      data$kernel <- "henderson"
      data$method <- method
      data$h <- h
      data$degree <- degree
      data
    })
    all_rev = do.call(rbind, all_rev)
    
    saveRDS(all_rev, sprintf("results_simul/compile_revisions/%s%s.RDS",dir, suffix))
    
    all_files <- list.files(sprintf("results_simul/%s_nn/", dir),pattern = suffix,full.names = TRUE)
    if (length(all_files) > 0){
    all_rev <- lapply(seq_along(all_files), function(i){
      print(i)
      f = all_files[i]
      data = readRDS(f)
      data = summary_revisions(data,                    
                               peaks = tp$downturn,
                               troughs = tp$upturn)
      
      full_names <- gsub(sprintf("%s.RDS$", suffix), "", basename(f))
      split <- strsplit(full_names, "_")
      split <- strsplit(full_names, "_")
      series <- sapply(split, `[`, 1)
      method <- sapply(split, `[`, 2)
      h <- sapply(split, `[`, 3)
      degree <- sapply(split, `[`, 4)
      # if(length(grep("localic_daf", dir)) >0){
      #   h <- "h6"
      #   degree <- sapply(split, `[`, 3)
      # } else {
      #   h <- sapply(split, `[`, 3)
      #   degree <- sapply(split, `[`, 4)
      # }
      degree[is.na(degree)] <- "d3"
      
      data$series <- series
      data$kernel <- "henderson"
      data$method <- method
      data$h <- h
      data$degree <- degree
      data
    })
    all_rev = do.call(rbind, all_rev)
    
    saveRDS(all_rev, sprintf("results_simul/compile_revisions/%s_nn%s.RDS",dir, suffix))
  }
  }
}
