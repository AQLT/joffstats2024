library(AQLThesis)
if(!dir.exists("results_simul_quarter/compile_tp"))
  dir.create("results_simul_quarter/compile_tp")

tp = readRDS("data_simul_quarter/tp_simul1.RDS")


all_files <- list.files("results_simul_quarter/lp/",pattern = "_tp",full.names = TRUE)

all_tp_lp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),
                   peaks = tp$downturn,
                   troughs = tp$upturn)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
kernel <- sapply(split, `[`, 2)
method <- sapply(split, `[`, 3)
all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names


saveRDS(all_t, "results_simul_quarter/compile_tp/troughs_lp.RDS")
saveRDS(all_p, "results_simul_quarter/compile_tp/peaks_lp.RDS")



all_files <- list.files("results_simul_quarter/arima/",pattern = "_tp",full.names = TRUE)

all_tp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = tp$downturn,
                   troughs = tp$upturn)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
ny <- gsub("ny", "", sapply(split, `[`, 2))
ny[is.na(ny)] <- "All"
method <- "auto_arima"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method, ny)
all_p <- data.frame(t(sapply(all_tp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method, ny)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_simul_quarter/compile_tp/troughs_arima.RDS")
saveRDS(all_p, "results_simul_quarter/compile_tp/peaks_arima.RDS")

all_files <- list.files("results_simul_quarter/ner_neigh/",pattern = "_tp",full.names = TRUE)
all_tp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = tp$downturn,
                   troughs = tp$upturn)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
method <- "nearest_neighbour"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_simul_quarter/compile_tp/troughs_ner_neigh.RDS")
saveRDS(all_p, "results_simul_quarter/compile_tp/peaks_ner_neigh.RDS")


for(dir in c("localic_daf_trunc", "localic_final")){
  
  all_files <- list.files(sprintf("results_simul_quarter/%s/", dir),pattern = "_tp",full.names = TRUE)
  all_tp_lp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),
                     peaks = tp$downturn,
                     troughs = tp$upturn)
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  method <- sapply(split, `[`, 2)
  if(length(grep("localic_daf", dir)) >0){
    h <- "h6"
    degree <- sapply(split, `[`, 3)
  } else {
    h <- sapply(split, `[`, 3)
    degree <- sapply(split, `[`, 4)
  }
  degree[is.na(degree)] <- "d3"
  all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_simul_quarter/compile_tp/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_simul_quarter/compile_tp/peaks_%s.RDS", dir))
  
  all_tp_lp <- lapply(seq_along(all_files), function(i){
    print(i)
    f = all_files[i]
    compute_time_lag(readRDS(f),
                     peaks = tp$downturn,
                     troughs = tp$upturn,
                     type = "no_revisions")
  })
  
  full_names <- gsub("_tp.RDS$", "", basename(all_files))
  split <- strsplit(full_names, "_")
  series <- sapply(split, `[`, 1)
  method <- sapply(split, `[`, 2)
  if(length(grep("localic_daf", dir)) >0){
    h <- "h6"
    degree <- sapply(split, `[`, 3)
  } else {
    h <- sapply(split, `[`, 3)
    degree <- sapply(split, `[`, 4)
  }
  degree[is.na(degree)] <- "d3"
  
  all_t <- data.frame(t(sapply(all_tp_lp, function(x) x[["troughs"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  all_p <- data.frame(t(sapply(all_tp_lp, function(x) x[["peaks"]][["phaseshift"]])),
                      series, kernel = "henderson", method, h, degree)
  rownames(all_t) <- rownames(all_p) <- full_names
  
  saveRDS(all_t, sprintf("results_simul_quarter/compile_tp_norev/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_simul_quarter/compile_tp_norev/peaks_%s.RDS", dir))
}

