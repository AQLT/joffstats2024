library(AQLThesis)
if(!dir.exists("results_simul/compile_tp"))
  dir.create("results_simul/compile_tp")

tp = readRDS("data_simul/tp_simul1.RDS")


all_files <- list.files("results_simul/lp/",pattern = "_tp",full.names = TRUE)

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


saveRDS(all_t, "results_simul/compile_tp/troughs_lp.RDS")
saveRDS(all_p, "results_simul/compile_tp/peaks_lp.RDS")



all_files <- list.files("results_simul/arima/",pattern = "_tp",full.names = TRUE)

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

saveRDS(all_t, "results_simul/compile_tp/troughs_arima.RDS")
saveRDS(all_p, "results_simul/compile_tp/peaks_arima.RDS")

all_files <- list.files("results_simul/ner_neigh_hend/",pattern = "_tp",full.names = TRUE)
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
method <- "nearest_neighbour_henderson"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_simul/compile_tp/troughs_ner_neigh_hen.RDS")
saveRDS(all_p, "results_simul/compile_tp/peaks_ner_neigh_hen.RDS")

all_files <- list.files("results_simul/ner_neigh_lp/",pattern = "_tp",full.names = TRUE)
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
degree <- sapply(split, `[`, 2)
method <- "nearest_neighbour_lp"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method, degree)
all_p <- data.frame(t(sapply(all_tp, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method, degree)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_simul/compile_tp/troughs_ner_neigh_lp.RDS")
saveRDS(all_p, "results_simul/compile_tp/peaks_ner_neigh_lp.RDS")


for(dir in c("localic_daf_trunc", "localic_final")){
  
  all_files <- list.files(sprintf("results_simul/%s/", dir),pattern = "_tp",full.names = TRUE)
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
  
  saveRDS(all_t, sprintf("results_simul/compile_tp/troughs_%s.RDS", dir))
  saveRDS(all_p, sprintf("results_simul/compile_tp/peaks_%s.RDS", dir))
}

