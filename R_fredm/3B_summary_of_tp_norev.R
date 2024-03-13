library(AQLThesis)
if(!dir.exists("results_fredm/compile_tp_norev"))
  dir.create("results_fredm/compile_tp_norev")


all_files <- list.files("results_fredm/lp/",pattern = "_tp",full.names = TRUE)

all_tp_lp <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(data = readRDS(f),
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"],
                   type = "no_revisions",
                   n_ahead_max = 12)
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

detected_tp <- data.frame(t(sapply(all_tp_lp, function(x) c(x[["troughs"]][["last_tp"]], x[["peaks"]][["last_tp"]]))),
           series, kernel)
detected_tp <- detected_tp[method == "lc",]
saveRDS(all_t, "results_fredm/compile_tp_norev/troughs_lp.RDS")
saveRDS(all_p, "results_fredm/compile_tp_norev/peaks_lp.RDS")
saveRDS(detected_tp, "results_fredm/compile_tp_norev/detected_tp_lp.RDS")




all_files <- list.files("results_fredm/arima/",pattern = "_tp",full.names = TRUE)

all_tp_arima <- lapply(seq_along(all_files), function(i){
  print(i)
  f = all_files[i]
  compute_time_lag(readRDS(f),                    
                   peaks = nber_tp_m[,"Peak"],
                   troughs = nber_tp_m[,"Trough"],
                   type = "no_revisions",
                   n_ahead_max = 12)
})

full_names <- gsub("_tp.RDS$", "", basename(all_files))
split <- strsplit(full_names, "_")
series <- sapply(split, `[`, 1)
method <- "auto_arima"
kernel = "henderson"
all_t <- data.frame(t(sapply(all_tp_arima, function(x) x[["troughs"]][["phaseshift"]])),
                    series, kernel, method)
all_p <- data.frame(t(sapply(all_tp_arima, function(x) x[["peaks"]][["phaseshift"]])),
                    series, kernel, method)
rownames(all_t) <- rownames(all_p) <- full_names

saveRDS(all_t, "results_fredm/compile_tp_norev/troughs_arima.RDS")
saveRDS(all_p, "results_fredm/compile_tp_norev/peaks_arima.RDS")


