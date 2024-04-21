# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

all_methods <- c("lp", "arima", "localic_daf_trunc", "localic_final")

for (crit in c("ce", "fe")) {
  for (method in all_methods) {
    data <- readRDS(sprintf("results_simul/compile_revisions/%s_%s_rev.RDS", method, crit)) %>%
      select_series() %>%
      select_mae()
    if (method == "lp")
      data <- data %>% dplyr::filter(kernel == "henderson")
    if (method %in% c("localic_daf_trunc", "localic_final")){
      suff <- ifelse(method == "localic_final", "_final", "")
      data <- data %>%
        mutate(method = sprintf("%s_localic%s_%s_%s", method, suff, degree, h)) %>%
        select(!c(degree, h))
    }
    if (method == "arima") {
      data <- data  %>% dplyr::filter(ny == "All") %>% 
        select(!c(ny))
    }
    data <- data %>% select(!kernel)
    assign(sprintf("rev_%s_%s", crit, method), data)
  }
  all_data <- do.call(rbind, mget(sprintf("rev_%s_%s", crit, all_methods))) %>%
    mutate(variability = factor(variability,
                                levels = c("lowvariability","mediumvariability","highvariability"),
                                ordered = TRUE))
  assign(sprintf("all_rev_%s", crit), all_data)
}

x = all_rev_fe
rev_tot = rbind(all_rev_fe  %>% summarise_ref()|> mutate(crit = "1fe"),
                all_rev_ce %>% summarise_ref()|> mutate(crit = "2ce"))
rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE)|> mutate(crit = "1fe"),
                all_rev_ce %>% summarise_ref(normalise = TRUE)|> mutate(crit = "2ce"))
rev_tot %>% dplyr::filter(variability == "mediumvariability")
rev_rel %>% dplyr::filter(variability == "mediumvariability")
rev_tot %>% dplyr::filter(method %in% c("lc", "gain"))

legend <- c(lc = "LC", lc_localic_final_d2_h6 = "LC local param. (final estimates)",
            lc_localic_d2_h6 = "LC local param.",
            ql = "QL",
            ql_localic_final_d2_h6 = "QL local param. (final estimates)",
            ql_localic_d2_h6 = "QL local param.",
            cq = "CQ", daf = "DAF",
            auto_arima = "ARIMA")
rev_table <- rev_tot %>% 
  dplyr::filter(variability == "mediumvariability" & method %in% names(legend))%>%
  mutate(method = recode(factor(method, level = names(legend), ordered = TRUE), !!!legend)) %>%
  arrange(crit, method) |> 
  select(!c(variability, crit)) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
rev_table
saveRDS(rev_table, file = "paper/data/simulations_revisions.RDS")


legend <- c( 
  "lc_localic_final_d2_h6" = "d=2, h=3", 
  "lc_localic_final_d2_h4" = "d=2, h=4", 
  "lc_localic_final_d2_h5" = "d=2, h=5",
  "lc_localic_final_d2_h6" = "d=2, h=6",
  "lc_localic_final_d3_h3" = "d=3, h=3", 
  "lc_localic_final_d3_h4" = "d=3, h=4", 
  "lc_localic_final_d3_h5" = "d=3, h=5", 
  "lc_localic_final_d3_h6" = "d=3, h=6")
rev_table_lc_final <- rev_tot %>% 
  dplyr::filter(variability == "mediumvariability" & method %in% names(legend))%>%
  mutate(method = recode(factor(method, level = names(legend), ordered = TRUE), !!!legend)) %>%
  arrange(crit, method) |> 
  select(!c(variability, crit)) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
rev_table_lc_final

legend <- c( 
  "lc_localic_d2_h6" = "d=2, h=3", 
  "lc_localic_d2_h4" = "d=2, h=4", 
  "lc_localic_d2_h5" = "d=2, h=5",
  "lc_localic_d2_h6" = "d=2, h=6",
  "lc_localic_d3_h3" = "d=3, h=3", 
  "lc_localic_d3_h4" = "d=3, h=4", 
  "lc_localic_d3_h5" = "d=3, h=5", 
  "lc_localic_d3_h6" = "d=3, h=6")
rev_table_lc <- rev_tot %>% 
  dplyr::filter(variability == "mediumvariability" & method %in% names(legend))%>%
  mutate(method = recode(factor(method, level = names(legend), ordered = TRUE), !!!legend)) %>%
  arrange(crit, method) |> 
  select(!c(variability, crit)) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
rev_table_lc
saveRDS(rev_table_lc_final, file = "paper/data/simulations_revisions_lc_final.RDS")
saveRDS(rev_table_lc, file = "paper/data/simulations_revisions_lc.RDS")


legend <- c( 
  "ql_localic_final_d2_h6" = "d=2, h=3", 
  "ql_localic_final_d2_h4" = "d=2, h=4", 
  "ql_localic_final_d2_h5" = "d=2, h=5",
  "ql_localic_final_d2_h6" = "d=2, h=6",
  "ql_localic_final_d3_h3" = "d=3, h=3", 
  "ql_localic_final_d3_h4" = "d=3, h=4", 
  "ql_localic_final_d3_h5" = "d=3, h=5", 
  "ql_localic_final_d3_h6" = "d=3, h=6")
rev_table_ql_final <- rev_tot %>% 
  dplyr::filter(variability == "mediumvariability" & method %in% names(legend))%>%
  mutate(method = recode(factor(method, level = names(legend), ordered = TRUE), !!!legend)) %>%
  arrange(crit, method) |> 
  select(!c(variability, crit)) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
rev_table_ql_final

legend <- c( 
  "ql_localic_d2_h6" = "d=2, h=3", 
  "ql_localic_d2_h4" = "d=2, h=4", 
  "ql_localic_d2_h5" = "d=2, h=5",
  "ql_localic_d2_h6" = "d=2, h=6",
  "ql_localic_d3_h3" = "d=3, h=3", 
  "ql_localic_d3_h4" = "d=3, h=4", 
  "ql_localic_d3_h5" = "d=3, h=5", 
  "ql_localic_d3_h6" = "d=3, h=6")
rev_table_ql <- rev_tot %>% 
  dplyr::filter(variability == "mediumvariability" & method %in% names(legend))%>%
  mutate(method = recode(factor(method, level = names(legend), ordered = TRUE), !!!legend)) %>%
  arrange(crit, method) |> 
  select(!c(variability, crit)) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
rev_table_ql
saveRDS(rev_table_ql_final, file = "paper/data/simulations_revisions_ql_final.RDS")
saveRDS(rev_table_ql, file = "paper/data/simulations_revisions_ql.RDS")