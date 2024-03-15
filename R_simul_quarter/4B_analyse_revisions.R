# Pour que ce programme puisse tourner, il faut également avoir lancé ceux sous R_local_ic

source("R_simul_quarter/4_utils.R",encoding = "UTF-8")
library(ggplot2)
library(scales)

all_methods <- c("lp", "arima", "localic_daf_trunc", "localic_final")

for (crit in c("ce", "fe")) {
  for (method in all_methods) {
    data <- readRDS(sprintf("results_simul_quarter/compile_revisions/%s_%s_rev.RDS", method, crit)) %>%
      select_series() %>%
      select_mae()
    if (method == "lp")
      data <- data %>% dplyr::filter(kernel == "henderson")
    if (method %in% c("localic_daf_trunc", "localic_final")){
      suff <- ifelse(method == "localic_final", "_final", "")
      data <- data  %>% dplyr::filter(degree == "d2", h == "h2") %>%
        mutate(method = sprintf("%s_localic%s", method,
                                suff)) %>%
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
    mutate(method = factor(method,c("lc", "lc_localic_final", "lc_localic",
                                    "ql", "ql_localic_final", "ql_localic",
                                    "cq","daf", "auto_arima"),
                           ordered = TRUE),
           variability = factor(variability,
                                levels = c("lowvariability","mediumvariability","highvariability"),
                                ordered = TRUE))
  assign(sprintf("all_rev_%s", crit), all_data)
}

x = all_rev_fe
rev_tot = rbind(all_rev_fe %>% summarise_ref(),
                all_rev_ce %>% summarise_ref())
rev_rel = rbind(all_rev_fe %>% summarise_ref(normalise = TRUE),
                all_rev_ce %>% summarise_ref(normalise = TRUE))
rev_tot %>% dplyr::filter(variability == "mediumvariability")
rev_rel %>% dplyr::filter(variability == "mediumvariability")
rev_tot %>% dplyr::filter(method %in% c("lc", "gain"))

rev_table <- rev_tot %>% dplyr::filter(variability == "mediumvariability")%>%
  select(!c(variability)) %>%
  mutate(method = recode(method, lc = "LC", ql = "QL",
                         cq = "CQ", daf = "DAF",
                         lc_localic_final = "LC local param. (final estimates)",
                         lc_localic = "LC local param.",
                         ql_localic_final = "QL local param. (final estimates)",
                         ql_localic = "QL local param.",
                         auto_arima = "ARIMA")) %>%
  rename_at(vars(starts_with("rev")), function(x){
    sprintf("$q=%s$", gsub("rev.q","",x))
  }) %>%
  rename(`Method` = method)
rev_table
saveRDS(rev_table, file = "data/simulations_revisions_q.RDS")
