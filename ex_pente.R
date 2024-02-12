# # To install rjd3filters
# remotes::install_github("rjdemetra/rjd3toolkit")
# remotes::install_github("rjdemetra/rjd3x11plus")
# remotes::install_github("rjdemetra/rjd3filters")
library(rjd3filters)
library(ggplot2)
library(patchwork)
library(zoo)
library(forecast)
y <- readRDS("data_simul/byseries/mediumvariability2.RDS")

# Local estimates of IC-ratios
# We replicate the direct estimates to have
# estimators of the slope and the concavity
gen_MM <- function(p=6, q=p, d=2){
  X_gen <- function(d = 1, p = 6, q = p){
    sapply(0:d, function(exp) seq(-p, q)^exp)
  }
  k = rjd3filters::get_kernel("Henderson", h = p)
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  # Estimator of the constant
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  # Estimator of the slope
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  # Estimor of the concavity
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  mm <- list(const = M1, slope = M2, concav = M3)
  lapply(mm, moving_average, lags = -p)
}
henderson_f = lp_filter()@sfilter
all_mm <- lapply(6:0, gen_MM, p = 6, d = 2)
est_slope <- finite_filters(all_mm[[1]]$slope,
                            lapply(all_mm[-1], `[[`, "slope"))

var_est <- sapply(y, var_estimator, coef = henderson_f)
slope_est <- y[[length(y)]] * est_slope@sfilter
slope_est2 <- y[[length(y)]] * est_slope[,"q=2"]

var_est <- ts(var_est, end = end(y[[length(y)]]),
              frequency = 12)
slope_est <- ts(slope_est, end = end(y[[length(y)]]),
                frequency = 12)
slope_est2 <- ts(slope_est2, end = end(y[[length(y)]]),
                frequency = 12)
icr_local <- (slope_est / sqrt(var_est))
icr_local2 <- (slope_est2 / sqrt(var_est))
icr_local <- abs(icr_local)
icr_local2 <- abs(icr_local2)

icr <- sapply(y, function(x) {
  ic_ratio(x, henderson(x, length = 13, musgrave = FALSE))
})
icr <- ts(icr, end = end(y[[length(y)]]),
          frequency = 12)
icr <- 2/(sqrt(pi) * icr)
data <- ts.intersect(icr, icr_local,icr_local2)

tp = readRDS("data_simul/tp_simul1.RDS")

p <- AQLTools::graph_ts(data[,c(1,2)],n_xlabel = 10,outDec = ".") +
  geom_vline(data = data.frame(xintercept = c(tp$downturn,
                                              tp$upturn)),
             aes(xintercept = xintercept)) +
  scale_color_discrete(
    breaks = c("icr", "icr_local"),
    labels = c(
      latex2exp::TeX("Global $|\\delta_1/\\sigma|"),
      latex2exp::TeX("Local $|\\delta_1/\\sigma|"))) +
  theme_bw() +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('gray99', 0.8),
                                         colour = "gray80", linetype = "solid")
  )

p
ggsave("img/filters_used/mm_penteconcavite_ex.pdf",
            plot = p,
            width = 7,height = 2)

p2 <- AQLTools::graph_ts(data[,c(1,3)],n_xlabel = 10,outDec = ".") +
  geom_vline(data = data.frame(xintercept = c(tp$downturn,
                                              tp$upturn)),
             aes(xintercept = xintercept)) +
  scale_color_discrete(
    breaks = c("icr", "icr_local2"),
    labels = c(
      latex2exp::TeX("Global $|\\delta_1/\\sigma|"),
      latex2exp::TeX("Local $|\\delta_1/\\sigma|"))) +
  theme_bw() +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('gray99', 0.8),
                                         colour = "gray80", linetype = "solid")
  )
p2

ggsave("img/filters_used/mm_penteconcavite_ex2.pdf",
       plot = p2,
       width = 7,height = 2)
