library(AQLThesis)
library(rjd3filters)
if(!dir.exists("data_simul/byseriespente_nn"))
  dir.create("data_simul/byseriespente_nn")

X_gen <- function(d = 1, p = 6, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}

gen_MM <- function(p=6, q=p, d=2){
  k = rjd3filters::get_kernel("Henderson", h = p)
  k
  k = c(rev(k$coef[-1]), k$coef[seq(0,q)+1])
  k
  K = diag(k)
  X = X_gen(d=d, p = p, q = q)
  e1 = e2 = e3 = matrix(0, ncol = 1, nrow = d+1)
  e1[1] = 1
  e2[2] = 1
  e3[3] = 1
  M1 = K %*% X %*% solve(t(X) %*% K %*% X, e1)
  M2 = K %*% X %*% solve(t(X) %*% K %*% X, e2)
  M3 = K %*% X %*% solve(t(X) %*% K %*% X, e3)
  cbind(M1, M2, M3)
}

MM = lapply(0:6, function(q){
  d2 = gen_MM(p=12-q, q=q, d=2)
  d3 = gen_MM(p=12-q, q=q, d=3)
  
  list(pente = list(
    `d=2` = moving_average(d2[,2], -(12-q)) ,
    `d=3` = moving_average(d3[,2], -(12-q))
  ),
  `deriv2` = list(
    `d=2` = moving_average(d2[,3], -(12-q)),
    `d=3` = moving_average(d3[,3], -(12-q))
  ))
})
MM = list(pente = list(
  `d=2` = finite_filters(MM[[7]][[1]][[1]], sapply(MM[-7], function(x) x[[1]][[1]]), first_to_last = TRUE),
  `d=3` = finite_filters(MM[[7]][[1]][[2]], sapply(MM[-7], function(x) x[[1]][[2]]), first_to_last = TRUE)
),
`deriv2` = list(
  `d=2` = finite_filters(MM[[7]][[2]][[1]], sapply(MM[-7], function(x) x[[2]][[1]]), first_to_last = TRUE),
  `d=3` = finite_filters(MM[[7]][[2]][[2]], sapply(MM[-7], function(x) x[[2]][[2]]), first_to_last = TRUE)
)
)

s = list.files("data_simul/byseries",full.names = TRUE)[1]
d = 2
h = 6
for(s in list.files("data_simul/byseries",full.names = TRUE)){
  print(s)
  for(h in 6){
    new_f = sprintf("data_simul/byseriespente_nn/%s_h%i.RDS",
                    gsub(".RDS", "",basename(s)),h)
    print(new_f)
    hend_filter <- lp_filter(horizon = h)@sfilter
    if(!file.exists(new_f)){
      data <- readRDS(s)
      info <- lapply(data, function(x){
        sigma2 <- var_estimator(x, hend_filter)
        list("LC" = list(
          `d=2` = as.numeric(tail(rjd3filters::filter(x, MM$pente$`d=2`),6)),
          `d=3` = as.numeric(tail(rjd3filters::filter(x, MM$pente$`d=3`),6)),
          `sigma2` = sigma2
        ),
        "QL" = list(
          `d=2` = as.numeric(tail(rjd3filters::filter(x, MM$deriv2$`d=2`),6)),
          `d=3` = as.numeric(tail(rjd3filters::filter(x, MM$deriv2$`d=3`),6)),
          `sigma2` = sigma2
        )
        )
      })
      saveRDS(info, new_f)
    }
  }
}
