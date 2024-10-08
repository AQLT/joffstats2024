library(AQLThesis)
library(rjd3filters)
if(!dir.exists("data_simul_quarter/byseriespente_final"))
  dir.create("data_simul_quarter/byseriespente_final")

# Dans ce programme, pour paramétrer les méthodes LC et QL :
# 1. On prend les MM symétriques finales d'estimation de la pente et polynôme degré 2
#  (dans gen_MM q = p)
# 2. la variance est estimée à chaque date sur les données connues

X_gen <- function(d = 1, p = 2, q = p){
  sapply(0:d, function(exp) seq(-p, q)^exp)
}

gen_MM <- function(p=2, q=p, d=2){
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


MM = lapply(2:3, function(h){
  list(pente = list(
    `d=2` = gen_MM(p = 3, d=2)[,2]
    ),
    `deriv2` = list(
      `d=2` = gen_MM(p = 3, d=2)[,3]
      ),
    henderson = lp_filter(horizon = h)@sfilter
  )
})
names(MM) <- sprintf("h=%i", 2:3)
s = list.files("data_simul_quarter/byseries",full.names = TRUE)[1]
d = 2
h = 2
for(s in list.files("data_simul_quarter/byseries",full.names = TRUE)){
  print(s)
  for(h in 2:3){
    new_f = sprintf("data_simul_quarter/byseriespente_final/%s_h%i.RDS",
                    gsub(".RDS", "",basename(s)),h)
    print(new_f)
    MM_h = MM[[sprintf("h=%i", h)]]
    if(!file.exists(new_f)){
      data <- readRDS(s)
      last_est = data[[length(data)]]
      pente_d2 = zoo::na.locf(moving_average(MM_h$pente[[sprintf("d=%i",2)]], -h) * last_est)
      courbure_d2 = zoo::na.locf(moving_average(MM_h$deriv2[[sprintf("d=%i",2)]], -h) * last_est)

      info <- lapply(data, function(x){
        sigma2 <- var_estimator(x, MM_h[["henderson"]])
        list("LC" = list(
          `d=2` = as.numeric(tail(window(pente_d2, end = end(x)), h)),
          `sigma2` = sigma2
        ),
        "QL" = list(
          `d=2` = as.numeric(tail(window(courbure_d2, end = end(x)), h)),
          `sigma2` = sigma2
        )
        )
        
      })
      saveRDS(info, new_f)
    }
  }
}
