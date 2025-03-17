source("bku.R")

library(MASS)
n = 200
dimm <- c(2, 4, 6)
lambda1 <- c(0.3143, 0.6172, 0.9428, 1.4402)
lambda2 <- c(0.1, 0.3, 0.5, 0.7)

sim.result <- array(dim = c(20, 3, 4, 4, 4))
for (i1 in c(1: 3))
  for (i2 in c(1: 4))
    for (i3 in c(1: 4))
      for (i4 in c(1: 20))
        if (is.na(sim.result[i4, i1, i2, i3, 1])){
        print(c(i1,i2,i3,i4))
        print(Sys.time())
        Sgm <- matrix(nrow = dimm[i1], ncol = dimm[i1])
        for (i in c(1: dimm[i1]))
          for (j in c(1: dimm[i1]))
            Sgm[i, j] <- 0.5^(abs(i-j))
        
        theta1 <- lambda1[i2] * seq(from = 0, to = 1, length.out = dimm[i1])
        beta0 <- seq(from = 1, to = -0.5, length.out = dimm[i1])
        beta1 <- sqrt(1/(1-lambda2[[i3]])-1) * beta0 / c(sqrt(t(beta0) %*% Sgm %*% beta0))
        
        a <- rnorm(n)
        m <- a %*% t(theta1) + mvrnorm(n, rep(0, dimm[i1]), Sgm)
        y <- a + m %*% beta1 + rnorm(n)
        
        
        res1 = bku_rv_indirect(y, m, a, vector_u = F)
        res2 = bku_rv_indirect(y, m, a, vector_u = T)
        sim.result[i4, i1, i2, i3, 1] <- res1[[1]][4]
        sim.result[i4, i1, i2, i3, 2] <- res2[[1]][4]
        sim.result[i4, i1, i2, i3, 3] <- res1[[1]][5]
        sim.result[i4, i1, i2, i3, 4] <- res2[[1]][5]
        print(sim.result[i4, i1, i2, i3, ])
      }


### ANOVA
result <- matrix(nrow = 20 * 3 * 4 * 4, ncol =  4)
k <- 1
for (i1 in c(1: 3))
  for (i2 in c(1: 4))
    for (i3 in c(1: 4))
      for (i4 in c(1: 20)){
        result[k, 1] <- c(2, 4, 6)[i1]
        result[k, 2] <- c(0.1, 0.3, 0.5, 0.7)[i2]
        result[k, 3] <- c(0.1, 0.3, 0.5, 0.7)[i3]
        result[k, 4] <- sim.result[i4, i1, i2, i3, 2] / sim.result[i4, i1, i2, i3, 1]
        k <- k+1
      }

result <- as.data.frame(result)
summary(aov(V4~as.factor(V1)+as.factor(V2)+as.factor(V3), data = result))

result <- matrix(nrow = 20 * 3 * 4 * 4, ncol =  4)
k <- 1
for (i1 in c(1: 3))
  for (i2 in c(1: 4))
    for (i3 in c(1: 4))
      for (i4 in c(1: 20)){
        result[k, 1] <- c(2, 4, 6)[i1]
        result[k, 2] <- c(0.1, 0.3, 0.5, 0.7)[i2]
        result[k, 3] <- c(0.1, 0.3, 0.5, 0.7)[i3]
        result[k, 4] <- sim.result[i4, i1, i2, i3, 4] / sim.result[i4, i1, i2, i3, 3]
        k <- k+1
      }

result <- as.data.frame(result)
result[which(result[, 4] == Inf), 4] = NA
summary(aov(V4~as.factor(V1)+as.factor(V2)+as.factor(V3), data = result))
