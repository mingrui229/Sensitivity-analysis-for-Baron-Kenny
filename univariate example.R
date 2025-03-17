source("bku.R")

### load the data 
library(haven)
dat <- read_sav("mediation_example.sav")
dat <- as.matrix(dat)
dat <- as.data.frame(dat)
colnames(dat) <- c("a", "m", "y")

### compute RV
set.seed(123)
result1 <- bku_rv(dat$y, dat$m, dat$a)

set.seed(123)
result2 <- bku_rv_direct(dat$y, dat$m, dat$a)

set.seed(123)
result3 <- bku_rv_indirect(dat$y, dat$m, dat$a)

### results with specified R parameters

bku_direct(dat$y, dat$m, dat$a, Ry = 0.3, Rm = -0.3, Ra = 0.3)
bku_indirect(dat$y, dat$m, dat$a, Ry = sqrt(0.2), Rm = sqrt(0.2), Ra = sqrt(0.2))

### worst-case results with specified rho's

bku_worst_direct(dat$y, dat$m, dat$a, rho_y = 0.15, rho_m = 0.15, rho_a = 0.15)
bku_worst_indirect(dat$y, dat$m, dat$a, rho_y = 0.27, rho_m = 0.27, rho_a = 0.27)

### plot
rho = c(1: 28) / 100
direct.t = result2[[3]][1:28, 4]
indirect.t = result3[[3]][1:28, 4]

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
plot(x = rho, y = indirect.t, ylim = c(0, 5), type = "l", lty = 2, 
     xlab = latex2exp::TeX("$rho$"), 
     ylab = latex2exp::TeX("min $t$ statistic"))
points(x = rho[1:16], y = direct.t[1:16], type = "l", lty = 1)
abline(h = 1.96, lty = 3, col = "grey")
abline(h = 0, lty = 3, col = "grey")
par(xpd=TRUE)
legend("topright", inset=c(-0.35,0), legend=c("Direct","Indirect"), lty=c(1,2), title="Effect")
