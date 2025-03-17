source("bku.R")

### load the data
dat <- read.csv("Chong et al Final Date Set.csv")
c <- matrix(nrow = 215, ncol = 11)
m <- matrix(nrow = 215, ncol = 2)

### treatment: 0=Control Group, 1=Treatment (Physician)
a <- as.numeric(dat$treatment == "Physician")

### outcome: Sum of Average of Last 2 Quarter Grades
y <- as.numeric(dat$gradesq34)

### mediators - Anemia and Cognitive Function
### mediator1: Anemia Status at Follow-Up Survey
m[, 1] <- as.numeric(dat$anemic == "Yes")
### mediator2: Total Wii Game Score
m[, 2] <- as.numeric(dat$wii_total)

### covariates
c[, 1] <- as.numeric(dat$male == "Yes")
c[, 2] <- as.numeric(dat$anemic_base_re == "Yes")
c[, 3] <- as.numeric(dat$hh_total_inc_hun)
c[, 4] <- as.numeric(dat$hh_elec_re == "Yes")
c[, 5] <- as.numeric(dat$hh_elec_miss == "0")
c[, 6] <- as.numeric(dat$hh_mother_edu_re)
c[, 7] <- as.numeric(dat$hh_mother_edu_miss == "0")
c[, 8] <- as.numeric(dat$class_level == "2")
c[, 9] <- as.numeric(dat$class_level == "3")
c[, 10] <- as.numeric(dat$class_level == "4")
c[, 11] <- as.numeric(dat$class_level == "5")

### removing missing values
missing_obs <- c(31, 38, 62, 68, 141, 152, 153, 155, 192, 206)

### removing soccer players
soccer <- which(dat$treatment == "Soccer Player")

a = a[-union(missing_obs, soccer)]
y = y[-union(missing_obs, soccer)]
m = m[-union(missing_obs, soccer), ]
c = c[-union(missing_obs, soccer), ]
n = length(a)

### compute RV

set.seed(123)
result1 <- bku_rv(y, m, a, c, randomized = T)

set.seed(123)
result2 <- bku_rv_direct(y, m, a, c, randomized = T)

set.seed(123)
result3 <- bku_rv_indirect(y, m, a, c, randomized = T)

### worst-case results with specified rho's

bku_worst_direct(y, m, a, c, rho_y = 0.39, rho_m = 0.39, rho_a = 0)
bku_worst_indirect(y, m, a, c, rho_y = 0.26, rho_m = 0.26, rho_a = 0)

### plot
rho = c(1: 39) / 100
direct.t = result2[[3]][1:39, 4]
indirect.t = result3[[3]][1:39, 4]

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
plot(x = rho, y = direct.t, ylim = c(0, 2), type = "l", lty = 1, 
     xlab = latex2exp::TeX("$rho$"), 
     ylab = latex2exp::TeX("min $t$ statistic"))
points(x = rho[1:27], y = indirect.t[1:27], type = "l", lty = 2)
abline(h = 1.96, lty = 3, col = "grey")
abline(h = 0, lty = 3, col = "grey")
par(xpd=TRUE)
legend("topright", inset=c(-0.4,0), legend=c("Direct","Indirect"), lty=c(1,2), title="Effect")

### formal benchmarking

formalresult <- matrix(nrow = 11, ncol = 4)
for (i in c(1: 11)){
  formalresult[i, ] <- bku_fb_point(y, m, a, c, i, 1, 1, 0)
  print(formalresult[i, ])
}

library(tidyverse)
library(broom)
tidy_reg <- data.frame(term = c("male", "anemic_base_re", "hh_total_inc_hun", "hh_elec_re", "hh_elec_miss",
                                "hh_mother_edu_re", "hh_mother_edu_miss", "class_level_2", "class_level_3",
                                "class_level_4", "class_level_5"),
                       direct = formalresult[,1],
                       indirect = formalresult[,2])
theme_set(theme_light())
tidy_reg %>%
  # reorder the coefficients so that the largest is at the top of the plot
  mutate(term = fct_reorder(term, direct, .desc = T)) %>%
  ggplot(aes(direct, term)) +
  xlim(0, 0.2210030) +
  geom_vline(xintercept = 0.2210030, lty = 2) +
  geom_point() +
  labs(
    x = "worst point estimate",
    y = NULL,
  )

tidy_reg %>%
  # reorder the coefficients so that the largest is at the top of the plot
  mutate(term = fct_reorder(term, indirect, .desc = T)) %>%
  ggplot(aes(indirect, term)) +
  xlim(0, 0.1383919) +
  geom_vline(xintercept = 0.1383919, lty = 2) +
  geom_point() +
  labs(
    x = "worst point estimate",
    y = NULL,
  )


formalresult2 <- matrix(nrow = 60, ncol = 2)
for (i in c(1: 60)){
  formalresult2[i, ] <- bku_fb_point(y, m, a, c, 1, i/10, i/10, 0)
  print(formalresult2[i, ])
}

### plot
rho = c(1: 54) / 10
direct.p = formalresult2[1:54, 1]
indirect.p = formalresult2[1:54, 2]

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=FALSE)
plot(x = rho, y = direct.p, ylim = c(0, 0.22), type = "l", lty = 1, 
     xlab = latex2exp::TeX("$Delta$"), 
     ylab = latex2exp::TeX("min point estimate"))
points(x = rho[1:39], y = indirect.p[1:39], type = "l", lty = 2)
abline(h = 0, lty = 3, col = "grey")
par(xpd=TRUE)
legend("topright", inset=c(-0.4,0), legend=c("Direct","Indirect"), lty=c(1,2), title="Effect")