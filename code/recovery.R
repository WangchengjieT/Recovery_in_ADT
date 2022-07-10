######  Recoverability Effects on Reliability Assessment for Accelerated Degradation Testing
#   Output:
#   Table 1 to 4 and Figure 2 in the paper
###### maintained by Chengjie Wang


######### packages to load
#library(ggplot2)
#library(reshape2)
#library(nlme)
#library(MASS)
#library(statmod)
#############################################

##----functions------

##record parameter theta = (bet0, bet1, sig)
"get.pv0" <- function(beta0, beta1, sigma){
  true.parameters <- c(beta0 = beta0, beta1 = beta1, sigma = sigma)
  return(true.parameters)
}

##record parameter theta_tilde = (bet0, bet1, sig, mu.xi, sig.xi)
"get.pv" <- function(beta0, beta1, sigma, mu.xi, sigma.xi){
  true.parameters <- c(beta0 = beta0, beta1 = beta1, sigma = sigma, 
                       mu.xi = mu.xi, sigma.xi = sigma.xi)
  return(true.parameters)
}  

##test plan
"get.test.plan" <- function(n, str.lev, mea.lev = NULL, time.vec, 
                            method = 0, time.units = "Time"){
  if(method == 0) mea.level <- str.lev
  else mea.level <- mea.lev
  if(is.list(time.vec))
    the.plan <- list(sample.sizes = n, stress.level = str.lev, 
                     measurement.level = mea.level,
                     time.vectors = time.vec)
  else the.plan <- list(sample.sizes = n, stress.level = str.lev, 
                        measurement.level = mea.level,
                        time.vectors = list(time.vec))
  attr(the.plan, "time.units") <- time.units
  attr(the.plan, "method") <- 1 - (method == 0)
  return(the.plan)
}

##data simulation
data.sim <- function(pv, plan, isplot = FALSE){

  require(MASS)
  require(ggplot2)
  
  ns <- plan$sample.sizes
  str.lev <- plan$stress.level
  mea.lev <- plan$measurement.level
  
  ##initialization
  Y <- NULL
  Y.sim <- NULL
  del.Y <- NULL
  del.Y.sim <- NULL
  unit <- NULL
  time <- NULL
  del.time <- NULL
  count <- 1
  time.vector <- NULL
  stress.lev <- NULL
  flag <- NULL
  
  ##generate data
  for(j in 1:length(ns)){
    n <- ns[j]
    time.span <- plan$time.vectors[[j]]
    len <- length(time.span)
    del.time.span <- time.span - c(0, time.span[-len])
    str <- str.lev[j]
    for(i in 1:n){
      del.D.i <- rnorm(len, mean = (pv[1] + pv[2] * str) * del.time.span, sd = sqrt(pv[3] * del.time.span))
      del.Y <- c(del.Y, del.D.i)
      Y <- c(Y, cumsum(del.D.i))
      
      del.D.i.sim <- del.D.i + rnorm(1, mean = pv[4], sd = sqrt(pv[5]))
      del.Y.sim <- c(del.Y.sim, del.D.i.sim)
      Y.sim <- c(Y.sim, cumsum(del.D.i.sim))
      
      unit <- c(unit, rep(count, len))
      time <- c(time, time.span)
      del.time <- c(del.time, del.time.span)
      time.vector <- c(time.vector, rep(j, len))
      stress.lev <- c(stress.lev, rep(str, len))
      flag <- c(flag, str * del.time.span)
      count <- count + 1
    }
  }
  
  ##generate data.frame
  dat <- data.frame(unit = factor(unit), time.vector = factor(time.vector), 
                    time = time, diff.time = del.time, stress.level = stress.lev, 
                    deg = Y, diff.deg = del.Y, deg.sim = Y.sim, diff.deg.sim = del.Y.sim, 
                    flag = flag)
  attr(dat, "method") <- attr(plan, "method")
  
  if(isplot){#plot
    deg.plot <- ggplot(data = dat, aes(x = time, y = deg, group = unit))
    print(deg.plot + geom_path() + 
            geom_path(aes(x = time, y = deg.sim), color = "blue") + facet_wrap(~ stress.level))
  }   
  
  return(dat)
}

##lse
ls.cal <- function(dat){
  len <- length(dat[,1])
  
  X1 <- matrix(0, nrow = len, ncol = 2)
  X1[,1] <- dat$diff.time
  X1[,2] <- dat$flag
  Sig.inv1 <- diag(1 / dat$diff.time)
  temp.coef1 <- solve(t(X1) %*% Sig.inv1 %*% X1) %*% t(X1) %*% Sig.inv1
  temp.beta1 <- temp.coef1 %*% dat$diff.deg
  temp.res1 <- dat$diff.deg - X1 %*% temp.beta1
  temp.sigma1 <- (t(temp.res1) %*% Sig.inv1 %*% temp.res1) / (len - 2)
  ests1 <- c(temp.beta1, temp.sigma1)
  
  temp.beta3 <-  temp.coef1 %*% dat$diff.deg.sim
  temp.res3 <- dat$diff.deg.sim - X1 %*% temp.beta3
  temp.sigma3 <- (t(temp.res3) %*% Sig.inv1 %*% temp.res3) / (len - 2)
  ests3 <- c(temp.beta3, temp.sigma3)
  
  Q <- matrix(0, nrow = len, ncol = len)
  pre <- 0
  for(i in 1:dat$unit[len]){
    last <- pre
    while(pre < len && dat$unit[pre + 1] == i )  pre <- pre + 1
    Di <- diag(dat$diff.time[(last+1):pre])
    Ai <- rbind(diag(1, pre - last - 1), -1)
    Q[(last+1):pre,(last+1):pre] <- Ai %*% solve(t(Ai) %*% Di %*% Ai) %*% t(Ai)
  }
  temp.beta2 <-  solve(t(X1) %*% Q %*% X1) %*% t(X1) %*% Q %*% dat$diff.deg.sim
  temp.res2 <- dat$diff.deg.sim - X1 %*% temp.beta2
  temp.sigma2 <- (t(temp.res2) %*% Q %*% temp.res2)/ (len - as.numeric(dat$unit[len]) - 2)
  ests2 <- c(temp.beta2, temp.sigma2)
  
  if(0){
    est.mat <- matrix(0, nrow = 5, ncol = 3)
    est.mat[1:3, 1] <- ests1
    est.mat[, 2] <- ests2
    est.mat[1:3, 3] <- ests3
  }
  
  return(c(ests1, ests2, ests3))
}

##mle
mle.cal <- function(dat){
  require("nlme")
  
  fit1 <- gls(diff.deg ~ diff.time + flag - 1, data = dat, 
              weights = varFixed(~ diff.time))
  fit2 <- lme(diff.deg.sim ~ diff.time + flag , data = dat, 
              random = ~ 1|unit, weights = varFixed(~ diff.time))
  fit3 <- gls(diff.deg.sim ~ diff.time + flag - 1, data = dat, 
              weights = varFixed(~ diff.time))
  
  est.mat <- matrix(0, nrow = 5, ncol = 3)
  est.mat[1:2, 1] <- fit1$coefficients
  est.mat[3, 1] <- fit1$sigma ^ 2
  est.mat[c(4, 1, 2), 2] <- fit2$coefficients$fixed
  est.mat[3, 2] <- summary(fit2)$sigma ^ 2
  est.mat[5, 2] <- getVarCov(fit2)[1]
  est.mat[1:2, 3] <- fit3$coefficients
  est.mat[3, 3] <- fit3$sigma ^ 2
   
  return(est.mat)
}

##lse simulation
ls.sims <- function(pv, plan, nsim){
  
  ests <- matrix(0, nrow = nsim, ncol = 9)
  for(i in 1:nsim){
    ests[i,] <- ls.cal(data.sim(pv, plan))
  }
  
  return(ests) 
}

##mle simulation
mle.sims <- function(pv, plan, nsim){
  if(0){
    ests <- matrix(0, ncol = nsim, nrow = 15)
    for(i in 1:nsim){
      ests[, i] <- as.vector(mle.cal(data.sim(pv, plan)))
    }
  }
  
  return(replicate(nsim, as.vector(mle.cal(data.sim(hp.pv, hp.plan)))))
}

##life quantil
life.quantile <- function(pv, p, deg.fail){
  require(statmod)
  
  return(qinvgauss(p, mean = deg.fail / pv[1], shape = deg.fail ^ 2 / pv[3]))
}

##reliability
reli.inv <- function(pv, p, deg.fail){
  require(statmod)
  
  return(1 - pinvgauss(p, mean = deg.fail / pv[1], shape = deg.fail ^ 2 / pv[3]))
}

##test
{
  hp.pv <- get.pv(7.9, 6.2, 1.6, -0.6, 1)
  n <- c(5, 5)
  str.lev <- c(0.5, 1)
  time.vec <- list()
  for(i in 1:length(n)){
    time.vec[[i]] <- cumsum(runif(n = 10, min = 0.5, max = 1.5))
  }
  hp.plan <- get.test.plan(n, str.lev, time.vec = time.vec, method = 1)
  Df <- 70
  N <- 10000
}
if(0){
  {
    temp.mle.sim <- ls.sims(hp.pv, hp.plan, nsim = 1)
    print(matrix(temp.mle.sim, nrow = 3, byrow = 3))
    {
      temp.mle.sim <- ls.sims(hp.pv, hp.plan, nsim = 1)
      print(matrix(temp.mle.sim, nrow = 3, byrow = 3))
      
      p = (1:99)/100
      quant <- data.frame(p = rep(p, 4), 
                          Cases = factor(rep(c("True value", "Case 1", "Case 2", "Case 3"), each = length(p))))
      life1 <- life2 <- life3<- life4<- c()
      for(i in 1:length(p)){
        life1[i] <- life.quantile(temp.mle.sim[1, 1:3], p[i], Df)
        life2[i] <- life.quantile(temp.mle.sim[1, 4:6], p[i], Df)
        life3[i] <- life.quantile(temp.mle.sim[1, 7:9], p[i], Df)
        life4[i] <- life.quantile(hp.pv, p[i], Df)
      }
      quant$Quantile <- c(life4, life1, life2, life3)
      print(signif(quant$Quantile[c(4, 9, 24, 49, 74, 89) + 99 * 0], 4))
      print(signif(quant$Quantile[c(4, 9, 24, 49, 74, 89) + 99 * 1], 4))
      print(signif(quant$Quantile[c(4, 9, 24, 49, 74, 89) + 99 * 2], 4))
      print(signif(quant$Quantile[c(4, 9, 24, 49, 74, 89) + 99 * 3], 4))
      life.com <- ggplot(data = quant, aes(linetype = Cases, color = Cases))
      plot(life.com + geom_line(aes(x = p, y = Quantile), size = 1) + 
             theme(panel.background = element_rect(color = "black", fill = NA), 
                   panel.grid.major = element_line(linetype = 5, color = "grey80")))
      plot(life.com + geom_line(aes(x = p, y = Quantile), size = 1))
      plot(life.com + geom_line(aes(x = p, y = Quantile)) + 
             theme(panel.background = element_rect(color = "black", fill = NA), 
                   panel.grid.major = element_line(linetype = 5, color = "grey80")))
      plot(life.com + geom_line(aes(x = p, y = Quantile), linetype = 1) + 
             theme(panel.background = element_rect(color = "black", fill = NA), 
                   panel.grid.major = element_line(linetype = 5, color = "grey80")))
    }
    {
      ti = seq(0, 13, length.out = 1000)
      re <- data.frame(Time = rep(ti, 4), 
                       Cases = factor(rep(c("True value", "Case 1", "Case 2", "Case 3"), each = length(ti))))
      life1 <- life2 <- life3<- life4<- c()
      for(i in 1:length(ti)){
        life1[i] <- reli.inv(temp.mle.sim[1, 1:3], ti[i], Df)
        life2[i] <- reli.inv(temp.mle.sim[1, 4:6], ti[i], Df)
        life3[i] <- reli.inv(temp.mle.sim[1, 7:9], ti[i], Df)
        life4[i] <- reli.inv(hp.pv, ti[i], Df)
      }
      re$Reliability <- c(life4, life1, life2, life3)
      re.com <- ggplot(data = re, aes(linetype = Cases, color = Cases))
      plot(re.com + geom_line(aes(x = Time, y = Reliability)) + 
             theme(panel.background = element_rect(color = "black", fill = NA), 
                   panel.grid.major = element_line(linetype = 5, color = "grey80")))
      
    }
    {
      ti = seq(0, 20, length.out = 1000)
      re <- data.frame(Time = 10 ^ rep(ti, 2), 
                       Cases = factor(rep(c("Case 2", "Case 3"), each = length(ti))))
      life1 <- life2 <- life3<- life4<- c()
      for(i in 1:length(ti)){
        life2[i] <- reli.inv(c(0.0892,	0.0913,	0.00695), ti[i], 0.8)
        life3[i] <- reli.inv(c(0.0886,	0.126,	0.00542), ti[i], 0.8)
      }
      re$Reliability <- c(life2, life3)
      re.com <- ggplot(data = re, aes(linetype = Cases, color = Cases))
      plot(re.com + geom_line(aes(x = Time, y = Reliability)) + 
             theme(panel.background = element_rect(color = "black", fill = NA), 
                   panel.grid.major = element_line(linetype = 5, color = "grey80"),
                   legend.title = element_text(size = 0)) +
             scale_x_log10())
      
    }
    {
      pr = c(0.01, 0.05, 0.10, 0.2, 0.3, 0.4, 0.5)
      quan2 <- quan3 <- c()
      for(i in 1:length(pr)){
        quan2[i] <- life.quantile(c(0.0892,	0.0913,	0.00695), pr[i], 0.8)
        quan3[i] <- life.quantile(c(0.0886,	0.126,	0.00542), pr[i], 0.8)
      }
      print(rbind(quan2, quan3))
      print(10 ^ rbind(quan2, quan3) / 365 / 86400)
    }
    plot(life.com + geom_line(aes(x = p, y = Quantile)) + 
           theme(panel.background = element_rect(color = "black", fill = NA), 
                 panel.grid.major = element_line(linetype = 5, color = "grey80")))
  }
  
  system.time(
    {
      bias <- temp.mle.sim <- ls.sims(hp.pv, hp.plan, nsim = N)
      print(apply(temp.mle.sim, 2, mean))
      for(i in 1:length(temp.mle.sim[,1])){
        bias[i,] <- bias[i,] - rep(hp.pv[1:3], 3)
      }
      aa <- rbind(apply(bias, 2, mean), apply(bias ^ 2, 2, mean))
      print(aa)
      bb <- c(aa)
      bb <- cbind(matrix(c(aa), nrow = 3, byrow = TRUE), 
                  Df / (aa[1, c(1, 4, 7)] + 7.9), 
                  7.9 / (aa[1, c(1, 4, 7)] + 7.9) -1)
      print(signif(bb, 3))
    }
  )
}

##probability of overestimation
para.prob.overestimate <- function(pv, plan){
  ns <- plan$sample.sizes
  str.lev <- plan$stress.level
  n <- sum(ns)
  S_T <- NULL
  TT <- NULL
  for(i in 1:length(ns)){
    temp_t <- rep(diff(c(0, plan$time.vectors[[i]])), ns[i])
    TT <- c(TT, temp_t)
    S_T <- c(S_T, temp_t * str.lev[i])
  }
  X <- cbind(TT, S_T)
  M <- length(TT)
  
  temp_inv <- solve(t(X) %*% diag(1 / TT) %*% X)
  mu_star <-  temp_inv %*% t(X) %*% diag(1 / TT) %*% rep(1, M)
  mu_star <- -mu_star[1]
  mu_star <- mu_star * hp.pv[4]
  D_star <- matrix(0, nrow = M, ncol = M)
  flag <- 0
  for(i in 1:length(ns)){
    temp_len <- length(plan$time.vectors[[i]])
    for(j in 1:ns[i]){
      D_star[(flag + 1):(flag + temp_len), (flag + 1):(flag + temp_len)] <- 1
      flag <- flag + temp_len
    }
  }
  sigma2_star <- temp_inv %*% t(X) %*% diag(1 / TT) %*% D_star %*% diag(1 / TT) %*% X %*% temp_inv
  sigma2_star <- pv[5] * sigma2_star[1, 1] + pv[3] * temp_inv[1, 1]
  
  return(c(mu_star, sqrt(sigma2_star), pnorm(mu_star / sqrt(sigma2_star))))
}


##----main_part------

{##parameter setting
  hp.pv <- get.pv(7.9, 6.2, 1.6, -0.6, 0.1)
  n <- c(3, 3)
  str.lev <- c(0.5, 1)
  time.vec <- list()
  for(i in 1:length(n)){
    time.vec[[i]] <- cumsum(runif(n = 5, min = 0.5, max = 1.5))
  }
  hp.plan <- get.test.plan(n, str.lev, time.vec = time.vec, method = 1)
  Df <- 70
  N <- 10^5
}

{##single
  system.time({
    bias <- temp.mle.sim <- mle.sims(hp.pv, hp.plan, nsim = N)
    bias <- bias - rep(hp.pv, 3)
    aa <- rbind(apply(bias, 1, mean), sqrt(apply(bias ^ 2, 1, mean)))
    print(aa)
    bb <- c(aa)
    bb <- matrix(c(aa), ncol = 3, byrow = FALSE)
    bb <- rbind(bb, Df / (hp.pv[1] + bb[1, ]), hp.pv[1] / (hp.pv[1] + bb[1, ]) - 1)
    bb[-11, ] = bb[-11, ] * 100
    bb <- rbind(bb, apply(temp.mle.sim[c(1, 6, 11), ] < hp.pv[1], 1, mean))
    print(signif(bb, 3))
    cat(signif(t(bb), 3), sep = "\t")
  })
}

{
  library(ggplot2)
  sram.origin <- read.csv("tcdv.csv", header = FALSE)
  col.base <- sram.origin[1:2, 2:3]
  row.base <- sram.origin[4:5, 2:6]
  sram.time <- as.numeric((sram.origin[8, 2:6] - 252) * 
                            4 / (1380 - 252))
  sram.tcdv <- (sram.origin[c(11:13, 16:18), 2:6] - 45) * 
    (-1 - 0) / (1494 - 45) + 0
  sram.tcdv.origin <- 10 ^ sram.tcdv
  sram.data <- data.frame("Tcdv" = 10 ^ matrix(t(sram.tcdv), ncol = 1),
                          "Time" = rep(10 ^ sram.time, 6),
                          "Voltage" = rep(c("1.6V","1.8V"), each = 15),
                          "Sample" = factor(rep(1:6, each = 5)))
  sram.plot <- ggplot(data = sram.data, aes(x = Time, y = Tcdv, 
                                            group = Sample, color = Voltage))
  print(sram.plot + geom_line() + geom_point(aes(shape = Sample), show.legend = FALSE) + 
          coord_cartesian(xlim = c(1, 10 ^ 4.1), ylim = c(0.05, 0.4)) + 
          theme(panel.background = element_rect(color = "black", fill = NA), 
                panel.grid.major = element_line(linetype = 5, color = "grey80")))
  print(sram.plot + geom_line() + geom_point(aes(shape = Sample), show.legend = FALSE) +
          scale_x_log10() + coord_cartesian(xlim = c(1, 10 ^ 4.1), ylim = c(0.05, 0.5)) + 
          theme(panel.background = element_rect(color = "black", fill = NA), 
                panel.grid.major = element_line(linetype = 5, color = "grey80")))
  print(sram.plot + geom_line(aes(linetype = Voltage)) + geom_point(aes(shape = Sample), show.legend = FALSE) +
          scale_x_log10() + scale_y_log10() + coord_cartesian(xlim = c(1, 10 ^ 4.1), ylim = c(0.05, 1)) + 
          theme(panel.background = element_rect(color = "black", fill = NA), 
                panel.grid.major = element_line(linetype = 5, color = "grey80")) +
          xlab("Time(s)"))
  
  sram.data$unit <- factor(rep(1:6, each = 5))
  #  sram.data$str <- rep(c((log(1.6) - log(1.2)) / (log(1.8) - log(1.2)), 1), each = 15)
  sram.data$str <- rep(c(1.6, 1.8), each = 15)
  sram.data$diff.time <- rep(sram.time - c(0, sram.time[-5]), 6)
  sram.data$diff.deg.sim <- sram.data$diff.deg <- 
    matrix(t(sram.tcdv.origin - cbind(0, sram.tcdv.origin[, -5])), ncol = 1)
  sram.data$flag <- sram.data$str * sram.data$diff.time
  ls.cal(sram.data)
}

{
  hp.pv.mat <- matrix(c(7.9, 6.2, 1.6, -0.6, 0.1,
                    7.9, 6.2, 1.6, -1.2, 0.1,
                    7.9, 6.2, 1.6, -0.6, 1), nrow = 3, byrow = TRUE)
  n.mat <- matrix(c(3, 3, 5, 5, 5, 5, 15, 15, 25, 25),  nrow = 5, byrow = TRUE)
  m.mat <- c(5, 10, 20, 20, 20)
  str.lev <- c(0.5, 1)
  
  hp.plan.list <- list()
  for(j in 1:5){
    time.vec <- list()
    for(k in 1:length(n)){
      time.vec[[k]] <- cumsum(runif(n = m.mat[j], min = 0.5, max = 1.5))
    }
    hp.plan.list[[j]] <- get.test.plan(n.mat[j, ], str.lev, time.vec = time.vec, method = 1)
  }
  
  Df <- 70
  N <- 10^5
}

system.time({ 
  table_sec3 <- list()
  for(i in 1:nrow(hp.pv.mat)){
    table_temp <- NULL
    hp.pv <- get.pv(hp.pv.mat[i, 1], hp.pv.mat[i, 2], 
                    hp.pv.mat[i, 3], hp.pv.mat[i, 4], hp.pv.mat[i, 5])
    be0 <- hp.pv.mat[i, 1]
    for(j in 1:nrow(n.mat)){
      hp.plan <- hp.plan.list[[j]]
      
      bias <- temp.mle.sim <- mle.sims(hp.pv, hp.plan, nsim = N)
      bias <- bias - rep(hp.pv, 3)
      aa <- rbind(apply(bias, 1, mean), sqrt(apply(bias ^ 2, 1, mean)))
      bb <- c(aa)
      bb <- matrix(c(aa), ncol = 3, byrow = FALSE)
      bb <- rbind(bb, Df / (hp.pv[1] + bb[1, ]), hp.pv[1] / (hp.pv[1] + bb[1, ]) - 1)
      bb[-11, ] = bb[-11, ] * 100
      bb <- rbind(bb, apply(temp.mle.sim[c(1, 6, 11), ] < hp.pv[1], 1, mean))
      table_temp <- cbind(table_temp, bb)
    }
    table_sec3[[i]] <- table_temp
  }
})

{
  sec3tabsig3 <- lapply(table_sec3, signif, 3)
  write.csv(rbind(sec3tabsig3[[1]], 0, 0,
                  sec3tabsig3[[2]], 0, 0,
                  sec3tabsig3[[3]]), file = "tables.csv")
}



