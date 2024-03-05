# Explore beta0 -----------------------------------------------------------

Explore_beta0_first <- function(beta_sub, W, prevalence) {
  seq_beta0 <- seq(-20, 0, 0.1)
  seq_beta <- cbind(
    seq_beta0, beta_sub[1],beta_sub[2], beta_sub[3], beta_sub[4]
  )
  
  E <- plogis(seq_beta %*% t(W))   # 各組み合わせの各個体のアウトカム発生確率を計算
  seq_prevalence <- apply(E, 1, mean)
  min_diff_prevalence <- which(abs(seq_prevalence - prevalence) == min(abs(seq_prevalence - prevalence)))
  beta0 <- seq_beta0[min_diff_prevalence]
  beta0
}

Explore_beta0 <- function(beta_sub, W, prevalence, beta0_first) {
  seq_beta0 <- seq(beta0_first - 0.01, beta0_first + 0.01, 0.0001)
  seq_beta <- cbind(
    seq_beta0, beta_sub[1],beta_sub[2], beta_sub[3], beta_sub[4]
  )
  
  E <- plogis(seq_beta %*% t(W))
  seq_prevalence <- apply(E, 1, mean)
  min_diff_prevalence <- which(abs(seq_prevalence - prevalence) == min(abs(seq_prevalence - prevalence)))
  beta0 <- seq_beta0[min_diff_prevalence]
  beta0
}


RD_separation_simulation <- function(data){
  
  n <- data$n
  prevalence <- data$prevalence
  RR <- data$RR
  RD <- data$RD
  prob.x <- data$prob.x
  
  
  RD_ols <- numeric(iteration)
  SE_ols <- numeric(iteration)
  SE_ols_HC0 <- numeric(iteration)
  SE_ols_HC1 <- numeric(iteration)
  SE_ols_HC2 <- numeric(iteration)
  SE_ols_HC3 <- numeric(iteration)
  SE_ols_HC4 <- numeric(iteration)
  
  prop_x <- numeric(iteration)
  n_separation <- numeric(iteration)
  
  i <- 1
  
  while (i <= iteration) {
    if (i %% 100 == 0) print(i)
    # set.seed(i)
    # Data generation
    z1 <- rbinom(n, 1, prob = 0.5)  # sex
    z2 <- rbeta(n, shape1 = 1, shape2 =2.5)*35 + 66  # age

    rate_z3 <- 4
    mu_z3 <- 100 /(z2)
    z3 <- ceiling(rbeta(n, shape1 = mu_z3*rate_z3, shape2 =rate_z3)*12)  #  number of hospital visit prior to date of exposure
    
    p_x <- plogis(-8.5+log(1.1)*z2 +log(1.2)*z3)
    x <- rbinom(n, 1, prob = p_x)
    # mean(x)
    
    W_x0 <- cbind(1, 0, z1, z2, z3)
    
    beta_sub <- c(log(RR), log(1), log(1.1), log(1.2))
    beta0 <- Explore_beta0_first(beta_sub, W_x0, prevalence)  # 粗く探索
    beta0_first <- beta0
    beta0 <- Explore_beta0(beta_sub, W_x0, prevalence, beta0_first)  # 細かく探索
    
    beta_p0 <- c(beta0, beta_sub)
    
    linear_predictor <- W_x0 %*% beta_p0
    p0 <- plogis(linear_predictor)
    p1 <- p0 + RD
    y_p0 <- rbinom(n, 1, p0)
    y_p1 <- rbinom(n, 1, p1)
    y <- y_p0*(1-x) + y_p1*x
    
    df <- data.frame(y, x, z1, z2, z3)
    
    # write_rds(df, paste0("01_Data/sim/df_", n, "_", prevalence, "_", RD, "_", i, "_231221.rds"))
    # 
    # df <- read_rds("01_Data/sim/df_1e+05_1e-05_2e-04_69_231221.rds")
    # x <- df$x
    # y <- df$y
    
    prop_x[i] <- mean(x)
    
    if (sum(y) == 0){
      easy_separation_check1 <- 0
    } else{
      easy_separation_check1 <- 1
    }
    
    if (easy_separation_check1 == 0){
      RD_ols[i] <- NA
      SE_ols[i] <- NA
      SE_ols_HC0[i] <- NA
      SE_ols_HC1[i] <- NA
      SE_ols_HC2[i] <- NA
      SE_ols_HC3[i] <- NA
      SE_ols_HC4[i] <- NA
      
      n_separation[i] <- 2
    } 
    
    if (easy_separation_check1 != 0){
      
      temp_table <- table(y,x)
      easy_separation_check2 <- as.numeric(temp_table[1,1]) * as.numeric(temp_table[1,2]) * as.numeric(temp_table[2,1]) * as.numeric(temp_table[2,2])
      
      # Separation --------------------------------------------------------------
      
      if (easy_separation_check2 == 0){
        n_separation[i] <- TRUE
        
        # Risk difference
        # OLS base----------------------------------------------------------
        fit_ols <- lm(y ~ x + z1, data = df)
        RD_ols[i] <- fit_ols$coefficients[2]
        SE_ols[i] <- summary(fit_ols)$coefficients[2,2]
        SE_ols_HC0[i] <- sqrt(vcovHC(fit_ols, type = "HC0")[2,2])
        SE_ols_HC1[i] <- sqrt(vcovHC(fit_ols, type = "HC1")[2,2])
        SE_ols_HC2[i] <- sqrt(vcovHC(fit_ols, type = "HC2")[2,2])
        SE_ols_HC3[i] <- sqrt(vcovHC(fit_ols, type = "HC3")[2,2])
        SE_ols_HC4[i] <- sqrt(vcovHC(fit_ols, type = "HC4")[2,2])
      } else {
        # Separation --------------------------------------------------------------
        # fit_separation <- glm(y ~ x + z1 + z2 + z3, data = df, family = binomial(link = "logit"),
        #                       method = "detect_separation")
        # n_separation[i] <- fit_separation$outcome
        n_separation[i] <- 3
        
        # Risk difference
        # OLS base----------------------------------------------------------
        fit_ols <- lm(y ~ x + z1 + z2 + z3, data = df)
        RD_ols[i] <- fit_ols$coefficients[2]
        SE_ols[i] <- summary(fit_ols)$coefficients[2,2]
        SE_ols_HC0[i] <- sqrt(vcovHC(fit_ols, type = "HC0")[2,2])
        SE_ols_HC1[i] <- sqrt(vcovHC(fit_ols, type = "HC1")[2,2])
        SE_ols_HC2[i] <- sqrt(vcovHC(fit_ols, type = "HC2")[2,2])
        SE_ols_HC3[i] <- sqrt(vcovHC(fit_ols, type = "HC3")[2,2])
        SE_ols_HC4[i] <- sqrt(vcovHC(fit_ols, type = "HC4")[2,2])
      }
    }
    if (easy_separation_check1 == 0){
      i <- i
    } else {
      i <- i + 1
    }
  }
  list(
    n = n,
    prevalence = prevalence,
    prob.x = prob.x,
    RD = RD,
    RR = RR,
    
    RD_ols = RD_ols,
    SE_ols = SE_ols,
    SE_ols_HC0 = SE_ols_HC0,
    SE_ols_HC1 = SE_ols_HC1,
    SE_ols_HC2 = SE_ols_HC2,
    SE_ols_HC3 = SE_ols_HC3,
    SE_ols_HC4 = SE_ols_HC4,
    
    n_separation = n_separation,
    prop_x = prop_x
  )
}

