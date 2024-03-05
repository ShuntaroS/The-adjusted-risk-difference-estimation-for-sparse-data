# Set up ------------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("logistf")
library(tidyverse)
# library(logistf)
# library(brglm2)
library(sandwich)
library(tictoc)
library(parallel)
# library(scales)
# library(detectseparation)

mycore <- detectCores()
# Separation simulation ---------------------------------------------------

source("04_Script/Functions/module3.R")

set.seed(1234)
iteration <- 2000
# n <- 1000
n <- c(1000, 10000, 100000)
num_increase <- c(0, 5, 10, 20 ,50)
prob.x <- c(0.05)  #結局使ってない、prob.xは大体0.9

df_sim <- expand_grid(n, num_increase, prob.x) %>% 
  mutate(prevalence = 1/n) %>%
  mutate(RD = prevalence + (num_increase/n)  - prevalence) %>% 
  mutate(RR = (prevalence + RD)/prevalence) %>%
  mutate(no = row_number())

data <- df_sim %>% 
  # filter(no %in% c(2)) %>%
  # filter(no %in% c(4, 5, 20, 21)) %>%
  mutate(no = row_number())
df_sim <- data

tic()
result_sim <- lapply(df_sim$no, function(i) {
  out <- RD_separation_simulation(df_sim[i,])
  cat(i, "/", dim(df_sim)[1], "Done!\n")
  tibble(
    n = out$n,
    prevalence = out$prevalence,
    prob.x = out$prob.x,
    RD_true = out$RD,
    RR_true = out$RR,
    
    n_separation = out$n_separation,
    prop_x = out$prop_x,
    
    RD_ols = out$RD_ols,
    SE_ols = out$SE_ols,
    SE_ols_HC0 = out$SE_ols_HC0,
    SE_ols_HC1 = out$SE_ols_HC1,
    SE_ols_HC2 = out$SE_ols_HC2,
    SE_ols_HC3 = out$SE_ols_HC3,
    SE_ols_HC4 = out$SE_ols_HC4
  )
}
# , mc.cores = mycore
)
toc()

df_result <- bind_rows(result_sim)

write_rds(df_result, "03_Output/result_module3_2402013.rds")

df_result <- read_rds("03_Output/result_module3_2402013.rds")

# Summary of RD ---------------------------------------------------------

df_RD_naive <- df_result %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, RD_ols, SE_ols) %>% 
  rename(est = RD_ols) %>% 
  rename(SE = SE_ols) %>% 
  mutate(method = "RD + naive SE")

df_RD_robust <- df_result %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, RD_ols, SE_ols_robust) %>% 
  rename(est = RD_ols) %>% 
  rename(SE = SE_ols_robust) %>% 
  mutate(method = "RD + robust SE")

df_RD <- bind_rows(df_RD_naive, df_RD_robust)

tab_RD <- df_RD %>% 
  group_by(n, prevalence, RD_true, method) %>% 
  summarise(expectation = mean(est, na.rm = TRUE),
            expectation_sep = mean(est[!n_separation], na.rm = TRUE),
            bias = mean(est - RD_true, na.rm = TRUE),
            bias_sep = mean((est - RD_true)[!n_separation], na.rm = TRUE),
            rmse = sqrt(mean((est - RD_true)^2)),
            rmse_sep = sqrt(mean((est - RD_true)[!n_separation]^2)),
            sd_mean = sd(est, na.rm = TRUE),
            sd_mean_sep = sd(est[!n_separation], na.rm = TRUE),
            mean_se = mean(SE, na.rm = TRUE),
            mean_se_sep = mean(SE[!n_separation], na.rm = TRUE),
            n_sep = sum(n_separation))


# Summary of RR ---------------------------------------------------------

df_RR_poisson <- df_result %>% 
  mutate(est = exp(RR_poisson)) %>% 
  mutate(SE = exp(SE_poisson_robust)) %>% 
  mutate(method = "RR poisson + robust SE") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)

df_RR_poisson_rd <- df_result %>% 
  mutate(est = exp(RR_poisson_br)) %>% 
  mutate(SE = exp(SE_poisson_br)) %>% 
  mutate(method = "RR poisson + bias reduction") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)

df_RR_poisson_rd_robust <- df_result %>% 
  mutate(est = exp(RR_poisson_br)) %>% 
  mutate(SE = exp(SE_poisson_br_robust)) %>% 
  mutate(method = "RR poisson + bias reduction + robust") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)


df_RR_bin_log <- df_result %>% 
  mutate(est = exp(RR_bin_log)) %>% 
  mutate(SE = exp(SE_bin_log)) %>% 
  mutate(method = "RR log-binomial + naive SE") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)

df_RR_bin_log_rd <- df_result %>% 
  mutate(est = exp(RR_bin_log_br)) %>% 
  mutate(SE = exp(SE_bin_log_br)) %>% 
  mutate(method = "RR log-binomial + bias reduction") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)

df_RR_bin_log_rd_robust <- df_result %>% 
  mutate(est = exp(RR_bin_log_br)) %>% 
  mutate(SE = exp(SE_bin_log_br_robust)) %>% 
  mutate(method = "RR log-binomial + bias reduction + robust") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)


# Summary of OR ---------------------------------------------------------

df_OR_logistic <- df_result %>% 
  mutate(est = exp(OR_logistic)) %>% 
  mutate(SE = exp(SE_logistic)) %>% 
  mutate(method = "OR logistic + naive SE") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)


df_OR_firth <- df_result %>% 
  mutate(est = exp(OR_firth)) %>% 
  mutate(SE = exp(SE_firth)) %>% 
  mutate(method = "OR firth + naive SE") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)

df_OR_logF <- df_result %>% 
  mutate(est = exp(OR_logF)) %>% 
  mutate(SE = exp(SE_logF)) %>% 
  mutate(method = "OR logF + naive SE") %>% 
  select(n, prevalence, prob.x, RD_true, RR_true, n_separation, est, SE, method)

df_RR <- bind_rows(df_RR_poisson, df_RR_poisson_rd, df_RR_poisson_rd_robust, df_RR_bin_log, df_RR_bin_log_rd, df_RR_bin_log_rd_robust, df_OR_logistic, df_OR_firth, df_OR_logF)

tab_RR <- df_RR %>% 
  group_by(n, prevalence, RR_true, method) %>% 
  summarise(expectation = mean(est, na.rm = TRUE),
            expectation_sep = mean(est[!n_separation], na.rm = TRUE),
            bias = mean(est - RR_true, na.rm = TRUE),
            bias_sep = mean((est - RR_true)[!n_separation], na.rm = TRUE),
            rmse = sqrt(mean((est - RR_true)^2)),
            rmse_sep = sqrt(mean((est - RR_true)[!n_separation]^2)),
            sd_mean = sd(est, na.rm = TRUE),
            sd_mean_sep = sd(est[!n_separation], na.rm = TRUE),
            mean_se = mean(SE, na.rm = TRUE),
            mean_se_sep = mean(SE[!n_separation], na.rm = TRUE),
            n_sep = sum(n_separation))

tab_RR2 <- tab_RR %>% 
  mutate(expectation2 = if_else(method %in% c("OR firth + naive SE", "OR logF + naive SE", "RR poisson + bias reduction",
                                              "RR poisson + bias reduction + robust", 
                                              "RR log-binomial + bias reduction",
                                              "RR log-binomial + bias reduction + robust"), expectation, expectation_sep)) %>% 
  mutate(bias2 = if_else(method %in% c("OR firth + naive SE", "OR logF + naive SE", "RR poisson + bias reduction",
                                       "RR poisson + bias reduction + robust", 
                                       "RR log-binomial + bias reduction",
                                       "RR log-binomial + bias reduction + robust"), bias, bias_sep)) %>% 
  mutate(rmse2 = if_else(method %in% c("OR firth + naive SE", "OR logF + naive SE", "RR poisson + bias reduction",
                                       "RR poisson + bias reduction + robust", 
                                       "RR log-binomial + bias reduction",
                                       "RR log-binomial + bias reduction + robust"), rmse, rmse_sep)) %>% 
  mutate(sd_mean2 = if_else(method %in% c("OR firth + naive SE", "OR logF + naive SE", "RR poisson + bias reduction",
                                          "RR poisson + bias reduction + robust", 
                                          "RR log-binomial + bias reduction",
                                          "RR log-binomial + bias reduction + robust"), sd_mean, sd_mean_sep)) %>% 
  mutate(mean_se2 = if_else(method %in% c("OR firth + naive SE", "OR logF + naive SE", "RR poisson + bias reduction",
                                          "RR poisson + bias reduction + robust", 
                                          "RR log-binomial + bias reduction",
                                          "RR log-binomial + bias reduction + robust"), mean_se, mean_se_sep))



# Figure ------------------------------------------------------------------
# RD
fig_RD_est_n1000 <- tab_RD %>%
  filter(n == 1000) %>%
  ggplot(aes(x = RD_true, y = expectation, color = method)) +
  geom_point(position = position_dodge(0.0005)) +
  geom_line(position = position_dodge(0.0005)) +
  geom_linerange(aes(ymin = expectation - sd_mean, ymax = expectation + sd_mean), position = position_dodge(0.0005)) +
  scale_x_continuous(breaks = c(0, 0.001, 0.002, 0.005, 0.01, 0.02, 0.03), labels = label_number()) +
  scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.005, 0.01, 0.02, 0.03), labels = label_number()) +
  labs(title = paste0("Prevalence: 0.001")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
fig_RD_est_n1000

fig_RD_est_n10000 <- tab_RD %>% 
  filter(n == 10000) %>%
  ggplot(aes(x = RD_true, y = expectation, color = method)) + 
  geom_point(position = position_dodge(0.00005)) +
  geom_line(position = position_dodge(0.00005)) +
  geom_linerange(aes(ymin = expectation - sd_mean, ymax = expectation + sd_mean), position = position_dodge(0.00005)) +
  scale_x_continuous(breaks = c(0, 0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.003), labels = label_number()) +
  scale_y_continuous(breaks = c(0, 0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.003), labels = label_number()) +
  labs(title = paste0("Prevalence: 0.0001")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~prob.x)
fig_RD_est_n10000

fig_RD_bias_n10000 <- tab_RD %>% 
  filter(n == 10000) %>% 
  ggplot(aes(x = RD_true, y = bias, color = method)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(0.00005)) +
  geom_line(position = position_dodge(0.00005)) +
  scale_x_continuous(breaks = c(0, 0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.003), labels = label_number()) +
  scale_y_continuous(labels = label_number()) +
  labs(title = paste0("Prevalence: 0.0001")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~prob.x)
fig_RD_bias_n10000

fig_RD_rmse_n10000 <- tab_RD %>% 
  filter(n == 10000) %>% 
  ggplot(aes(x = RD_true, y = rmse, color = method)) + 
  # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(0.00005)) +
  geom_line(position = position_dodge(0.00005)) +
  scale_x_continuous(breaks = c(0, 0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.003), labels = label_number()) +
  # scale_y_continuous(labels = label_number()) +
  labs(title = paste0("Prevalence: 0.0001")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~prob.x)
fig_RD_rmse_n10000



# RR
pd <- position_dodge(0.9)
fig_RR_est_n1000 <- tab_RR2 %>% 
  filter(n == 1000) %>% 
  ggplot(aes(x = RR_true, y = expectation2, color = method)) + 
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_linerange(aes(ymin = expectation2 - sd_mean2, ymax = expectation2 + sd_mean2), position = pd) +
  scale_x_continuous(breaks = c(1,2,3,6,11,21,31)) +
  scale_y_continuous(breaks = c(1,2,3,6,11,21,31), limits = c(0,31)) +
  labs(title = paste0("Prevalence: 0.0001"),
       subtitle = ("Results of firth, log-F, bias reduction methods are included separation cases.\nMLE based methods are only consisted of overlap cases.")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
fig_RR_est_n1000

fig_RR_est <- tab_RR2 %>% 
  filter(n == 10000) %>% 
  ggplot(aes(x = RR_true, y = expectation2, color = method)) + 
  geom_point(position = pd) +
  geom_line(position = pd) +
  geom_linerange(aes(ymin = expectation2 - sd_mean2, ymax = expectation2 + sd_mean2), position = pd) +
  scale_x_continuous(breaks = c(1,2,3,6,11,21,31)) +
  scale_y_continuous(breaks = c(1,2,3,6,11,21,31), limits = c(-20,100)) +
  labs(title = paste0("Prevalence: 0.0001"),
       subtitle = ("Results of firth, log-F, bias reduction methods are included separation cases.\nMLE based methods are only consisted of overlap cases.")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~prob.x)
fig_RR_est

fig_RR_bias_n1000 <- tab_RR2 %>% 
  filter(n == 1000) %>% 
  ggplot(aes(x = RR_true, y = bias2, color = method)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = pd) +
  geom_line(position = pd) +
  scale_x_continuous(breaks = c(1,2,3,6,11,21,31)) +
  scale_y_continuous(labels = label_number()) +
  labs(title = paste0("Prevalence: 0.001"),
       subtitle = "Results of firth, log-F, bias reduction methods are included separation cases.\nMLE based methods are only consisted of overlap cases.") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
fig_RR_bias_n1000

fig_RR_bias <- tab_RR2 %>% 
  filter(n == 10000) %>% 
  ggplot(aes(x = RR_true, y = bias2, color = method)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = pd) +
  geom_line(position = pd) +
  scale_x_continuous(breaks = c(1,2,3,6,11,21,31)) +
  scale_y_continuous(labels = label_number()) +
  labs(title = paste0("Prevalence: 0.0001"),
       subtitle = "Results of firth, log-F, bias reduction methods are included separation cases.\nMLE based methods are only consisted of overlap cases.") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~prob.x)
fig_RR_bias

fig_RR_rmse <- tab_RR2 %>% 
  filter(n == 10000) %>% 
  ggplot(aes(x = RR_true, y = rmse2, color = method)) + 
  # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = pd) +
  geom_line(position = pd) +
  scale_x_continuous(breaks = c(1,2,3,6,11,21,31)) +
  # scale_y_continuous(labels = label_number()) +
  labs(title = paste0("Prevalence: 0.0001"),
       subtitle = "Results of firth, log-F, bias reduction methods are included separation cases.\nMLE based methods are only consisted of overlap cases.") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~prob.x)
fig_RR_rmse
