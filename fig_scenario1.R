library(tidyverse)
library(scales)
library(openxlsx)
library(ggview)
library(patchwork)


# Performance measure -----------------------------------------------------

# Scenario 1 --------------------------------------------------------------

df_result_scenario1 <- read_rds("03_Output/result_module1_240208.rds")
df_performance_scenario1 <- df_result_scenario1 %>% 
  mutate(num_increase = n * RD_true) %>% 
  mutate(cover_ols = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols & RD_true <= RD_ols + qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(cover_HC0 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC0 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(cover_HC1 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC1 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(cover_HC2 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC2 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(cover_HC3 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC3 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC3, 1, 0)) %>%
  mutate(cover_HC4 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC4 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(pow_ols = if_else(0 < RD_ols - qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(pow_HC0 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(pow_HC1 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(pow_HC2 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(pow_HC3 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC3, 1, 0)) %>% 
  mutate(pow_HC4 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(flg_separation = if_else(n_separation == 1, 1, 0)) %>% 
  group_by(n, prevalence, num_increase, RD_true) %>% 
  summarise(n_sim = n(),
            n_sep = mean(flg_separation),
            expectation = mean(RD_ols),
            bias = mean(RD_ols - RD_true),
            emp_se = sqrt(1/(n_sim-1))*sum((RD_ols - mean(RD_ols))^2),
            mse = (1/n_sim)*sum((RD_ols - RD_true)^2),
            
            coverage_ols = mean(cover_ols),
            coverage_HC0 = mean(cover_HC0),
            coverage_HC1 = mean(cover_HC1),
            coverage_HC2 = mean(cover_HC2),
            coverage_HC3 = mean(cover_HC3),
            coverage_HC4 = mean(cover_HC4),
            power_ols = mean(pow_ols),
            power_HC0 = mean(pow_HC0),
            power_HC1 = mean(pow_HC1),
            power_HC2 = mean(pow_HC2),
            power_HC3 = mean(pow_HC3),
            power_HC4 = mean(pow_HC4),
            
            monte_se_bias = sqrt((1/(n_sim*(n_sim-1))) * sum((RD_ols - mean(RD_ols))^2)),
            monte_se_emp_se = emp_se / sqrt(2*(n_sim-1)),
            monte_se_mse = sqrt(sum(((RD_ols - RD_true)^2 - mse)^2) / (n_sim*(n_sim-1))),
            monte_se_coverage_ols = sqrt(coverage_ols*(1-coverage_ols)/n_sim),
            monte_se_coverage_HC0 = sqrt(coverage_HC0*(1-coverage_HC0)/n_sim),
            monte_se_power_ols = sqrt(power_ols*(1-power_ols)/n_sim),
            monte_se_power_HC0 = sqrt(power_HC0*(1-power_HC0)/n_sim)
            )
write.xlsx(df_performance_scenario1, "03_Output/df_performance_scenario1.xlsx")

# Scenario 2 --------------------------------------------------------------

df_result_scenario1 <- read_rds("03_Output/result_module2_240208.rds")

iteration <- seq(1,2000, 1)
n <- c(1000, 10000, 100000)
num_increase <- c(0, 5, 10, 20, 50)
prob.x <- c(0.7, 0.8, 0.9, 0.95)

df_sim <- expand_grid(n, num_increase, prob.x, iteration) %>% 
  mutate(prevalence = 1/n) %>%
  mutate(RD_true = prevalence + (num_increase/n)  - prevalence) %>% 
  mutate(no = row_number()) %>% 
  select(prob.x)

df_result_scenario2 <- bind_cols(df_result_scenario1, df_sim)


df_performance_scenario2 <- df_result_scenario2 %>% 
  mutate(num_increase = n * RD_true) %>% 
  mutate(cover_ols = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols & RD_true <= RD_ols + qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(cover_HC0 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC0 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(cover_HC1 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC1 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(cover_HC2 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC2 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(cover_HC3 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC3 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC3, 1, 0)) %>%
  mutate(cover_HC4 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC4 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(pow_ols = if_else(0 < RD_ols - qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(pow_HC0 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(pow_HC1 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(pow_HC2 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(pow_HC3 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC3, 1, 0)) %>% 
  mutate(pow_HC4 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(flg_separation = if_else(n_separation == 1, 1, 0)) %>% 
  group_by(n, prevalence, num_increase, RD_true, prob.x) %>% 
  summarise(n_sim = n(),
            n_sep = mean(flg_separation),
            expectation = mean(RD_ols),
            bias = mean(RD_ols - RD_true),
            emp_se = sqrt(1/(n_sim-1))*sum((RD_ols - mean(RD_ols))^2),
            mse = (1/n_sim)*sum((RD_ols - RD_true)^2),
            
            coverage_ols = mean(cover_ols),
            coverage_HC0 = mean(cover_HC0),
            coverage_HC1 = mean(cover_HC1),
            coverage_HC2 = mean(cover_HC2),
            coverage_HC3 = mean(cover_HC3),
            coverage_HC4 = mean(cover_HC4),
            power_ols = mean(pow_ols),
            power_HC0 = mean(pow_HC0),
            power_HC1 = mean(pow_HC1),
            power_HC2 = mean(pow_HC2),
            power_HC3 = mean(pow_HC3),
            power_HC4 = mean(pow_HC4),
            
            monte_se_bias = sqrt((1/(n_sim*(n_sim-1))) * sum((RD_ols - mean(RD_ols))^2)),
            monte_se_emp_se = emp_se / sqrt(2*(n_sim-1)),
            monte_se_mse = sqrt(sum(((RD_ols - RD_true)^2 - mse)^2) / (n_sim*(n_sim-1))),
            monte_se_coverage_ols = sqrt(coverage_ols*(1-coverage_ols)/n_sim),
            monte_se_coverage_HC0 = sqrt(coverage_HC0*(1-coverage_HC0)/n_sim),
            monte_se_power_ols = sqrt(power_ols*(1-power_ols)/n_sim),
            monte_se_power_HC0 = sqrt(power_HC0*(1-power_HC0)/n_sim)
  )
write.xlsx(df_performance_scenario2, "03_Output/df_performance_scenario2.xlsx")


# Scenario 3 --------------------------------------------------------------

df_result_scenario1 <- read_rds("03_Output/result_module3_2402013.rds")
df_performance_scenario3 <- df_result_scenario1 %>% 
  mutate(num_increase = n * RD_true) %>% 
  mutate(cover_ols = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols & RD_true <= RD_ols + qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(cover_HC0 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC0 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(cover_HC1 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC1 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(cover_HC2 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC2 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(cover_HC3 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC3 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC3, 1, 0)) %>%
  mutate(cover_HC4 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC4 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(pow_ols = if_else(0 < RD_ols - qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(pow_HC0 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(pow_HC1 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(pow_HC2 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(pow_HC3 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC3, 1, 0)) %>% 
  mutate(pow_HC4 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(flg_separation = if_else(n_separation == 1, 1, 0)) %>% 
  group_by(n, prevalence, num_increase, RD_true) %>% 
  summarise(n_sim = n(),
            n_sep = mean(flg_separation),
            p_x = mean(prop_x),
            expectation = mean(RD_ols),
            bias = mean(RD_ols - RD_true),
            emp_se = sqrt(1/(n_sim-1))*sum((RD_ols - mean(RD_ols))^2),
            mse = (1/n_sim)*sum((RD_ols - RD_true)^2),
            
            coverage_ols = mean(cover_ols),
            coverage_HC0 = mean(cover_HC0),
            coverage_HC1 = mean(cover_HC1),
            coverage_HC2 = mean(cover_HC2),
            coverage_HC3 = mean(cover_HC3),
            coverage_HC4 = mean(cover_HC4),
            power_ols = mean(pow_ols),
            power_HC0 = mean(pow_HC0),
            power_HC1 = mean(pow_HC1),
            power_HC2 = mean(pow_HC2),
            power_HC3 = mean(pow_HC3),
            power_HC4 = mean(pow_HC4),
            
            monte_se_bias = sqrt((1/(n_sim*(n_sim-1))) * sum((RD_ols - mean(RD_ols))^2)),
            monte_se_emp_se = emp_se / sqrt(2*(n_sim-1)),
            monte_se_mse = sqrt(sum(((RD_ols - RD_true)^2 - mse)^2) / (n_sim*(n_sim-1))),
            monte_se_coverage_ols = sqrt(coverage_ols*(1-coverage_ols)/n_sim),
            monte_se_coverage_HC0 = sqrt(coverage_HC0*(1-coverage_HC0)/n_sim),
            monte_se_power_ols = sqrt(power_ols*(1-power_ols)/n_sim),
            monte_se_power_HC0 = sqrt(power_HC0*(1-power_HC0)/n_sim)
  )
write.xlsx(df_performance_scenario3, "03_Output/df_performance_scenario3.xlsx")

# Scenario 4 --------------------------------------------------------------

df_result_scenario1 <- read_rds("03_Output/result_module4_242016.rds")
df_performance_scenario4 <- df_result_scenario1 %>% 
  mutate(num_increase = n * RD_true) %>% 
  mutate(cover_ols = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols & RD_true <= RD_ols + qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(cover_HC0 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC0 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(cover_HC1 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC1 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(cover_HC2 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC2 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(cover_HC3 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC3 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC3, 1, 0)) %>%
  mutate(cover_HC4 = if_else(RD_true >= RD_ols - qnorm(0.975)*SE_ols_HC4 & RD_true <= RD_ols + qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(pow_ols = if_else(0 < RD_ols - qnorm(0.975)*SE_ols, 1, 0)) %>% 
  mutate(pow_HC0 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC0, 1, 0)) %>% 
  mutate(pow_HC1 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC1, 1, 0)) %>% 
  mutate(pow_HC2 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC2, 1, 0)) %>% 
  mutate(pow_HC3 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC3, 1, 0)) %>% 
  mutate(pow_HC4 = if_else(0 < RD_ols - qnorm(0.975)*SE_ols_HC4, 1, 0)) %>% 
  mutate(flg_separation = if_else(n_separation == 1, 1, 0)) %>% 
  group_by(n, prevalence, num_increase, RD_true) %>% 
  summarise(n_sim = n(),
            n_sep = mean(flg_separation),
            p_x = mean(prop_x),
            expectation = mean(RD_ols),
            bias = mean(RD_ols - RD_true),
            emp_se = sqrt(1/(n_sim-1))*sum((RD_ols - mean(RD_ols))^2),
            mse = (1/n_sim)*sum((RD_ols - RD_true)^2),
            
            coverage_ols = mean(cover_ols),
            coverage_HC0 = mean(cover_HC0),
            coverage_HC1 = mean(cover_HC1),
            coverage_HC2 = mean(cover_HC2),
            coverage_HC3 = mean(cover_HC3),
            coverage_HC4 = mean(cover_HC4),
            power_ols = mean(pow_ols),
            power_HC0 = mean(pow_HC0),
            power_HC1 = mean(pow_HC1),
            power_HC2 = mean(pow_HC2),
            power_HC3 = mean(pow_HC3),
            power_HC4 = mean(pow_HC4),
            
            monte_se_bias = sqrt((1/(n_sim*(n_sim-1))) * sum((RD_ols - mean(RD_ols))^2)),
            monte_se_emp_se = emp_se / sqrt(2*(n_sim-1)),
            monte_se_mse = sqrt(sum(((RD_ols - RD_true)^2 - mse)^2) / (n_sim*(n_sim-1))),
            monte_se_coverage_ols = sqrt(coverage_ols*(1-coverage_ols)/n_sim),
            monte_se_coverage_HC0 = sqrt(coverage_HC0*(1-coverage_HC0)/n_sim),
            monte_se_power_ols = sqrt(power_ols*(1-power_ols)/n_sim),
            monte_se_power_HC0 = sqrt(power_HC0*(1-power_HC0)/n_sim)
  )
write.xlsx(df_performance_scenario4, "03_Output/df_performance_scenario4.xlsx")



# Performance all ---------------------------------------------------------

df_performance_scenario1_1 <- df_performance_scenario4 %>% mutate(scenario = 1, .before = n)
df_performance_scenario2_1 <- df_performance_scenario2 %>% mutate(scenario = 2, .before = n)
df_performance_scenario3_1 <- df_performance_scenario3 %>% mutate(scenario = 3, .before = n)
df_performance_scenario4_1 <- df_performance_scenario4 %>% mutate(scenario = 4, .before = n)

df <- bind_rows(df_performance_scenario1_1, df_performance_scenario2_1, df_performance_scenario3_1, df_performance_scenario4_1) %>% 
  mutate(scenario = factor(scenario))


# Estimate ----------------------------------------------------------------
# Scenario 1 estimate -----------------------------------------------------

fig_s1_n1000 <- df %>% 
  filter(scenario == 1) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 1",
    subtitle = "n = 1000, Prevalence: 0.001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s1_n10000 <- df %>% 
  filter(scenario == 1) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.0005, 0.001, 0.002, 0.005), labels = label_number(),
                     limits = c(-0.0005,0.006)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s1_n100000 <- df %>% 
  filter(scenario == 1) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.00005, 0.0001, 0.0002, 0.0005), labels = label_number(),
                     limits = c(-0.00005,0.0006)) +
  labs(title = "",
       subtitle = "n = 100000, Prevalence: 0.00001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())
  
fig_s1 <- fig_s1_n1000 + fig_s1_n10000 + fig_s1_n100000

ggsave("03_Output/fig/fig_s1.pdf", dpi = 300, width = 12, height = 4)


# Scenario 2 estimate -----------------------------------------------------

pd <- position_dodge(0.5)
fig_s2_n1000 <- df %>% 
  filter(scenario == 2) %>%
  filter(n == 1000) %>% 
  mutate(prob.x = factor(prob.x)) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation, color = prob.x, shape = prob.x)) +
  geom_point(position = pd) +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1,
                position = pd) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
                     limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2",
    subtitle = "n = 1000, Prevalence: 0.001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD",
       color = "Proportion of exposure",
       shape = "Proportion of exposure") + 
  theme(panel.grid.minor.y = element_blank(),
        legend.position = c(0.01,0.99), legend.justification = c(0,1))

fig_s2_n10000 <- df %>% 
  filter(scenario == 2) %>%
  filter(n == 10000) %>% 
  mutate(prob.x = factor(prob.x)) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation, color = prob.x, shape = prob.x)) +
  geom_point(position = pd) +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1,
                position = pd) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.0005, 0.001, 0.002, 0.005), labels = label_number(),
                     limits = c(-0.0005,0.006)) +
  labs(title = "",
       subtitle = "n = 10000, Prevalence: 0.0001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD",
       color = "Proportion of exposure",
       shape = "Proportion of exposure") + 
  theme(panel.grid.minor.y = element_blank(),
        legend.position = c(0.01,0.99), legend.justification = c(0,1))

fig_s2_n100000 <- df %>% 
  filter(scenario == 2) %>%
  filter(n == 100000) %>% 
  mutate(prob.x = factor(prob.x)) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation, color = prob.x, shape = prob.x)) +
  geom_point(position = pd) +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1,
                position = pd) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.00005, 0.0001, 0.0002, 0.0005), labels = label_number(),
                     limits = c(-0.00005,0.0006)) +
  labs(title = "",
       subtitle = "n = 100000, Prevalence: 0.00001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD",
       color = "Proportion of exposure",
       shape = "Proportion of exposure") + 
  theme(panel.grid.minor.y = element_blank(),
        legend.position = c(0.01,0.99), legend.justification = c(0,1))

fig_s2 <- fig_s2_n1000 + fig_s2_n10000 + fig_s2_n100000

ggsave("03_Output/fig/fig_s2.pdf", dpi = 300, width = 12, height = 4)


# Scenario 3 estimate -----------------------------------------------------

fig_s3_n1000 <- df %>% 
  filter(scenario == 3) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
                     limits = c(-0.005,0.06)) +
  labs(title = "Scenario: 3",
       subtitle = "n = 1000, Prevalence: 0.001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s3_n10000 <- df %>% 
  filter(scenario == 3) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.0005, 0.001, 0.002, 0.005), labels = label_number(),
                     limits = c(-0.0005,0.006)) +
  labs(title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s3_n100000 <- df %>% 
  filter(scenario == 3) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.00005, 0.0001, 0.0002, 0.0005), labels = label_number(),
                     limits = c(-0.00005,0.0006)) +
  labs(title ="",
       subtitle = "n = 100000, Prevalence: 0.00001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s3 <- fig_s3_n1000 + fig_s3_n10000 + fig_s3_n100000

ggsave("03_Output/fig/fig_s3.pdf", dpi = 300, width = 12, height = 4)


# Scenario 4 estimate -----------------------------------------------------

fig_s4_n1000 <- df %>% 
  filter(scenario == 4) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
                     limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 4",
    subtitle = "n = 1000, Prevalence: 0.001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s4_n10000 <- df %>% 
  filter(scenario == 4) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.0005, 0.001, 0.002, 0.005), labels = label_number(),
                     limits = c(-0.0005,0.006)) +
  labs(title = "",
       subtitle = "n = 10000, Prevalence: 0.0001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s4_n100000 <- df %>% 
  filter(scenario == 4) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = expectation)) +
  geom_point() +
  geom_errorbar(aes(x = num_increase, ymin = expectation - emp_se, ymax = expectation + emp_se),
                width = 0.1) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.00005, 0.0001, 0.0002, 0.0005), labels = label_number(),
                     limits = c(-0.00005,0.0006)) +
  labs(title = "",
       subtitle = "n = 100000, Prevalence: 0.00001",
       x = "True RD\n(Increased number of events due to exposure)",
       y = "Expectation of RD") + 
  theme(panel.grid.minor.y = element_blank())

fig_s4 <- fig_s4_n1000 + fig_s4_n10000 + fig_s4_n100000

ggsave("03_Output/fig/fig_s4.pdf", dpi = 300, width = 12, height = 4)

fig_estimate <- fig_s1 / fig_s2 / fig_s3 / fig_s4
ggsave("03_Output/fig/fig_estimate.pdf", dpi = 300, width = 12, height = 16)



# Coverage ----------------------------------------------------------------

df_coverage <- df %>% 
  select(n, num_increase, scenario, prob.x, 
         coverage_ols, coverage_HC0, coverage_HC1, coverage_HC2, coverage_HC3, coverage_HC4) %>% 
  pivot_longer(
    cols = c(coverage_ols, coverage_HC0, coverage_HC1, coverage_HC2, coverage_HC3, coverage_HC4),
    names_to = "SE",
    values_to = "coverage"
  ) %>% 
  ungroup() %>% 
  mutate(SE = factor(SE, levels = c("coverage_ols", "coverage_HC0", "coverage_HC1", "coverage_HC2", "coverage_HC3", "coverage_HC4"), labels = c("OLS", "HC0", "HC1", "HC2", "HC3", "HC4")))


# Scenario 1 --------------------------------------------------------------
fig_cov_s1_n1000 <- df_coverage %>% 
  filter(scenario == 1) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 1",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s1_n10000 <- df_coverage %>% 
  filter(scenario == 1) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s1_n100000 <- df_coverage %>% 
  filter(scenario == 1) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s1 <- fig_cov_s1_n1000 + fig_cov_s1_n10000 + fig_cov_s1_n100000

ggsave("03_Output/fig/fig_cov_s1.pdf", dpi = 300, width = 12, height = 4)



# Scenario 2 --------------------------------------------------------------

# proportion exposure 0.7 -------------------------------------------------

fig_cov_s2_px0.7_n1000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.7) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.7",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.7_n10000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.7) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.7_n100000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.7) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s2_px0.7 <- fig_cov_s2_px0.7_n1000 + fig_cov_s2_px0.7_n10000 + fig_cov_s2_px0.7_n100000

ggsave("03_Output/fig/fig_cov_s2_px0.7.pdf", dpi = 300, width = 12, height = 4)


# proportion exposure 0.8 -------------------------------------------------

fig_cov_s2_px0.8_n1000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.8) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.8",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.8_n10000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.8) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.8_n100000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.8) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s2_px0.8 <- fig_cov_s2_px0.8_n1000 + fig_cov_s2_px0.8_n10000 + fig_cov_s2_px0.8_n100000

ggsave("03_Output/fig/fig_cov_s2_px0.8.pdf", dpi = 300, width = 12, height = 4)


# proportion exposure 0.9 -------------------------------------------------

fig_cov_s2_px0.9_n1000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.9) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.9",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.9_n10000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.9) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.9_n100000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.9) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s2_px0.9 <- fig_cov_s2_px0.9_n1000 + fig_cov_s2_px0.9_n10000 + fig_cov_s2_px0.9_n100000

ggsave("03_Output/fig/fig_cov_s2_px0.9.pdf", dpi = 300, width = 12, height = 4)

# proportion exposure 0.95 -------------------------------------------------

fig_cov_s2_px0.95_n1000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.95) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.95",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.95_n10000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.95) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s2_px0.95_n100000 <- df_coverage %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.95) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s2_px0.95 <- fig_cov_s2_px0.95_n1000 + fig_cov_s2_px0.95_n10000 + fig_cov_s2_px0.95_n100000

ggsave("03_Output/fig/fig_cov_s2_px0.95.pdf", dpi = 300, width = 12, height = 4)

fig_cov_s2 <- fig_cov_s2_px0.7 / fig_cov_s2_px0.8 / fig_cov_s2_px0.9 / fig_cov_s2_px0.95 + plot_layout(guides='collect') &
  theme(legend.position='bottom') &
  scale_y_continuous(breaks = seq(0.85, 1, 0.05),limits = c(0.85, 1))
  
ggsave("03_Output/fig/fig_cov_s2.pdf", dpi = 300, width = 12, height = 16)


# Scenario 3 --------------------------------------------------------------
fig_cov_s3_n1000 <- df_coverage %>% 
  filter(scenario == 3) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 3",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s3_n10000 <- df_coverage %>% 
  filter(scenario == 3) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s3_n100000 <- df_coverage %>% 
  filter(scenario == 3) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s3 <- fig_cov_s3_n1000 + fig_cov_s3_n10000 + fig_cov_s3_n100000

ggsave("03_Output/fig/fig_cov_s3.pdf", dpi = 300, width = 12, height = 4)


# Scenario 4 --------------------------------------------------------------
fig_cov_s4_n1000 <- df_coverage %>% 
  filter(scenario == 4) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 4",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s4_n10000 <- df_coverage %>% 
  filter(scenario == 4) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())

fig_cov_s4_n100000 <- df_coverage %>% 
  filter(scenario == 4) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = coverage, color = SE, shape = SE, group = SE)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "#222222") +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Covarage") + 
  theme(panel.grid.minor.y = element_blank())


fig_cov_s4 <- fig_cov_s4_n1000 + fig_cov_s4_n10000 + fig_cov_s4_n100000

ggsave("03_Output/fig/fig_cov_s4.pdf", dpi = 300, width = 12, height = 4)

fig_cov <- fig_cov_s1 / fig_cov_s2_px0.9 / fig_cov_s3 / fig_cov_s4 + plot_layout(guides='collect') &
  theme(legend.position='bottom') &
  scale_y_continuous(breaks = seq(0.9, 1, 0.01),limits = c(0.9, 1))

ggsave("03_Output/fig/fig_cov.pdf", dpi = 300, width = 12, height = 16)




# Power ----------------------------------------------------------------

df_power <- df %>% 
  select(n, num_increase, scenario, prob.x, 
         power_ols, power_HC0, power_HC1, power_HC2, power_HC3, power_HC4) %>% 
  pivot_longer(
    cols = c(power_ols, power_HC0, power_HC1, power_HC2, power_HC3, power_HC4),
    names_to = "SE",
    values_to = "power"
  ) %>% 
  ungroup() %>% 
  mutate(SE = factor(SE, levels = c("power_ols", "power_HC0", "power_HC1", "power_HC2", "power_HC3", "power_HC4"), labels = c("OLS", "HC0", "HC1", "HC2", "HC3", "HC4")))


# Scenario 1 --------------------------------------------------------------
fig_pow_s1_n1000 <- df_power %>% 
  filter(scenario == 1) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 1",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s1_n10000 <- df_power %>% 
  filter(scenario == 1) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s1_n100000 <- df_power %>% 
  filter(scenario == 1) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s1 <- fig_pow_s1_n1000 + fig_pow_s1_n10000 + fig_pow_s1_n100000

ggsave("03_Output/fig/fig_pow_s1.pdf", dpi = 300, width = 12, height = 4)



# Scenario 2 --------------------------------------------------------------

# proportion exposure 0.7 -------------------------------------------------

fig_pow_s2_px0.7_n1000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.7) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.7",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.7_n10000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.7) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.7_n100000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.7) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s2_px0.7 <- fig_pow_s2_px0.7_n1000 + fig_pow_s2_px0.7_n10000 + fig_pow_s2_px0.7_n100000

ggsave("03_Output/fig/fig_pow_s2_px0.7.pdf", dpi = 300, width = 12, height = 4)


# proportion exposure 0.8 -------------------------------------------------

fig_pow_s2_px0.8_n1000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.8) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.8",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.8_n10000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.8) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.8_n100000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.8) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s2_px0.8 <- fig_pow_s2_px0.8_n1000 + fig_pow_s2_px0.8_n10000 + fig_pow_s2_px0.8_n100000

ggsave("03_Output/fig/fig_pow_s2_px0.8.pdf", dpi = 300, width = 12, height = 4)


# proportion exposure 0.9 -------------------------------------------------

fig_pow_s2_px0.9_n1000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.9) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.9",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.9_n10000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.9) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.9_n100000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.9) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s2_px0.9 <- fig_pow_s2_px0.9_n1000 + fig_pow_s2_px0.9_n10000 + fig_pow_s2_px0.9_n100000

ggsave("03_Output/fig/fig_pow_s2_px0.9.pdf", dpi = 300, width = 12, height = 4)

# proportion exposure 0.95 -------------------------------------------------

fig_pow_s2_px0.95_n1000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.95) %>% 
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 2, Proportion of exposure: 0.95",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.95_n10000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.95) %>% 
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s2_px0.95_n100000 <- df_power %>% 
  filter(scenario == 2) %>%
  filter(prob.x == 0.95) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s2_px0.95 <- fig_pow_s2_px0.95_n1000 + fig_pow_s2_px0.95_n10000 + fig_pow_s2_px0.95_n100000

ggsave("03_Output/fig/fig_pow_s2_px0.95.pdf", dpi = 300, width = 12, height = 4)

fig_pow_s2 <- fig_pow_s2_px0.7 / fig_pow_s2_px0.8 / fig_pow_s2_px0.9 / fig_pow_s2_px0.95 + plot_layout(guides='collect') &
  theme(legend.position='bottom') &
  scale_y_continuous(breaks = seq(0, 1, 0.1),limits = c(0, 1))


ggsave("03_Output/fig/fig_pow_s2.pdf", dpi = 300, width = 12, height = 16)


# Scenario 3 --------------------------------------------------------------
fig_pow_s3_n1000 <- df_power %>% 
  filter(scenario == 3) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 3",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s3_n10000 <- df_power %>% 
  filter(scenario == 3) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s3_n100000 <- df_power %>% 
  filter(scenario == 3) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s3 <- fig_pow_s3_n1000 + fig_pow_s3_n10000 + fig_pow_s3_n100000

ggsave("03_Output/fig/fig_pow_s3.pdf", dpi = 300, width = 12, height = 4)


# Scenario 4 --------------------------------------------------------------
fig_pow_s4_n1000 <- df_power %>% 
  filter(scenario == 4) %>%
  filter(n == 1000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.005\n(5)", "0.01\n(10)", "0.02\n(20)", "0.05\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "Scenario: 4",
    subtitle = "n = 1000, Prevalence: 0.001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s4_n10000 <- df_power %>% 
  filter(scenario == 4) %>%
  filter(n == 10000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.0005\n(5)", "0.001\n(10)", "0.002\n(20)", "0.005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 10000, Prevalence: 0.0001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())

fig_pow_s4_n100000 <- df_power %>% 
  filter(scenario == 4) %>%
  filter(n == 100000) %>%
  mutate(num_increase = factor(num_increase,
                               levels = c(0, 5, 10, 20, 50),
                               labels = c("0\n(0)", "0.00005\n(5)", "0.0001\n(10)", "0.0002\n(20)", "0.0005\n(50)"))) %>% 
  ggplot(aes(x = num_increase, y = power, color = SE, shape = SE, group = SE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.005, 0.01, 0.02, 0.05),
  #                    limits = c(-0.005,0.06)) +
  labs(
    title = "",
    subtitle = "n = 100000, Prevalence: 0.00001",
    x = "True RD\n(Increased number of events due to exposure)",
    y = "Power") + 
  theme(panel.grid.minor.y = element_blank())


fig_pow_s4 <- fig_pow_s4_n1000 + fig_pow_s4_n10000 + fig_pow_s4_n100000

ggsave("03_Output/fig/fig_pow_s4.pdf", dpi = 300, width = 12, height = 4)

fig_pow <- fig_pow_s1 / fig_pow_s2_px0.9 / fig_pow_s3 / fig_pow_s4 + plot_layout(guides='collect') &
  theme(legend.position='bottom') &
  scale_y_continuous(breaks = seq(0, 1, 0.1),limits = c(0, 1))

ggsave("03_Output/fig/fig_pow.pdf", dpi = 300, width = 12, height = 16)
