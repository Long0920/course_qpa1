rm(list = ls())

library(tidyverse)






###problem 1
#1-1
p_pos_truth <- 0.01
p_pos_lie <- 0.99
p_neg_truth <- 1 - p_pos_truth
p_neg_lie <- 1 - p_pos_lie

p_both_positive <- p_pos_truth * p_pos_lie
p_both_positive

#1-2
p_guilty_pos_innocent_neg <- p_pos_lie * p_neg_truth
p_guilty_pos_innocent_neg

#1-3
p_completely_wrong <- p_pos_truth * p_neg_lie
p_completely_wrong

#1-4
p_either_or_both_positive <- 1 - (p_neg_truth * p_neg_lie)
p_either_or_both_positive

###problem 2
#2-1
p_hh <- 1 / 3
p_tt <- 1 / 3
p_ht <- 1 / 3

p_head <- p_hh * 1 + p_ht * 0.5 + p_tt * 0
p_other_side_head_given_head <- (p_hh * 1) / p_head
p_other_side_head_given_head

###problem 3
#3-1
p_mother_disease <- 1 / 3
p_mother_no_disease <- 2 / 3

p_neither_child_disease <- p_mother_disease * (0.5 * 0.5) + p_mother_no_disease * 1
p_neither_child_disease

#3-2
p_elder_disease <- p_mother_disease * 0.5
p_younger_disease <- p_mother_disease * 0.5
p_both_disease <- p_mother_disease * (0.5 * 0.5)
p_independent_check <- p_elder_disease * p_younger_disease

independence_result <- data.frame(
  P_both = p_both_disease,
  P_elder_times_P_younger = p_independent_check,
  independent = p_both_disease == p_independent_check
)
independence_result

#3-3
p_elder_no_disease <- p_mother_disease * 0.5 + p_mother_no_disease * 1
p_mother_disease_given_elder_no <- (0.5 * p_mother_disease) / p_elder_no_disease
p_mother_disease_given_elder_no

#3-4
p_both_no_disease <- p_mother_disease * (0.5 * 0.5) + p_mother_no_disease * 1
p_mother_disease_given_both_no <- ((0.5 * 0.5) * p_mother_disease) / p_both_no_disease
p_mother_disease_given_both_no

#3-5
p_mother_no_disease_given_elder_no <- 1 - p_mother_disease_given_elder_no
p_younger_no_given_elder_no <- p_mother_disease_given_elder_no * 0.5 + p_mother_no_disease_given_elder_no * 1
p_mother_disease_sequential <- (0.5 * p_mother_disease_given_elder_no) / p_younger_no_given_elder_no

sequential_vs_onestep <- data.frame(
  one_step = p_mother_disease_given_both_no,
  sequential = p_mother_disease_sequential,
  same_result = all.equal(p_mother_disease_given_both_no, p_mother_disease_sequential)
)
sequential_vs_onestep

###problem 4
df <- read.csv("C:/Users/ylc75/OneDrive/Desktop/waseda/QPA1/vote.csv")
#View(df)

#4-1
summary(df)

#4-2
n_df <- df %>% 
  mutate(
    vote = ifelse(vote < 0|vote > 1, NA, vote),
    income = ifelse(income < 0|income > 17, NA, income),
    education = ifelse(education < 0, NA, education),
    age = ifelse(age < 18|age > 85, NA, age),
    female = ifelse(female < 0|female > 1, NA, female)
    )
summary(n_df)

#4-3
voteRate <- mean(n_df$vote, na.rm = TRUE)
voteRate

#4-4
class(n_df$state)
table(n_df$state)
n_df <- n_df %>%
  mutate(
    AR = ifelse(state == "AR",1,0)
  )
table(n_df$state,n_df$AR)

#4-5
n_df %>%
  group_by(AR) %>%
  summarise(
    mean_voteRate = mean(vote, na.rm = TRUE),
    sd_voteRate = sd(vote, na.rm = TRUE),
    n = n()
  )

#4-6
n_df <- n_df[order(-n_df$AR, n_df$age), ]

min.ark <- min(n_df$age[n_df$AR == 1], na.rm = TRUE)
min.sc <- min(n_df$age[n_df$AR == 0], na.rm = TRUE)
min.ark
min.sc

#4-7
n_df %>%
  group_by(AR) %>%
  filter(AR == 1 & age == min.ark) %>%
  summarise(
    m_income = median(income, na.rm = TRUE),
    n = n()
  )
n_df %>%
  group_by(AR) %>%
  filter(AR == 0 & age == min.sc) %>%
  summarise(
    womenPropotion = mean(female, na.rm = TRUE),
    n = n()
  )

#4-8
n_df$low_inc <- ifelse(
  n_df$income < median(n_df$income, na.rm = TRUE),
  1, 0
)
table(n_df$income,n_df$low_inc)

#4-9
par(mfrow = c(1,3))

hist(n_df$education,
     main = "All Individuals",
     xlab = "Education Level")

hist(n_df$education[n_df$low_inc == 1],
     main = "Low Income Individuals",
     xlab = "Education Level")

hist(n_df$education[n_df$low_inc == 0],
     main = "High Income Individuals",
     xlab = "Education Level")

#4-10
set.seed(456)
sample_df <- n_df[sample(nrow(n_df), 100, replace = FALSE), ]

voteRate_s <- mean(sample_df$vote, na.rm = TRUE)
voteRate_s_sd <- sd(sample_df$vote, na.rm = TRUE)

#4-11
set.seed(456)
sample_df2 <- n_df[sample(nrow(n_df), 1000, replace = FALSE), ]

voteRate_s2 <- mean(sample_df2$vote, na.rm = TRUE)
voteRate_s2_sd <- sd(sample_df2$vote, na.rm = TRUE)

voteRate_sd <- sd(n_df$vote, na.rm = TRUE)

comparison <- data.frame(
  Statistic = c("Vote Rate", "Standard Deviation"),
  Sample_1 = c(voteRate_s, voteRate_s_sd),
  Sample_2 = c(voteRate_s2, voteRate_s2_sd),
  Full_Sample = c(voteRate, voteRate_sd)
)

print(comparison, row.names = FALSE)

#4-12
set.seed(123)

rates10 <- numeric(10)

for (i in 1:10) {
  s <- sample(nrow(n_df), 100, replace = FALSE)
  rates10[i] <- mean(n_df$vote[s] == 1, na.rm = TRUE)
}

mean10 <- mean(rates10)
mean10

set.seed(123)

rates100 <- numeric(100)

for (i in 1:100) {
  s <- sample(nrow(n_df), 100, replace = FALSE)
  rates100[i] <- mean(n_df$vote[s] == 1, na.rm = TRUE)
}

mean100 <- mean(rates100)
mean100

par(mfrow = c(1, 2))

hist(rates10,
     main = "10 Samples",
     xlab = "Turnout Rate")
abline(v = mean10, col = "blue", lwd = 2)
abline(v = voteRate, col = "red", lwd = 2)
legend("topright",
       legend = c("Sample Mean", "Original Rate"),
       col = c("blue", "red"), lty = 1, cex = 0.8)

hist(rates100,
     main = "100 Samples",
     xlab = "Turnout Rate")
abline(v = mean100, col = "blue", lwd = 2)
abline(v = voteRate, col = "red", lwd = 2)
legend("topright",
       legend = c("Sample Mean", "Original Rate"),
       col = c("blue", "red"), lty = 1, cex = 0.8)

par(mfrow = c(1, 1))

comparison <- data.frame(
  Group = c("Q3 Original", "Q10 Sample", "10 Samples", "100 Samples"),
  Turnout_Rate = c(voteRate, voteRate_s, mean10, mean100)
)

print(comparison)
