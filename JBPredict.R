df <- read.csv("JBTrain.csv")
nrow(df) # 9014
colnames(df) # testId", "testDate", "age", "personality", "testScore", "satisfactionScore"

df$testDate <- as.Date(df$testDate)
df$month <- as.numeric(format(df$testDate, "%m"))
df$dayOfWeek <- weekdays(df$testDate)

# 1. Distribution of Satisfaction Score
hist(df$satisfactionScore,
     main = "Distribution of Satisfaction Score",
     xlab = "Satisfaction Score")

# 2. Test Score vs Satisfaction
plot(df$testScore, df$satisfactionScore,
     main = "Test Score vs Satisfaction",
     xlab = "Test Score",
     ylab = "Satisfaction Score",
     pch = 16)

# Add regression line
abline(lm(satisfactionScore ~ testScore, data = df), col = "red") 

# 3. Age vs Satisfaction
plot(df$age, df$satisfactionScore,
     main = "Age vs Satisfaction",
     xlab = "Age",
     ylab = "Satisfaction Score",
     pch = 16)

# 4. Satisfaction by Personality
boxplot(satisfactionScore ~ personality,
        data = df,
        main = "Satisfaction by Personality",
        xlab = "Personality",
        ylab = "Satisfaction Score")

# 5. Satisfaction over Time
plot(df$testDate, df$satisfactionScore,
     main = "Satisfaction Over Time",
     xlab = "Date",
     ylab = "Satisfaction Score",
     pch = 16)

# 6. Satisfaction by Month
boxplot(satisfactionScore ~ month,
        data = df,
        main = "Satisfaction by Month",
        xlab = "Month",
        ylab = "Satisfaction Score")

# 7. Satisfaction by Day of Week
boxplot(satisfactionScore ~ dayOfWeek,
        data = df,
        main = "Satisfaction by Day of Week",
        xlab = "Day",
        ylab = "Satisfaction Score")

# 8. Satisfaction by Test Score
bins <- cut(df$testScore, breaks = 10)
boxplot(df$satisfactionScore ~ bins,
        main = "Satisfaction by Test Score Bins",
        xlab = "Test Score Bin",
        ylab = "Satisfaction Score",
        las = 2)

# 9. Satisfaction by Age Group
age_bins <- cut(df$age, breaks = c(0,20,30,40,50,60,100))
boxplot(df$satisfactionScore ~ age_bins,
        main = "Satisfaction by Age Group",
        xlab = "Age Group",
        ylab = "Satisfaction Score")

# 10. Outliers
boxplot(df$satisfactionScore,
        main = "Outliers in Satisfaction Score",
        ylab = "Satisfaction Score")


# Graphs Withhin Group Patterns

# 1. Test Score vs Satisfaction

plot(df$testScore[df$personality == "Extrovert"],
     df$satisfactionScore[df$personality == "Extrovert"],
     main = "Extroverts: TestScore vs Satisfaction",
     xlab = "Test Score",
     ylab = "Satisfaction Score",
     pch = 16)
plot(df$testScore[df$personality == "Introvert"],
     df$satisfactionScore[df$personality == "Introvert"],
     main = "Introverts: TestScore vs Satisfaction",
     xlab = "Test Score",
     ylab = "Satisfaction Score",
     pch = 16)

# 2. Satisfaction by Month
boxplot(satisfactionScore ~ month,
        data = df[df$personality == "Introvert", ],
        main = "Introverts: Satisfaction by Month",
        xlab = "Month",
        ylab = "Satisfaction Score")
boxplot(satisfactionScore ~ month,
        data = df[df$personality == "Extrovert", ],
        main = "Extroverts: Satisfaction by Month",
        xlab = "Month",
        ylab = "Satisfaction Score")

# 3. Satisfaction by Day
boxplot(satisfactionScore ~ dayOfWeek,
        data = df[df$personality == "Introvert", ],
        main = "Introverts: Satisfaction by Day",
        xlab = "Day of Week",
        ylab = "Satisfaction Score")
boxplot(satisfactionScore ~ dayOfWeek,
        data = df[df$personality == "Extrovert", ],
        main = "Extroverts: Satisfaction by Day",
        xlab = "Day of Week",
        ylab = "Satisfaction Score")

# 4. Age vs Satisfaction
plot(df$age[df$personality == "Introvert"],
     df$satisfactionScore[df$personality == "Introvert"],
     main = "Introverts: Age vs Satisfaction",
     xlab = "Age",
     ylab = "Satisfaction Score",
     pch = 16)
plot(df$age[df$personality == "Extrovert"],
     df$satisfactionScore[df$personality == "Extrovert"],
     main = "Extroverts: Age vs Satisfaction",
     xlab = "Age",
     ylab = "Satisfaction Score",
     pch = 16)

# 5. Satisfaction by Test Score
intro_data <- df[df$personality == "Introvert", ]
score_bins <- cut(intro_data$testScore, breaks = 10)
boxplot(intro_data$satisfactionScore ~ score_bins,
        main = "Introverts: Satisfaction by Test Score Bins",
        xlab = "Score Bins",
        ylab = "Satisfaction Score",
        las = 2)

# 6. Month by Personality
month_ext <- tapply(df$satisfactionScore[df$personality == "Extrovert"],
                    df$month[df$personality == "Extrovert"], mean)
month_int <- tapply(df$satisfactionScore[df$personality == "Introvert"],
                    df$month[df$personality == "Introvert"], mean)
plot(as.numeric(names(month_ext)), month_ext, type = "l", lty = 1,
     ylim = c(0,10),
     xlab = "Month", ylab = "Average Satisfaction",
     main = "Monthly Trends by Personality")
lines(as.numeric(names(month_int)), month_int, lty = 2)
legend("bottomleft", legend = c("Extrovert", "Introvert"), lty = c(1,2))





# Simple LM to see the main introvert vs extrovert split
lm_personality <- lm(satisfactionScore ~ personality, data = df)
summary(lm_personality)

# Linear model with personality + testScore
lm_score_personality <- lm(satisfactionScore ~ personality + testScore, data = df)
summary(lm_score_personality)





introverts <- df[df$personality == "Introvert", ]
extroverts <- df[df$personality == "Extrovert", ]

# Introvert models
lm_intro1 <- lm(satisfactionScore ~ testScore, data = introverts)
summary(lm_intro1)

lm_intro2 <- lm(satisfactionScore ~ testScore + month, data = introverts)
summary(lm_intro2)

lm_intro3 <- lm(satisfactionScore ~ testScore + month + age, data = introverts)
summary(lm_intro3)

# Extrovert models
lm_extro1 <- lm(satisfactionScore ~ testScore, data = extroverts)
summary(lm_extro1)

lm_extro2 <- lm(satisfactionScore ~ testScore + month, data = extroverts)
summary(lm_extro2)

lm_extro3 <- lm(satisfactionScore ~ testScore + month + age, data = extroverts)
summary(lm_extro3)





df$dayNum <- as.numeric(format(df$testDate, "%j"))
df$weekNum <- as.numeric(format(df$testDate, "%U"))

lm11 <- lm(satisfactionScore ~ personality + testScore + dayNum, data = df)
summary(lm11)

lm12 <- lm(satisfactionScore ~ personality + testScore + weekNum, data = df)
summary(lm12)

lm13 <- lm(satisfactionScore ~ personality + testScore + month + weekNum, data = df)
summary(lm13)





df$weekEven <- df$weekNum %% 2
df$dayMod3 <- df$dayNum %% 3

lm_weird1 <- lm(satisfactionScore ~ personality + testScore + weekEven, data = df)
summary(lm_weird1)

lm_weird2 <- lm(satisfactionScore ~ personality + testScore + factor(dayMod3), data = df)
summary(lm_weird2)

lm_weird3 <- lm(satisfactionScore ~ personality + testScore + testId, data = df)
summary(lm_weird3)





# Read test data
test_df <- read.csv("JBTest-students.csv")

# Test file dates are in m/d/Y format
test_df$testDate <- as.Date(test_df$testDate, format = "%m/%d/%Y")

# Engineer same features on test data
test_df$month <- as.numeric(format(test_df$testDate, "%m"))
test_df$weekNum <- as.numeric(format(test_df$testDate, "%U"))
test_df$isIntrovert <- ifelse(test_df$personality == "Introvert", 1, 0)
test_df$introvertScore <- test_df$isIntrovert * test_df$testScore
test_df$introvertMonth <- test_df$isIntrovert * test_df$month
test_df$weekEven <- test_df$weekNum %% 2

# Predict
pred <- predict(final_lm, newdata = test_df)

# Bound predictions between 1 and 10
pred <- pmax(1, pmin(10, pred))

submission <- data.frame(
  testId = test_df$testId,
  satisfactionScore = pred
)

write.csv(submission, "submission.csv", row.names = FALSE)

# 70/30 cross validation

set.seed(123)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

n <- nrow(df)
train_index <- sample(1:n, size = 0.7 * n)

train_data <- df[train_index, ]
valid_data <- df[-train_index, ]

cv_lm <- lm(
  satisfactionScore ~ isIntrovert + testScore + introvertScore + month + introvertMonth + weekEven,
  data = train_data
)

cv_pred <- predict(cv_lm, newdata = valid_data)

# Bound predictions between 1 and 10
cv_pred <- pmax(1, pmin(10, cv_pred))

cv_rmse <- rmse(valid_data$satisfactionScore, cv_pred)

cat("70/30 Validation RMSE:", cv_rmse, "\n")

rmse_results <- data.frame(
  split = integer(),
  final = numeric()
)

for (i in 1:10) {
  set.seed(100 + i)
  
  train_index <- sample(1:nrow(df), size = 0.7 * nrow(df))
  train_data <- df[train_index, ]
  valid_data <- df[-train_index, ]
  
  cv_lm <- lm(
    satisfactionScore ~ isIntrovert + testScore + introvertScore + month + introvertMonth + weekEven,
    data = train_data
  )
  
  cv_pred <- predict(cv_lm, newdata = valid_data)
  cv_pred <- pmax(1, pmin(10, cv_pred))
  
  rmse_results[i, "split"] <- i
  rmse_results[i, "final"] <- rmse(valid_data$satisfactionScore, cv_pred)
}

print(rmse_results)
cat("Average RMSE:", mean(rmse_results$final), "\n")
cat("SD of RMSE:", sd(rmse_results$final), "\n")
