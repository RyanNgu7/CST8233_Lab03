library(dplyr)
# -------- Part 1 -------
# 1.
my_df = airquality
# 2.
str(my_df)

# 3.
names(my_df)

# 4.
my_df_temp = select(my_df, "Temp")

# 6.
temp_mean = c()
temp_median = c()
temp_sd = c()
for (m in 5:9) {
  single_month = my_df[my_df$Month == m,"Temp"]
  temp_mean = append(temp_mean, mean(single_month))
  temp_median = append(temp_median, median(single_month))
  temp_sd = append(temp_sd, sd(single_month))
}

monthly_temp_df = data.frame(
  Month = c(5:9),
  Mean = temp_mean,
  Median = temp_median,
  SD = temp_sd
)

# 7.
all_temp = my_df$Temp
overall_temp_df = data.frame(
  Mean = mean(all_temp),
  Median = median(all_temp),
  SD = sd(all_temp)
)

# -------- Part 2 -------

# 1. histogram of temperature
all_temp_hist = hist(all_temp, breaks = 20)

# 2. Boxplot of ozone by month
ozone_monthly_box = boxplot(my_df$Ozone ~ my_df$Month,
        main = "Ozone by Month",
        xlab = "Month",
        ylab = "Ozone"
)

# 3. Scatterplot of temp vs ozone
all_ozone = my_df$Ozone

plot(x = all_temp, y = all_ozone,
     main = "Temperature's Effect on Ozone",
     xlab = "Temp",
     ylab = "Ozone")
abline(lm(Ozone ~ Temp, data = my_df), col = "blue", lwd = 2)

# 4.
# The probability that the temperature during these months is less than 70:
pnorm(q = 70, mean = mean(all_temp), sd = sd(all_temp), lower.tail = T)

# The probability that the temperature during these months is greater than 85:
pnorm(q = 85, mean = mean(all_temp), sd = sd(all_temp), lower.tail = F)

# The probability that the temperature during these months is less than 90 and 
# greater than 75:
pnorm(q = 90, mean = mean(all_temp), sd = sd(all_temp), lower.tail = T) -
  pnorm(q = 75, mean = mean(all_temp), sd = sd(all_temp), lower.tail = T)

# z scores for all temp values
overall_temp_mean = overall_temp_df$Mean
overall_temp_sd = overall_temp_df$SD

my_df$z_score = (my_df$Temp - overall_temp_mean)/overall_temp_sd

