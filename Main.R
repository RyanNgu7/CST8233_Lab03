library(dplyr)
# 1.1
my_df = airquality
# 1.2
str(my_df)

# 1.3
names(my_df)

# 1.4
my_df_temp = select(my_df, "Temp")

# 1.6
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
  Month = c(1:5),
  Mean = temp_mean,
  Median = temp_median,
  SD = temp_sd
)

# 1.7
all_temp = my_df$Temp
overall_temp_df = data.frame(
  Mean = mean(all_temp),
  Median = median(all_temp),
  SD = sd(all_temp)
)

# 2.1
all_temp_hist = hist(all_temp)

# 2.2
ozone_monthly_box = boxplot(my_df$Ozone ~ my_df$Month,
        main = "Ozone by Month",
        xlab = "Month",
        ylab = "Ozone"
)
abline(lm(Ozone ~ Temp, data = my_df), col = "blue", lwd = 2)
