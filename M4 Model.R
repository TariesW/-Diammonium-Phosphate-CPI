library(readxl)
library(readxl)
DAP_vs_HOUST <- read_excel("Documents/准雀/3-Time Series Analysis:Forecasting:Regression-Diammonium Phosphate CPI:/DAP vs HOUST.xlsx")
View(DAP_vs_HOUST)
date1 <- DAP_vs_HOUST$...1
DAP <- DAP_vs_HOUST$`Diammonium phosphate, US Gulf NOLA DAP Export Spot Price per MT, USD/metric tonne`
HOUST <- DAP_vs_HOUST$`US privately owned house， thousands of units`
df2 <- data.frame(date1, DAP, HOUST)
head(df2)

library(tidyr)
library(dplyr)

df3 <- df2 %>%
  select(date1, DAP, HOUST)%>%
  gather(key = "variable", value = "value", -date1)

head(df3)
library(ggplot2)
ggplot2(df3, aes(x = date1, y=value)) +
  geom_line(aes(color = variable), size = 1) + 
  scale_color_manual(values=c("#E7B800","#00AFBB"))+
  theme_minimal()

library(forecast)
df2_ts <- ts(df2[,2,3], start = c(1980,01), frequency = 12)
head(df2_ts)
summary(model_lm_ts)

forecast(df2_ts, h=60)
forecast_m4 <- forecast(df2_ts, h=60)
summary(forecast_m4)


  
  