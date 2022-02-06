library(readxl)
DAP_vs_CPI <- read_excel("Documents/准雀/3-Time Series Analysis:Forecasting:Regression-Diammonium Phosphate CPI:/DAP vs CPI.xlsx")
View(DAP_vs_CPI)

date <- DAP_vs_CPI$...1
DAP <- DAP_vs_CPI$`Diammonium phosphate, US Gulf NOLA DAP Export Spot Price per MT, USD/metric tonne`
CPI <- DAP_vs_CPI$USACPIALLMINMEI
df1 <- data.frame(date, DAP, CPI)
head(df1)

library(tidyr)
library(dplyr)

df2 <- df1 %>%
  select(date, DAP, CPI) %>%
  gather(key = "variable", value = "value",  -date)

head(df2)

library(ggplot2)
ggplot(df2, aes(x = date, y = value))+
  geom_line(aes(color = varible), size = 1) + 
  scale_color_manual(values = c("#E7B800","#00AFBB")) + 
  theme_minimal()

library(forecast)
df1_ts <- ts(df1[,2,3], start = c(1980,01), frequency = 12)
df1_ts

model_lm_ts <- tslm(DAP ~ CPI, data = df1_ts)
summary(model_lm_ts)
coef(model_lm_ts)
forecast(df1_ts, h=12)
forecast_m3 <- forecast(df1_ts, h=60)
summary(forecast_m3)








  
  