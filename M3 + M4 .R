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




