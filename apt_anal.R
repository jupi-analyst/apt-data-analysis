library(tidyverse)
library(extrafont)
result <- readRDS(file = 'apt_price_2019.RDS')

#기술통계량 EDA (평균, 중앙값, 최소, 최대, 사분위, 사분범위, 분산, 표준편차)
mean(x = result$거래금액)
median(result$거래금액)

result$건축년도 %>% 
  table() %>% 
  sort(decreasing = TRUE)

min(result$거래금액)
max(result$거래금액)
range(result$거래금액)
range(result$거래금액) %>% diff()
quantile(result$거래금액)
quantile(result$거래금액, prob = c(0.95, 0.99))
IQR(result$거래금액)
var(result$거래금액)
sd(result$거래금액)

hist(result$거래금액, main = '거래그램 히스토그램', family = 'AppleGothic')
hist(result$거래금액,
     breaks = seq(from = 0, to = 625000, by = 25000),
     freq = FALSE,
     col = 'gray50',
     border = 'gray30',
     main = '거래그램 히스토그램',
     family = 'AppleGothic')
lines(density(result$거래금액),
      lwd = 3,
      col = 'red')

boxplot(result$거래금액, main = '거래금액 상자수염그림', family = 'AppleGothic')
boxplot(formula = 거래금액 ~ 법정동,
        data = result,
        las = 2,
        main = '법정동별 거래금액 상자수염그림',
        family = 'AppleGothic')

plot(x = result$전용면적,
     y = result$거래금액,
     pch = 19,
     col = 'gray50',
     main = '전용면적과 거래 금액 간 산점도')

abline(reg = lm(formula = 거래금액 ~ 전용면적, data = result),
       lwd = 3,
       col = 'red')
