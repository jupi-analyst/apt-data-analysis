Sys.getenv('GOOGLE_MAP_KEY')
library(tidyverse)
library(ggmap)
register_google(key = Sys.getenv('GOOGLE_MAP_KEY'))
geocode(location = '서울특별시', output = 'latlona', source = 'google')

qmap(location = c(lon = 126.9768,
                  lat = 37.5759),
     zoom = 16,
     maptype = 'satellite',
     source = 'google') # 생략 가능


df <- readRDS(file = 'apt_price_2019_df.RDS')
head(df)
# 위경도를 숫자 벡터로 변환
df$경도 <- as.numeric(df$경도)
df$위도 <- as.numeric(df$위도)
# df[, c('위도, 경도')] <- map_df(.x = df[, c('위도', '경도')],
#                             .f = as.numeric)
# 좌표 중위수를 계산 중심좌표 설정
center <- c(lon = median(x = df$경도),
            lat = median(x = df$위도))
center

#중심좌표 기준으로 지도를 불러옴
qmap(location = c(lon = center[1] + 0.01,
                  lat = center[2]) - 0.01,
     zoom = 13,
     maptype = 'hybrid',
     source = 'google') +
  geom_point(data = df,
             mapping = aes(x = 경도,
                           y = 위도,
                           color = 단지평균),
             shape = 19,
             size = 2) +
  scale_color_gradient(low = 'yellow', high = 'red') +
  theme(legend.position = 'None')
