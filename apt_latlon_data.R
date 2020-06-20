result = readRDS(file = 'apt_price_2019.RDS')
result
usethis::edit_r_environ() ## REST API
Sys.getenv('KAKAO_MAP_API_KEY')
library(httr)
library(jsonlite)
library(tidyverse)

query = URLencode(iconv("?query=서울특별시 종로구 사직로 161", to = 'UTF-8')) #한글쿼리 인코딩
#카카오맵 REST API로 주소검색 HTTP요청
res = GET(url = paste0('https://dapi.kakao.com/v2/local/search/address.json',query),
          add_headers('Authorization' = str_c('KakaoAK ',Sys.getenv('KAKAO_MAP_API_KEY'))))
print(res)

#다른 방법  인코딩 테스트
# query_test = URLencode(iconv(paste0("?query=",df$지번주소[1]), to = 'UTF-8'))
# query_test
# test_res = GET(url = paste0('https://dapi.kakao.com/v2/local/search/address.json',
#                query_test = URLencode(iconv(paste0("?query=",df$지번주소[1]), to = 'UTF-8'))),
#                add_headers('Authorization' = str_c('KakaoAK ',Sys.getenv('KAKAO_MAP_API_KEY'))))
# print(test_res)
coord <- res %>% content(as = 'text') %>% fromJSON()
str(coord)
coord$documents$y #위도
coord$documents$x #경도

#------------------------------------------------------------------------------------------

#강남구 아파트 단지별 전용면적당 평균 거래금액 계산
result %>%
  mutate(지번주소 = str_c('서울 강남구', 법정동, 지번, sep = ' '),
         면적평균 = 거래금액 / 전용면적) %>%
  select(아파트, 지번주소, 면적평균) %>%
  group_by(아파트, 지번주소) %>%
  summarise(단지평균 = mean(면적평균)) -> df
df
df$위도 <- NA #위도 경도 추가
df$경도 <- NA

for(i in 1:nrow(df)) {
  cat('현재', i, '번째 주소의 위경도 좌표를 얻고 있습니다. \n')
  
  # HTTP 요청, query 한글 인코딩
  query = URLencode(iconv(paste0("?query=",df$지번주소[i]), to = 'UTF-8'))
  res <- GET(url = paste0('https://dapi.kakao.com/v2/local/search/address.json', query),
             add_headers('Authorization' = str_c('KakaoAK ',Sys.getenv('KAKAO_MAP_API_KEY'))))
  
  tryCatch({
    # coord 객체에 저장
    coord <- res %>% content(as = 'text') %>% fromJSON()
    
    # 위도와 경도를 추가
    df$위도[i] <- coord$documents$y
    df$경도[i] <- coord$documents$x
    
    # 1초 쉬기
    Sys.sleep(time = 1)
  }, error = function(e) cat('---> 에러가 발생해서 건너 뚭니다\n'))
  
}
is.na(df$위도) %>% sum # 3개의 NA가 존재

#위도가 NA인 건 모두 삭제 
df <- dplyr::filter(.data = df, is.na(x = 위도) == FALSE)
head(df)
table(is.na(df))
saveRDS(df, file = 'apt_price_2019_df.RDS')
