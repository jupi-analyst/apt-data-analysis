install.packages("httr")
install.packages("rvest") # xml file
install.packages("jsonlite") # json file
install.packages("tidyverse")
library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)
#인증키 암호화
usethis::edit_r_environ()
Sys.getenv("DATAGOKR_TOKEN")

#일반 인증키 저장
myKey <- Sys.getenv("DATAGOKR_TOKEN")

#인증키 호출 및 HTTP Request
URL <- 'http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade'
res <- GET(url = URL,
           query = list(LAWD_CD = "11110",
                        DEAL_YMD = "201512",
                        serviceKey = myKey %>% I() ))
print(x = res) #status가 200이면 정상이 아님

#json 형태의 데이터 추출
res %>% 
  content(as = 'text', encoding = 'UTF-8') %>% #res를 text를 전환 
  fromJSON() -> json #json 객체에 할당
str(object = json)

df <- json$response$body$items$item

#월 단위
print(x = df)
seq(from = as.Date(x = '2015-01-01'),
    to = as.Date(x = '2018-12-01'),
    by = '1 month') -> months
print(x = months)
format(x = months, format = '%Y%m') #날짜벡터를 문자벡터로 전환

#2019-01-01 ~ 2019-07-01
seq(from = as.Date(x = '2019-01-01'),
    to = as.Date(x = '2019-07-01'),
    by = '1 month') -> months
format(x = months, format = '%Y%m') -> months

#전체 데이터 저장 객체를 빈 데이터프레임으로 생성
months
result <- data.frame()
for(month in months) {
  cat('현재', month, '기간 거래된 데이터 수집 중! \n')
  
  #HTTP요청
  res <- GET(url = URL,
             query = list(LAWD_CD = "11680",
                          DEAL_YMD = month,
                          serviceKey = myKey %>% I() ))
  #응답 데이터에서 json데이터를 추출
  res %>% 
    content(as = 'text', encoding = 'UTF-8') %>% #res를 text를 전환 
    fromJSON() -> json #json 객체에 할당
  
  df <- json$response$body$items$item
  result <- rbind(result, df)
  
  Sys.sleep(time = 1)
}
str(object = result)

#거래금액을 숫자벡터로 변환
result$거래금액 %>% 
  str_remove(pattern = ',') %>%
  as.numeric() -> result$거래금액

#법정동 문자형 벡터를 범주형 벡터로 변환, level은 문자형벡터 중복 제거한 다음 정렬
head(result$법정동)
as.factor(x = result$법정동) %>% head() # 14개의 레벨
as.factor(x = result$법정동) %>% as.numeric() %>% head() # 역삼동이 레벨10번이라는 뜻

#tidyver purrr 패키지를 이용한 여러개의 문자형 벡터를 범주형으로
map_df(.x = result[, c(4, 5, 10)], #map()은 리스크로 결과를 반환 데이터 프레임을 위해선 _df를 사용
       .f = as.factor) -> result[, c(4, 5, 10)]

getwd()
saveRDS(object = result, file = 'apt_price_2019.RDS')

