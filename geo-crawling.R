library(tidyverse)
library(httr)
library(rvest)
library(magrittr)
library(jsonlite)
library(purrr)
getwd()
# 기준년원을 지정
month <- '201909'

#area.RDS 우리나라 법정코드 파일 www.code.go.kr
area <- readRDS(file = 'area.RDS')
str(area)
head(area, 10L)

# 서울 강남구만 선택, code 컬럼을 8자리로 변경
area <- area %>%
  filter(sido == '서울특별시' & sigg == '강남구') %>%
  mutate(code = str_sub(string = code, start = 1, end = 8))
area

aptList <- data.frame()
# 반복문을 실행하여 서울특별시 아파트 단지 정보를 수집
for(i in 1:length(area$code)) {
  cat('현재', i, '번 째 법정동 데이터를 수집 중입니다.')
  
  # HTTP 요청
  res <- POST(url = 'http://www.k-apt.go.kr/kaptinfo/getKaptList.do',
              body = list(bjd_code = area$code[i], 
                          search_date = month),
              encode = 'form')
  
  # 아파트 단지 정보가 없는 법정동은 건너 뜀
  tryCatch({
    # res 객체에서 JSON 데이터 추출
    json <- res %>%
      content(as = 'text', encoding = 'UTF-8') %>%
      fromJSON()
    
    # json 객체는 리스트이며, 첫 번쨰 원소가 데이터 프레임
    df <- json$resultList
    
    aptList <- rbind(aptList, df)
    cat('\n')
  }, error = function(e) {cat('-------> 아파트 단지 목록이 없음 \n')})
  
  Sys.sleep(time = 10L)
}
aptList
saveRDS(aptList, file = 'aptList.RDS')


# 아파트 상세정보를 수집하는 사용자 정의 함수를 생성.
getAptInfo = function(aptCode) {
  
  # Request URL를 지정
  reqURL <- 'http://www.k-apt.go.kr/kaptinfo/getKaptInfo_detail.do'
  
  # HTTP 요청
  res <- POST(url = reqURL,
              body = list(kapt_code = aptCode),
              encode = 'form')
  
  json <- res %>%
    content(as = 'text', encoding = 'UTF-8') %>%
    fromJSON()
  
  # json 객체에서 두 번쨰 데이터 프레임인 '상세정보' 추출
  detl <- json$resultMap_kapt %>%
    plyr::ldply(.fun = as.data.frame) %>%
    t() %>%
    as.data.frame() %>%
    set_rownames(value = NULL) %>%
    set_colnames(value = .[1, ]) %>%
    slice(2)
  
  detl <- detl %>%
    select(KAPT_CODE,
           CODE_SALE,
           CODE_HEAT,
           KAPT_TAREA,
           KAPT_DONG_CNT,
           CODE_APT,
           CODE_HALL,
           KAPT_USEDATE,
           KAPT_MAREA,
           KAPT_MGR_CNT,
           KAPTD_SCNT,
           KAPTD_CLCNT,
           KAPTD_DCNT,
           KAPTD_PCNT,
           KAPTD_PCNTU,
           KAPTD_CCCNT,
           KAPTD_ECAPA,
           KAPTD_ECNTP,
           CODE_NET,
           KAPTD_WTIMEBUS,
           KAPTD_WTIMESUB)
  
  # 세 번째 '세대정보' 추출 
  count <- json$resultMap_kapt_areacnt %>%
    filter(KAPTDA_CNT != 'NA') %>%
    mutate(KAPTDA_CNT = as.numeric(KAPTDA_CNT)) %>%
    summarise(KAPTDA_TTL = sum(KAPTDA_CNT))
  
  # 세대 수 합계가 없으면 NA
  if(nrow(count) == 0) {
    count <- data.frame(KAPTDA_TTL = NA)
  }
  
  # 네 번쨰 '주소정보' 추출
  jibun <- json$resultMap_kapt_addrList %>%
    filter(ADDR_GBN =='B') %>%
    slice(1) %>%
    select(ADDR) %>%
    rename(JB_ADDR = ADDR)
  
  road <- json$resultMap_kapt_addrList %>%
    filter(ADDR_GBN == 'R') %>%
    slice(1) %>%
    select(ADDR) %>%
    rename(RD_ADDR = ADDR)
  
  # 도로명 주소 없으면 NA
  if(nrow(road) == 0) {
    road <- data.frame(RD_ADDR = NA)
  }
  
  df <- cbind(detl, count, jibun, road)
  return(df)
}

aptInfo <- data.frame()

for(i in 1:length(aptList$KAPT_CODE)) {
  
  # 작업 진행 상황
  cat('현재', i, '번 째 법정동 수집')
  
  # 아파트 단지 상세 정보가 없으면 건너 뜀
  tryCatch({
    
    # 아파트 상세정보 수집
    df <- getAptInfo(aptCode = aptList$KAPT_CODE[i])
    
    # 최종 결과 객체에 추가
    aptInfo <- rbind(aptInfo, df)
    cat('\n')
  }, error = function(e) cat('--> 상제 정보 없음\n'))
  
  Sys.sleep(time = 10L)
}
str(aptInfo)



# 전처리
# 범주형으로 변환해야 할 컬럼을 지정
cols <- c('CODE_SALE', 'CODE_HEAT', 'CODE_APT', 'CODE_HALL',
          'CODE_NET', 'KAPTD_WTIMEBUS', 'KAPTD_WTIMESUB')
cols
# 해당 컬럼에 대해 빈도수를 확인
map(.x = aptInfo[cols], .f = table)

# CODE_SALE 컬럼에 '임대'가 포함되어 있으면 '임대',
# 없으면 '분양'의 값을 갖도록 덮음
aptInfo$CODE_SALE <- str_detect(string = aptInfo$CODE_SALE,
                                pattern = '임대') %>%
  ifelse(yes = '임대', no = '분양')

# CODE_HEAT 컬럼이 '중앙난방'과 '지역난방'인 행을 하나로 합쳐
# '개별난방'거ㅣ '중앙난방'만 갖도록 덮음
aptInfo$CODE_HEAT <- ifelse(test = aptInfo$CODE_HEAT == '지역난방',
                            yes = '중앙난방',
                            no = '개별난방')

# CODE_APT 컬럼이 '-'와 '연립주택'인 행은 삭제합니다.
aptInfo <- aptInfo %>%
  filter(CODE_APT %in% c('아파트', '주상복합'))

# CPDE_HALL 컬럼이 '타워형'인 행은 삭제
aptInfo <- aptInfo %>%
  filter(CODE_HALL != '타워형')

# KAPTD_WTIMEBUS 컬럼이 '05분이내', '10분이내', '10분초과'의 값을 갖도록
aptInfo <- aptInfo %>%
  mutate(KAPTD_WTIMEBUS = case_when(
    KAPTD_WTIMEBUS == '5분이내' ~ '05분이내',
    KAPTD_WTIMEBUS == '5~10분이내' ~ '10분이내',
    KAPTD_WTIMEBUS == '10~15분이내' ~ '10분초과',
    KAPTD_WTIMEBUS == '15~20분이내' ~ '10분초과',
    KAPTD_WTIMEBUS == '20분초과' ~ '10분초과'))

# KAPTD_WTIMESUB 컬럼이 '5분이내', '10분이내', '10분초과'의 값을 
aptInfo <- aptInfo %>%
  mutate(KAPTD_WTIMEBUS = case_when(
    KAPTD_WTIMEBUS == '5분이내' ~ '05분이내',
    KAPTD_WTIMEBUS == '5~10분이내' ~ '10분이내',
    KAPTD_WTIMEBUS == '10~15분이내' ~ '10분초과',
    KAPTD_WTIMEBUS == '15~20분이내' ~ '10분초과',
    KAPTD_WTIMEBUS == '20분초과' ~ '10분초과',
    TRUE ~ '10분초과'))

imsi <- aptInfo %>%
  mutate(KAPTD_WTIMEBUS = case_when(
    KAPTD_WTIMEBUS == '5분이내' ~ '05분이내',
    KAPTD_WTIMEBUS == '10분이내' ~ '10분이내',
    KAPTD_WTIMEBUS == '10분초과' ~ '10분초과',
    TRUE ~ '10분초과'))

# 위 칼럼을 범주형 벡터로 변환
aptInfo[cols] <- map_df(.x = aptInfo[cols], .f = as.factor)

# 숫자형으로 변환해야 할 컬럼을 지정
cols <- c('KAPT_TAREA', 'KAPT_DONG_CNT', 'KAPT_MAREA', 'KAPT_MGR_CNT',
          'KAPTD_SCNT', 'KAPTD_CLCNT', 'KAPTD_DCNT', 'KAPTD_PCNT',
          'KAPTD_PCNTU', 'KAPTD_CCCNT', 'KAPTD_ECAPA', 'KAPTD_ECNTP')

aptInfo[cols] <- map_df(.x = aptInfo[cols], .f = as.numeric)

aptInfo <- aptInfo %>% filter(complete.cases(.))

saveRDS(aptInfo, file = 'aptInfo.RDS')
