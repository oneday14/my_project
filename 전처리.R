### 도서관
library(stringr)

# 데이터 불러오기
lib <- read.csv('서울특별시 공공도서관 현황정보(2019).csv',stringsAsFactors = F)

# 필요한 컬럼만 남기기
lib <- lib[,c(2,4,11:13)]

# 공공도서관만 불러오기
lib$도서관.구분명 <- trimws(lib$도서관.구분명, 'both')
lib <- lib[lib$도서관.구분명 == '공공도서관',]

# 비계층적 군집분석(k-means) 실행
kmean1 <- kmeans(lib[,-c(1:3)], centers = 25)   ### 97.4%
lib$cluster1 <- kmean1$cluster

# 군집수 변화 시키기
vscore <- c()

for (i in seq(25,100,5)) {
  m_kmean <- kmeans(lib[,-c(1:4,7:9)], i)
  vscore <- c(vscore, round((m_kmean$betweenss/m_kmean$totss)*100,2))
}

dev.new()
plot(seq(25,100,5), vscore, type = 'o', xlab = 'k의 수',
     ylab = 'score', main = '군집수 변화에 따른 설명력')

# 군집중심과 각 도서관의 거리구하기
# step1) 군집의 중심 추출
dis <- data.frame(kmean1$centers) 
dis$number <- c(1:25)

# step2) 군집과 각 도서관의 조인
lib <- merge(lib, dis, by.x = 'cluster1', by.y = 'number', all = T)

# step3) 군집중심과 각 도서관의 거리구하기
lib$distance <- sqrt((lib$위도.x - lib$위도.y)^2 + (lib$경도.x - lib$경도.y)^2)

################################################
### 치안수
# 데이터 불러오기
crm <- read.csv('범죄발생지_2018.csv', stringsAsFactors = F) 

# 필요한 행, 열만 가져오기
crm <- crm[c(1:3),-c(1:4)]

# 서울지역만 선택
crm[1,] <- trimws(crm[1,],'both')
crm <- crm[,crm[1,]=='서울']

# 전치
crm <- data.frame(t(crm), stringsAsFactors = F)

# 구이름 형태 맞추기
library(stringr)

crm[,2] <- sapply(crm[,2], str_c, '구')
crm[2,2] <- '중구'

# 필요한 컬럼 선택 및 컬럼 이름 바꾸기
crm <- crm[,-1]
colnames(crm)[2] <- '치안수'

# 치안수 숫자포맷으로 변경
crm$치안수 <- sapply(crm$치안수, as.numeric)

##############################################
### 승하차수
# 데이터 불러오기
sub <- read.csv('구별 지하철 승하차 인원.csv', stringsAsFactors = F) 

# 필요한 컬럼 선택
sub <- sub[,c(2,5)]

# 정렬
crm <- crm[order(crm$치안수),]

###############################
### 학생수
# 고등학교 데이터 불러오기
high <- read.csv('2019년도_고등학교학생수.csv', stringsAsFactors = F)

# 서울지역 선택
high <- high[str_detect(high$지역,'서울특별시'),]

# 구 추출
v1 <- c()
for (i in high$지역) {
  v1 <- c(v1, str_split(i, ' ')[[1]][2])
}

high$구 <- v1

# 학생수 숫자포맷으로 변경
high$학생수.계. <- sapply(high$학생수.계., str_remove_all, ',')
high$학생수.계. <- sapply(high$학생수.계., as.numeric)

# 불필요 컬럼 삭제
high <- high[,-1]

# 구별로 그룹화
library(plyr)

high <- ddply(high, .(구), summarise, 학생수 = sum(학생수.계.))

###############################

# 중학교 데이터 불러오기
mid <- read.csv('2019년도_중학교학생수.csv', stringsAsFactors = F)

# 서울지역 선택
mid <- mid[str_detect(mid$지역,'서울특별시'),]

# 구 추출
v2 <- c()
for (i in mid$지역) {
  v2 <- c(v2, str_split(i, ' ')[[1]][2])
}

mid$구 <- v2

# 학생수 숫자포맷으로 변경
mid$학생수.계. <- sapply(mid$학생수.계., str_remove_all, ',')
mid$학생수.계. <- sapply(mid$학생수.계., as.numeric)

# 불필요 컬럼 삭제
mid <- mid[,-1]

# 구별 그룹화
mid <- ddply(mid, .(구), summarise, 학생수 = sum(학생수.계.))

############

# 대학교 데이터 불러오기
univ <- read.csv('대학_재학생수.csv', stringsAsFactors = F)

# 서울지역 선택
univ$지역명 <- trimws(univ$지역명,'both')
univ <- univ[univ$지역명 == '서울',]

# 재학생수 구하기
univ$재학생수 <- univ$재학생.정원내. + univ$재학생.정원외.

# 불필요 컬럼 삭제
univ <- univ[,-c(3,4)]

# 학교이름별 그룹화
univ[univ$학교명 == '서울과학기술대학교(산업대)', 1] <- '서울과학기술대학교'
univ <- ddply(univ, .(학교명), summarise, 재학생수 = sum(재학생수))

# 대학교 위치 데이터 불러오기
loc_univ <- read.csv('대학교위치.csv', stringsAsFactors = F)

# 대학교 위치와 대학별 학생수 조인
univ <- merge(loc_univ, univ, by.x='이름', by.y = '학교명')

# 구별 그룹화
univ <- ddply(univ, .(위치), summarise, 재학생수 = sum(재학생수))

############

# 전문대학교 데이터 불러오기
college <- read.csv('전문대학_재학생수.csv', stringsAsFactors = F)

# 서울지역 선택
college$지역명 <- trimws(college$지역명,'both')
college <- college[college$지역명 == '서울',]

# 재학생수 구하기
college$재학생수 <- college$재학생.정원내. + college$재학생.정원외.

# 불필요 컬럼 삭제
college <- college[,-c(3,4)]

# 학교이름별 그룹화
college <- ddply(college, .(학교명), summarise, 재학생수 = sum(재학생수))
college[20,1] <- '한국폴리텍대학(강서구)'
college[c(21,22),1] <- '한국폴리텍대학(용산구)'
college <- ddply(college, .(학교명), summarise, 재학생수 = sum(재학생수))

# 대학교 위치 데이터 불러오기
loc_college <- read.csv('전문대학교위치.csv', stringsAsFactors = F)

# 대학교 위치와 대학별 학생수 조인
college <- merge(loc_college, college, by.x='이름', by.y = '학교명')
college <- ddply(college, .(위치), summarise, 재학생수 = sum(재학생수))

###########################

# 대학 학생수와 전문대학 학생수 조인
std <- merge(univ, college, by = '위치', all= T)

# 결측치에 0대입
std[,] <- apply(std, c(1,2), str_replace_na,0)
std[,-1] <- apply(std[,-1], c(1,2), as.numeric)

# 대학 재학생수 구하기
std$대학재학생수 <- std$재학생수.x + std$재학생수.y

# 필요한 컬럼 선택
std <- std[,c(1,4)]

# 중고등 학생수 구하기
high$중고등학생수 <- high$학생수 + mid$학생수

# 대학 재학생수와 중고등 학생수 조인
std <- merge(std, high, by.x = '위치',by.y = '구', all= T)

# 필요한 컬럼 선택
std <- std[,-3]

# 결측치에 0대입
std[,-1] <- apply(std[,-1], c(1,2), str_replace_na,0)
std[,-1] <- apply(std[,-1], c(1,2), as.numeric)

# 중, 고, 대학 학생수 구하기
std$학생수 <- std$중고등학생수 + std$대학재학생수

# 필요한 컬럼 선택
std <- std[,c(1,4)]

############################################################
# 모든 데이터 조인

lib_merge <- merge(lib, std, by.x = '구명', by.y = '위치')
lib_merge <- merge(lib_merge, sub, by.x = '구명', by.y = '구')
lib_merge <- merge(lib_merge, crm, by.x = '구명', by.y = 'X2')

#######
# 치안수 숫자형태 변환
str(lib_merge)
lib_merge$치안수 <- sapply(lib_merge$치안수, as.numeric)
