library(stringr)

# 서울에있는 중학교위치
mid = read.csv('2019년도_학교위치정보.csv', stringsAsFactors = F)
mid = mid[str_detect(mid$지역,'서울'),]

# 서울에있는 고등학교위치 
high = read.csv('2019년도_학교위치정보(고등학교).csv', stringsAsFactors = F)
high = high[str_detect(high$지역,'서울'),]

#  서울에있는 중학교별 학생수
mid_s = read.csv('2019년도_학생수.csv', stringsAsFactors = F)
mid_s = mid_s[str_detect(mid_s$지역,'서울'),]

# 서울에있는 고등학교별 학생수
high_s = read.csv('2019년도_학생수(고등학교).csv', stringsAsFactors = F)
high_s = high_s[str_detect(high_s$지역,'서울'),]

library(plyr)

# 조인
mid = merge(mid, mid_s, by = '학교명')
high = merge(high, high_s, by = '학교명')

# 문자제거
mid$학생수.계. <- sapply(mid$학생수.계.,str_remove_all,',')
high$학생수.계. <- sapply(high$학생수.계.,str_remove_all,',')

# 숫자변형
mid$학생수.계. <- sapply(mid$학생수.계.,as.numeric)
high$학생수.계. <- sapply(high$학생수.계.,as.numeric)

# 컬럼이름변경
colnames(mid) <- c("학교명", "지역", "위도", "경도", "남녀공학", "학생수")
colnames(high) <- c("학교명", "지역", "위도", "경도", "남녀공학", "학생수")

# 공백분리후 첫번째 단어추출 함수
f1 <- function(x) {
  str_split(x,' ')[[1]][2]
}

# 함수 적용
mid$지역 <- sapply(mid$지역, f1)
high$지역 <- sapply(high$지역, f1)

# 대학교위치
univ <- read.csv('대학교위치.csv',stringsAsFactors = F)

# 대학교 위도, 경도 추출
univ_geo <- geocode(univ$이름)
univ$경도 <- univ_geo$lon
univ$위도 <- univ_geo$lat

# 오류 확인 및 수정
univ_geo_na <- geocode(c('서울특별시 노원구 화랑로 815','서울특별시 동작구 사당로 143'))
univ[univ$이름 == '삼육대학교',c('경도','위도')] <- univ_geo_na[1,]
univ[univ$이름 == '총신대학교',c('경도','위도')] <- univ_geo_na[2,]

# 전문대학교위치
college <- read.csv('전문대학교위치.csv',stringsAsFactors = F)

# 전문대학교 위도, 경도 추출
college_geo <- geocode(college$이름)
college$경도 <- college_geo$lon
college$위도 <- college_geo$lat

# 대학교, 전문대학교 학생수
univ_s <- read.csv('대학_재학생수.csv',stringsAsFactors = F)
college_s <- read.csv('전문대학_재학생수.csv',stringsAsFactors = F)

# 서울만 추출
univ_s <- univ_s[univ_s$지역명 == '서울',]
college_s <- college_s[college_s$지역명 == '서울',]

# 재학생수 구하기
univ_s$학생수 <- univ_s$재학생.정원내. + univ_s$재학생.정원외.
college_s$학생수 <- college_s$재학생.정원내. + college_s$재학생.정원외.

# 불필요 컬럼 삭제
univ_s <- univ_s[,-c(3,4)]
college_s <- college_s[,-c(3,4)]

# 학교이름별 그룹화
univ_s[univ_s$학교명 == '서울과학기술대학교(산업대)', 1] <- '서울과학기술대학교'
univ_s <- ddply(univ_s, .(학교명), summarise, 학생수 = sum(학생수))

college_s <- ddply(college_s, .(학교명), summarise, 학생수 = sum(학생수))
college_s[20,1] <- '한국폴리텍대학(강서구)'
college_s[c(21,22),1] <- '한국폴리텍대학(용산구)'
college_s <- ddply(college_s, .(학교명), summarise, 학생수 = sum(학생수))

# 대학교 위치와 대학별 학생수 조인
univ <- merge(univ, univ_s, by.x='이름', by.y = '학교명')
college <- merge(college, college_s, by.x='이름', by.y = '학교명')

# 남녀공학 판단
univ$남녀공학 <- ifelse(str_detect(univ$이름,'여'),'녀','남녀공학')
college$남녀공학 <- ifelse(str_detect(college$이름,'여'),'녀','남녀공학')

# 컬럼순서변경
univ <- univ[,c('이름','위치','위도','경도','학생수','남녀공학')]
college <- college[,c('이름','위치','위도','경도','학생수','남녀공학')]

# 컬럼이름변경
colnames(univ) <- c("학교명", "지역", "위도", "경도", "남녀공학", "학생수")
colnames(college) <- c("학교명", "지역", "위도", "경도", "남녀공학", "학생수")

# 불필요 컬럼 삭제
mid <- mid[,-6]
high <- high[,-6]

# 컬럼이름변경
colnames(mid) <- c("학교명", "지역", "위도", "경도", "남녀공학", "학생수")
colnames(high) <- c("학교명", "지역", "위도", "경도", "남녀공학", "학생수")

# 중학교, 고등학교, 대학교, 전문학교 합치기
school <- rbind(mid, high, univ, college)
write.csv(school,'학교위치및학생수.csv')

# 비계층적 군집분석(k-means) 실행
kmean1 <- kmeans(school[,c(4,5)], centers = 45)   ### 98.6%

# 군집 컬럼 추가
school$cluster1 <- kmean1$cluster

# 시각화
school$cluster1 <- as.factor(school$cluster1)
a1.map <- ggmap(a1) + geom_point(data=school, aes(x=경도, y=위도, col = school$cluster1),size=1,alpha=0.7)

dev.new()

# 오류발견 및 수정
err <- school[school$위도 < 37.45,]
school[school$학교명 == '신광여자중학교',c('경도','위도')] <- geocode('서울특별시 용산구 청파로 263')
school[school$학교명 == '경기대학교',c('경도','위도')] <- geocode('서울특별시 서대문구 경기대로9길 24')
school[school$학교명 == '명지대학교',c('경도','위도')] <- geocode('서울특별시 서대문구 거북골로 34')
school[school$학교명 == '금호고등학교',c('경도','위도')] <- geocode('서울특별시 성동구 금호로 118')
e1 <- geocode(err$학교명)
err$경도 <- e1$lon
err$위도 <- e1$lat
school[school$위도 < 37.45,] <- err
e1 <- geocode(c('서울특별시 금천구 시흥대로38길 60',
                '서울특별시 금천구 시흥대로38길 62',
                '서울특별시 금천구 시흥대로38길 26',
                '서울특별시 금천구 시흥대로47길 45'))


# 군집수 변화 시키기
vscore <- c()

for (i in seq(25,150,5)) {
  m_kmean <- kmeans(school[,c(4,5)], i)
  vscore <- c(vscore, round((m_kmean$betweenss/m_kmean$totss)*100,2))
}

# elbow curve 시각화
dev.new()
par(mfrow=c(2,1))
plot(seq(25,150,5), vscore, type = 'o', xlab = '군집의 수',
     ylab = 'score', main = '군집수 변화에 따른 설명력')      # k= 45로 선정

# 군집의 중심 
dis <- data.frame(kmean1$centers) 
dis$number <- c(1:45)

# 각학교가 해당하는 군집 조인
school <- merge(school, dis, by.x = 'cluster1', by.y = 'number')
colnames(school) <- c('cluster1',"학교명", "지역", "위도", "경도", "남녀공학", "학생수","위도중심","경도중심")

# 오류발견 및 수정
err2 <- school[str_detect(school$남녀공학,'[0-9]'),]
colnames(err2) <- c("학교명", "지역", "위도", "경도", "학생수","남녀공학", 'cluster1')
err2 <- err2[,c("학교명", "지역", "위도", "경도","남녀공학", "학생수", 'cluster1')]
school[str_detect(school$남녀공학,'[0-9]'),] <- err2[,]

# 각 군집중심과의 거리 구하기
school$distance <- sqrt((school$위도 - school$위도중심)^2 + (school$경도 - school$경도중심)^2)

# 남녀공학, 남, 여 가중치 두기
school$학생수 <- sapply(school$학생수, as.numeric)
school$학생수가중치 <- ifelse(school$남녀공학 == '남', school$학생수 * 0.95, ifelse(school$남녀공학 == '녀', school$학생수 * 0.9, school$학생수))
school$학생수가중치 <- sapply(school$학생수가중치,round, 0)

# 지하철유동인구수
sub <- read.csv('구별 지하철 승하차 인원.csv', stringsAsFactors = F)
sub <- sub[,c(2,5)]

# 컬럼 이름변경
colnames(sub) <- c('지역','승하차인원')

# 구별 지하철 유동인구수 조인
school <- merge(school, sub, by = '지역')

# 정규화 -> 대학교의 학생수가 압도적으로 커서 군집별로 그룹화하기로 결정
school$학생수가중치_sc <- round((school$학생수가중치 - min(school$학생수가중치)) / (max(school$학생수가중치) - min(school$학생수가중치)), 5)
school$승하차인원_sc <- round((school$승하차인원 - min(school$승하차인원)) / (max(school$승하차인원) - min(school$승하차인원)), 5)
school$distance_sc <- round((school$distance - min(school$distance)) / (max(school$distance) - min(school$distance)), 5)
school$distance_sc <- 1 - school$distance_sc

# 군집별 학생수
df1 <- ddply(school, .(cluster1), summarise, 학생수 = sum(학생수))
school <- merge(school, df1, by = 'cluster1')

# 정규화
school$학생수.y_sc <- round((school$학생수.y - min(school$학생수.y)) / (max(school$학생수.y) - min(school$학생수.y)), 5)
school$승하차인원_sc <- round((school$승하차인원 - min(school$승하차인원)) / (max(school$승하차인원) - min(school$승하차인원)), 5)
school$distance_sc <- round((school$distance - min(school$distance)) / (max(school$distance) - min(school$distance)), 5)

# 접근성 구하기 
school$func <- round((school$학생수.y_sc * school$승하차인원_sc)/school$distance_sc,5)
school <- school[order(school$func, decreasing = T),]

# 서울 구별 5대 범죄수
crm <- read.csv('범죄발생지_2018.csv', stringsAsFactors = F)
crm <- crm[str_detect(crm$범죄별.2.,'강력범죄') | str_detect(crm$범죄별.2.,'범죄별'), str_detect(crm[1,],'서울') | str_detect(crm[1,],'범죄')]
crm <- crm[,-c(1:4)]
crm <- crm[-1,]
crm[1,] <- str_c(crm[1,],'구')
colnames(crm) <- crm[1,]

# 문자제거
crm[,] <- apply(crm, c(1,2),str_replace_all,'-','0')

# 숫자변경
crm[,] <- apply(crm, c(1,2),as.numeric)

# 데이터 정리
crm <- apply(crm, 2,sum)
crm <- stack(crm)

# 컬럼이름변경
colnames(crm) <- c('강력범죄수','지역')
crm$지역 <- as.character(crm$지역)
crm[2,2] <- '중구'
write.csv(crm,'강력범죄수.csv')
crm[order(crm$강력범죄수),]

# 청소년 범죄
crm2 <- read.csv('서울 청소년 범죄_2020.csv', stringsAsFactors = F)
crm2$구분 <- str_c(crm2$구분,'구')
crm2 <- crm2[-c(32,33),-7]

# 데이터 정리
crm2$범죄 <- apply(crm2[,-1] , 1, sum)
crm2 <- ddply(crm2, .(구분), summarise, 범죄 = sum(범죄))
write.csv(crm2, '구별청소년범죄.csv')

# 5대범죄와 청소년 범죄 합치기
crm <- merge(crm, crm2, by.x = '지역',by.y='구분')
crm$총범죄수 <- apply(crm[,-1],1,sum)
crm <- crm[order(crm$총범죄수),]
write.csv(crm, '구별치안수.csv')

# 컬럼이름변경
colnames(crm) <- c('지역','치안수')
crm[order(crm$치안수),]

# 지도 시각화
library(ggmap)
register_google(key='')
a1.map <- ggmap(a1) + geom_point(data=school[school$학교명 %in% c('경신중학교','홍익대학교사범대학부속중학교','환일고등학교'),], aes(x=경도, y=위도),size=1,alpha=0.7)
a1.map

# 상위 3개 학교 추출
school <- school[order(school$func),]
top3 <- school[3,]

library(devtools)
a1 <- get_googlemap(center=c(lon = top3[1,4],lat = top3[1,3]),zoom=15, maptype = 'roadmap')
a2 <- get_googlemap(center=c(lon = top3[2,4],lat = top3[2,3]),zoom=15, maptype = 'roadmap')
a3 <- get_googlemap(center=c(lon = top3[3,4],lat = top3[3,3]),zoom=15, maptype = 'roadmap')

dev.new()

a1.map <- ggmap(a1) + geom_point(data=top3[1,c(3,4)], aes(x=경도, y=위도),size=2,alpha=1, colour = 'red') + geom_point(data=top3[1,c(3,4)], aes(x=경도, y=위도),size=70,alpha=0.6, colour = 'pink')
a2.map <- ggmap(a2) + geom_point(data=top3[2,c(3,4)], aes(x=경도, y=위도),size=2,alpha=1, colour = 'red') + geom_point(data=top3[2,c(3,4)], aes(x=경도, y=위도),size=70,alpha=0.6, colour = 'pink')
a3.map <- ggmap(a3) + geom_point(data=top3[3,c(3,4)], aes(x=경도, y=위도),size=2,alpha=1, colour = 'red') + geom_point(data=top3[3,c(3,4)], aes(x=경도, y=위도),size=70,alpha=0.6, colour = 'pink')
a1.map


