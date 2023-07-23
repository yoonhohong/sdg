
# 가설
# 생활수준이 높은 나라일수록 플라스틱 폐기량이 많을까?   

# 작업 디렉토리 설정하기 
setwd("~/Dropbox/Github/sdg")

# 데이터 불러오기 
# 폐기물 중 플라스틱 함유 비율 
plastic = read.csv("compt_plastic_wst.csv")

# 데이터프레임 구조 변경하기: wide 포맷 -> long 포맷 
library(tidyr)
temp = gather(plastic, key = "year", value = "plastic", X1993:X2017)

# 년도 앞에 있는 "X" 문자를 떼어내기 
library(stringr)
temp$year = str_remove(temp$year, "^X")

# 년도를 수치 자료형으로 변경하기    
temp$year = as.numeric(temp$year) 
plastic <- temp 

# 데이터 시각화하기: 산점도   
library(ggplot2)
ggplot(plastic, aes(x = year, y = plastic)) + 
  geom_point()

# 데이터 불러오기 
# human development index: life expectancy, education, gross national income per capita 
hdi = read.csv("hdi_human_development_index.csv")

# 데이터프레임 구조 변경하기: wide 포맷 -> long 포맷 
temp = gather(hdi, key = "year", value = "hdindex", X1990:X2021)
temp$year = str_remove(temp$year, "^X")
temp$year = as.numeric(temp$year) 
hdi <- temp 

# 데이터프레임 병합하기  
library(dplyr)
plastic_hdi = plastic %>%
  inner_join(hdi, by = c("country", "year"))

# 결측치를 포함한 관측치는 제외시키기    
ph = plastic_hdi[complete.cases(plastic_hdi),]

# hdindex 와 plastic 의 관계를 산점도로 시각화하기 
ggplot(ph, aes(x = hdindex, y=plastic)) + 
  geom_point()

# 산점도 탐색하기  
p = ggplot(ph, aes(x = hdindex, y=plastic, color = country)) + 
  geom_point() + 
  theme(legend.position = "none")

library(plotly)
ggplotly(p)

# 예상대로 생활수준이 높은 나라일수록 폐기물 중 플라스틱 비율이 높은 경향이 있다.    


# 그런데, 생활수준이 높은 나라들 가운데에서도 편차가 크다. 이유가 뭘까?  

# 혹시, 평균의 함정?   
# 부의 불평등, 성 불평등이 관련이 있지 않을까?     
# 불평등 지표: 지니 지수(Gini index), 여성 국회의원 비율    

# 가설: 생활수준이 높은 나라에서 폐기물중 플라스틱 비율이 여성 국회의원 비율(남녀 정치적 공평성) 혹은 Gini index (경제적 불평등)와 관련이 있지 않을까?

# 데이터 불러오기: 여성 국회의원 비율  
# 데이터 전처리 
# 데이터 구조 변경(wide -> long), 자료형 변경(년도를 수치형으로), 기존 데이터와 병합, 결측 관측치 제외        
wn = read.csv("wn_bothhouses_c.csv")
temp = gather(wn, key = "year", value = "wnparl", X1945:X2021)
temp$year = str_remove(temp$year, "^X")
temp$year = as.numeric(temp$year) 
phw = ph %>%
  inner_join(temp, by = c("country", "year"))
phw = phw[complete.cases(phw),]

# 시각화: 산점도    
# hindex와 plastic 관계에 대한 산점도에서 여성 국회의원 비율을 점의 크기와 색으로 표현    
p = ggplot(phw, aes(x = hdindex, y=plastic, color = wnparl, size = wnparl)) + 
  geom_point(alpha = 0.5) + 
  scale_color_continuous() 
ggplotly(p)

# 예상대로 생활수준이 높은 나라들에서 플라스틱 폐기률과 여성 국회의원 비율 간에는 연관성이 있어 보인다. 반면, 생활수준이 높지 않은 나라들에서는 그렇지 않아 보인다.    

# 좀 더 구체적으로 분석해보자. 어떻게?    

# 생활수준에 따라 전체를 둘로 구분해서, 여성 국회의원의 비율과 플라스틱 폐기율간 상관관계에 차이가 있는지 알아보자. 
# 가설은? 차이가 있을 것이다.    

phwg = phw %>%
  mutate(hdi_gr = ifelse(hdindex < median(hdindex), "low", "high"))

ggplot(phwg, aes(x = wnparl, y=plastic)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ hdi_gr)

# 통계적 추론: 추정과 검정     
# 얼마나 강한 관련성인가?      
# 유의한 관련성인가? 
# 추론(추정, 검정)의 불확실성은 어느 정도인가? 

# 부분 그룹으로 나누기     
df = phwg 
df_lo = df %>%
  filter(hdi_gr == "low")
df_hi = df %>%
  filter(hdi_gr == "high")

# 회귀 분석: 단순 선형회귀 
lm_lo = lm(plastic ~ wnparl, data = df_lo)
summary(lm_lo)

lm_hi = lm(plastic ~ wnparl, data = df_hi)
summary(lm_hi)

# 상관관계: 전체와 부분   
cor.test(df$plastic, df$wnparl)
cor.test(df_lo$plastic, df_lo$wnparl)
cor.test(df_hi$plastic, df_hi$wnparl)





