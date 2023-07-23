
plastic = read.csv("compt_plastic_wst.csv")

str(plastic)

library(tidyr)

temp = gather(plastic, key = "year", value = "plastic", X1993:X2017)

library(stringr)

temp$year = str_remove(temp$year, "^X")

str(temp)

temp$year = as.numeric(temp$year) 
plastic <- temp 

library(ggplot2)

ggplot(plastic, aes(x = year, y = plastic)) + 
  geom_point()

hdi = read.csv("hdi_human_development_index.csv")

temp = gather(hdi, key = "year", value = "hdindex", X1990:X2021)
temp$year = str_remove(temp$year, "^X")
temp$year = as.numeric(temp$year) 
hdi <- temp 

library(dplyr)

plastic_hdi = plastic %>%
  inner_join(hdi, by = c("country", "year"))

ph = plastic_hdi[complete.cases(plastic_hdi),]

ggplot(ph, aes(x = hdindex, y=plastic)) + 
  geom_point()

p = ggplot(ph, aes(x = hdindex, y=plastic, color = country)) + 
  geom_point() + 
  theme(legend.position = "none")

library(plotly)

ggplotly(p)

wn = read.csv("wn_bothhouses_c.csv")

temp = gather(wn, key = "year", value = "wnparl", X1945:X2021)
temp$year = str_remove(temp$year, "^X")
temp$year = as.numeric(temp$year) 
wn <- temp 


phw = ph %>%
  inner_join(wn, by = c("country", "year"))

phw = phw[complete.cases(phw),]

p = ggplot(phw, aes(x = hdindex, y=plastic, color = wnparl, size = wnparl)) + 
  geom_point(alpha = 0.5) + 
  scale_color_continuous() 
ggplotly(p)

# 여성 국회의원 비율(남녀 정치적 공평성)과 Gini index (경제적 불평등)이 생활수준이 높은 나라에서 폐기물 중 플라스틱 비율과 관련이 있을까?

# 얼마나 관련성이 있을까? 
# 유의한 관련성인가? 
# 얼마나 강한 관련성인가? 
# 추정의 불확실성은 어느 정도인가? 

# 생활 수준이 높은 나라들을 선정 
# Gini index vs. plastic waste 
# Women parliament comp vs. plastic waste 

