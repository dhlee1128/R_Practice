#### 반복측정 분산분석(Repeated Measures ANOVA) #####

#매일 유산소 운동에 따른 체지방률 감소 (20~40대 남성)

# 01.데이터 불러오기 
rmaData <- read.csv("d:/R 실습/fat_rma.csv", 
                    header=TRUE, 
                    na.strings = "."
)
str(rmaData)
rmaData$time <- factor(rmaData$time,
                       levels=c(1:3),
                       labels=c("초기", "6주", "12주"))

attach(rmaData) # 객체연결 (attach  -> detach)

# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describeBy(per, time, mat=T)

# 03.그래프 그리기(박스그래프,히스토그램)
boxplot(per~time, data=rmaData, 
        ylab="percent", xlab="time", 
        col = c("Orange red", "Cornflower blue", "yellow"))

# 04.통계분석
# 구형성(sphericity)검정: Mauchly’s test. 
# 구형성검사에 통과해야 anova분석을 할 수 있음!

library(car)
rmaMatrix <- cbind(per[time=="초기"], #데이터 조정할 행렬형태로 만들기 위해
                   per[time=="6주"],  #구형성 테스트...
                   per[time=="12주"])
rmaModelLm <- lm(rmaMatrix ~ 1)
timeF <- factor(c("초기","6주","12주")) #시간
options(contrasts=c("contr.sum", "contr.poly")) #내장되어 있는 함수
rmaResultMt <- Anova(rmaModelLm, idata=data.frame(timeF), #Anova 대문자//구형성 가정의 테스트에서 쓰는 분산분석은 대문자로 표현함
                     idesign=~timeF, type="III") #구형성 검사
summary(rmaResultMt, multivariate=F)

# ANOVA 검정
rmaResult <- aov(per ~ time+Error(id/time), data=rmaData)
summary(rmaResult)

# 사후검정(Multicamparison test) - t-value 포함
#install.packages("multcomp")
library(multcomp)
resultLm <- lm(per ~ time)
TukeyResult <- glht(resultLm, linfct=mcp(time='Tukey'))
summary(TukeyResult)

# 통계결과 그래프
plot(TukeyResult)

detach(rmaData)

