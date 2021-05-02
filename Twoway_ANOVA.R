#### 이원 분산분석(Two-way ANOVA) #####

# 01.데이터 불러오기
twaData <- read.csv("d:/R 실습/06.TWA.csv",
                    header=TRUE,
                    na.strings = "."
)
twaData$time <- factor(twaData$time,
                       levels=c(1:2),
                       labels=c("10분","20분"))
twaData$temp <- factor(twaData$temp,
                       levels=c(1:2),
                       labels=c("1,000도","1,500도"))

str(twaData)

attach(twaData) # 객체연결 (attach  -> detach)

# 02.기본통계치 확인: describe(psych패키지 이용)
library(psych)
describeBy(taste, time:temp, mat=T) #두개의 변수의 상관관계를를 구하려면 : 을 사용


# 03.그래프 그리기(박스그래프,히스토그램)
plot(taste ~ time + temp, data=twaData)
boxplot(taste ~ time*temp, data=twaData)

library(ggplot2)
ggplot(twaData, aes(x = temp, y = taste, fill = temp)) + #축에 관한 설정
    geom_boxplot(outlier.colour="red") +
    facet_wrap(~time) + #그룹으로 나눌때 사용. 시간을 그룹으로 지정해준것.
    ggtitle("시간*온도에 따른 맛")

# 04.통계분석
twaResult <- aov(taste ~ time + temp + time:temp, data=twaData)

summary(twaResult)

#상호작용효과 그래프
interaction.plot(time, temp, taste) #순서대로 x축, y축 값 입력하고, 수치형 데이터를 맨 마지막에 넣음.
#X축과 Y축에 뭘 넣느냐에 따라 해석이 다름 -> x,y둘을 바꿔보고 내가 편한 걸로 적용!


# 사후검정(Multicamparison test )
# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
# 상호작용이 없을 경우 : 각 변수별 주효과 분석(t-test, ANOVA분석)

# 상호작용이 있을 경우 : 그룹별로 나누어서 분석
tw10 <- twaData[twaData$time=="10분",]
tw20 <- twaData[twaData$time=="20분",]

t.test(taste ~ temp,
       data=tw10,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)
t.test(taste ~ temp,
       data=tw20,
       alternative = c("two.sided"),
       var.equal = TRUE,
       conf.level = 0.95)

detach(twaData)

# 부록 : 일반 분석 TukeyHSD 결과 해석 -> 튜키가 코딩은 쉬운데 그래프 해석이 어려워서 웬만하면 t-test를 사용해!! 대신 두가지 할 줄 알아야돼!!

TukeyHSD(twaResult)
tukeyPlot <- TukeyHSD(twaResult) # 그룹간 차이 비교
plot(tukeyPlot)

