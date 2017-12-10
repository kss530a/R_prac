#연습 데이터셋 확인하기
data()

#iris데이터 확인
head(iris)

#데이터구조 확인 
str(iris)

#기술통계량 확인
summary(iris)

#밀도추정 그래프
pal<-brewer.pal(9, "Pastel1")
hist(iris[["Sepal.Width"]], freq=F, 
     main="Seungsu's Iris Data", col=pal,
     xlab = "Sepal.Width")
lines(density(iris[[2]]), lwd=1, col="Red")

#plot을 위한 변수 지정 
shapes = c(21,22,23)[unclass(iris$Species)]
color = c("red", "green", "blue")
colors =c("red", "green", "blue")[unclass(iris$Species)]

#Box Plot 
boxplot(Sepal.Width~Species, data=iris, horizontal=T,
        col=color, xlab="Sepal.Width",
        main="Seungsu's Iris Data")

#Scatter plot
plot(iris$Sepal.Length, iris$Sepal.Width, 
     col=iris$Species, pch=shapes,
     xlab = "Sepal.Length", ylab = "Sepal.Width",
     bg=colors, main = "Seungsu's Iris Data")
line = lm(iris$Sepal.Width~iris$Sepal.Length, data = iris)
abline(line,col="black",lwd=2)

#Scatter plot matrices
pairs(iris[1:4], main = "Seungsu's Iris Data", 
      pch = shapes, bg = colors)

#Scatter plot 3d
library(scatterplot3d)
scatterplot3d(iris[,1:3], pch=shapes, bg=colors,
              main="Seungsu's Iris Data")

#Parallel coordinate plot
library(MASS)
parcoord(iris[1:4], col=colors)

#Google Map
library(ggmap)
library(ggplot2)

#우리집, 홍대T동 길찾기
locs = c("My house", "Hongik Univ T-dong")
adr = c('95-21 서울특별시 강서구 화곡동',
        '72-1 서울특별시 마포구 상수동')
#루트 만들기 - 대중교통
route_df <- route(adr[1], adr[2], structure = 'route', 
                  mode='transit', alternatives=TRUE)

gcode = geocode(adr)

locationInfo <- data.frame(
  Name = locs, 
  lon = gcode$lon,
  lat = gcode$lat
)
#지도 센터 구하기
center <-c(mean(locationInfo[,2]),
           mean(locationInfo[,3]))
#지도에 그리기 
getmap <- get_googlemap(center=center, zoom=13, marker=gcode)
mymap = ggmap(getmap)
mymap + 
  geom_text(data=locationInfo, aes(x=lon, y=lat), 
            size=3, label=locationInfo$Name) +
  geom_path(data=route_df, aes(x=lon, y=lat),
            color="red", lineend = 'round')

#word cloud 만들기
library("rJava")
library("KoNLP")
library("wordcloud")
library("RColorBrewer")

f <- file("/Users/InsightBridge/R-project/kim.txt", blocking=F)
text <- readLines(f)

nouns <- sapply(text, extractNoun, USE.NAMES = F)
term2 <- unlist(nouns)

termCount <- table(term2)
mypal<-brewer.pal(9, "Set1")

data= Filter(function(x){nchar(x)>=2}, term2)
termCount2 <- table(data)

wordcloud(names(termCount2), freq = termCount2, scale=c(6,0.3),
          min.freq = 3, random.order = F, rot.per = 0.1,
          colors = mypal, family="AppleGothic")


