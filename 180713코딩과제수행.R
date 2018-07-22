getwd()
setwd("C:\Users\sweetrain\Documents\rlang")
getse()
setwd("C:\\Users\\sweetrain\\Documents\\rlang")
getsd()
getwd()

## 월요일 문제
# 체질량 지수(體質量指數, Body Mass Index, BMI)는 인간의 비만도를 나타내는 지수로, 
# 체중과 키의 관계로 계산된다.
# 키가 t 미터, 체중이 w 킬로그램일 때, BMI는 다음과 같다.
# (키의 단위가 센티미터가 아닌 미터임에 유의해야 한다.)
# 공식은 https://ko.wikipedia.org/wiki/%EC%B2%B4%EC%A7%88%EB%9F%89_%EC%A7%80%EC%88%98
# 가면 구할 수 있습니다.
# 이 BMI 지수를 구하는 Function 객체를 구하시오
# 이 문제는 스위치버전은 구할수 없고 if-else 버전만 가능합니다.

x <- 180 # 키
y <- 70 # 몸무게
bmi <- function(x,y){return ()}

res <- bmi(180, 70)
res
# '정상' 으로 나오도록 코드를 완성하시오

bmi<- function(x,y){
  z<- y/(x/100*x/100)
  res<-if(z>=35)"고도비만"else
    if(z>=30)"중등도 비만"else
      if(z>=25)"경도 비만"else
        if(z>=23)"과체중"else
          if(z>=18.5)"정상"else
            if(z<18.5)"저체중"
  return(cat("키:",x,"몸무게:",y,res))
}

bmi(180,70)

##  문자열에서 필요한 값 추출하기
# substr("문자열", 시작인덱스, 끝인덱스) 인덱스 시작은 1
# 아래 - 부분도 한자리로 인식함
stu <- "800101-1"
sub <- substr(stu, 8,8)
sub
# 실행하면 1이 찍힘
# 이것을 통해서 주민번호를 통한 성별 추출이 가능함
# 1, 3 남자  2, 4 여자 5, 6 은 외국인...그외는 잘못된 값
# 문제
# 위 문법을 이용해서 주민번호를 입력하면
# 남, 여, 외 이렇게 출력되는 객체(함수)를 완성하시오

stu<-"800101-1"
sub<-substr(stu, 8,8)
sub

whatsex<-function(x){
  stu<-x
  sub<-substr(stu,8,8)
  res<-if(sub==1)"남" else
    if(sub==2)"여"else
      if(sub==3)"남"else
        if(sub==4)"여"else
          if(sub==5)"외"else
            if(sub==6)"외"
  return(res)
}
whatsex("800101-1")

whatsex<-function(x){
  sex<-substr(x,8,8)
  res<-switch(
    sex,
    "1"="남",
    "2"="여",
    "3"="남",
    "4"="여",
    "5"="외",
    "6"="외"
  )
  return(res)
}
whatsex("920803-2")

whatsex("839102-5")

whatsex<-function(x){
  sex<-substr(x,8,8)
  y<-as.numeric(sex)
  res<-switch(
    y,
    "남",
    "여",
    "남",
    "여",
    "외",
    "외"
  )
  return(res)
}
whatsex("839102-2")

# sample(1:3,1,replace = TRUE) 하면 1부터 3까지 중에서 랜덤숫자 
# 하나를 반환합니다. 1:3 이 범위고, 1이 갯수, replace = TRUE 는 한번 뽑은
# 숫자를 다시 출력할지 여부인데 True를 주면 다시 뽑힐 수 있습니다. 즉 2
# 가 랜덤으로 나왔어도 다음 회차에서 다시 2가 나올 수 있습니다.
# 이를 이용해서 가위바위보 를 함수로 생성하세요.
# 예제는 아래와 같습니다.
# comVal <- sample(1:3,1,replace = TRUE) 
# myVal <- 2
# rps <- function(comVal, myVal)
# 이김 

sample(1:3,1,replace=TRUE)
?sample
sample(1:5,3,replace=FALSE)

rps<-function(comVal,myVal){
  res<-if(comVal==myVal)"비김"else
    if(comVal=="가위"&myVal=="바위")"이김"else
  if(comVal=="바위"&myVal=="보")"이김"else
    if(comVal=="보"&myVal=="가위")"이김"else
      "패배"
  return(res)
}

rps<-function(comVal,myVal){
  res<-if(comVal==myVal)"비김"else
    if(myVal==comVal+1)"이김"else
      if(myVal==1&comVal==3)"이김"else
        "패배"
  return(res)
}

kk<-sample(1:3,2,replace=TRUE)
comVal<-kk[1]
myVal<kk[2]
cat("comVal",comVal,"myVal",myVal,"결과:myVal",rps(comVal,myVal))

