### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris)

# 使用tail() 回傳iris的後六列
tail(iris)

# 使用str()
# 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)
for (i in 1:9){
  for (j in 1:9){
    print(paste(i, "*", j, "=", i*j))
  }
}


########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
nums = sample(10:100, 10)

# 查看nums
nums

# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。

for (i in 50:70){
  if (i%%2 == 0){
    print(paste("\"偶數且大於50\":", i))
    if(i == 66){
      print("太66666666666了")
      break
    }
  }
}
  
  
  
  



########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年

ans <- 0
readInput <- function(){
  n <- readline(prompt = "請輸入任一年份: ")
  ans <<- as.integer(n)
  return(ans)
}

while(1){
  readInput()
  if( (ans %% 4 == 0 && ans %% 100 != 0) || (ans %% 400 == 0) ){
    print("是閏年")
  }
  else{
    print("是平年")
  }
}

########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

counter <- 0
input_number_string <- ''
a_count <- 0
b_count <- 0

random_number <- sample(0:9, 4)

#for(a in random_number){
#  print(a)
#}

readInput <- function(){
  n <- readline(prompt = "請輸入猜測數字：")
  counter <<- counter + 1
  input_number <<- n
  return(input_number)
}

flag <- TRUE
while(flag){
  readInput()
  input_number_1 <- strtoi(substr(input_number, start = 1, stop = 1))
  input_number_2 <- strtoi(substr(input_number, start = 2, stop = 2))
  input_number_3 <- strtoi(substr(input_number, start = 3, stop = 3))
  input_number_4 <- strtoi(substr(input_number, start = 4, stop = 4))
  user_input_list <- c(input_number_1, input_number_2, input_number_3, input_number_4) 
  #first find how many a
  user_input_list
  
  for(a in 1:4){
    if(random_number[a] == user_input_list[a]){
      a_count <- a_count + 1
    }
    for(b in 1:4){
      if(a == b){
        next
      }
      if(user_input_list[a] == random_number[b]){
        b_count <- b_count + 1
      }
    }
  }
  print(paste(a_count, 'A, ', b_count, 'B'))
  if(a_count == 4 && b_count == 0){
    flag <<- FALSE 
  }
  a_count <<- 0
  b_count <<- 0
}
print(paste("你總共輸入了: ", counter, '次'))
