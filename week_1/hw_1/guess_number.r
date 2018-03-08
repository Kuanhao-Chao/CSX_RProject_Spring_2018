counter <- 0
input_number_string <- ''
a_count <- 0
b_count <- 0

random_number <- sample(0:9, 4)

#for(a in random_number){
#  print(a)
#}

readInput <- function(){
  cat("請輸入猜測數字：");
  n <- readLines('stdin', n=1);
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
  cat(paste(a_count, 'A, ', b_count, 'B'))
  cat('\n')
  if(a_count == 4 && b_count == 0){
    flag <<- FALSE 
  }
  a_count <<- 0
  b_count <<- 0
}
print(paste("你總共輸入了: ", counter, '次'))
