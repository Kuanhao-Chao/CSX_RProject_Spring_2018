library(tidyverse)
library(magrittr)

# laod in data
titanicTrain = read_csv("project2/titanicTrain.csv")
titanicTrain %<>% filter(!is.na(survived))

# is.boat is powerful!
titanicTrain %>% mutate(is.boat = !is.na(boat)) %>% 
  group_by(is.boat,survived) %>% summarise(m=n()) %>% 
  ggplot(mapping = aes(interaction(is.boat,survived),m)) + geom_col()
  
titanicTrain %>% 
  mutate(is.boat = !is.na(boat)) %>% 
  filter(is.boat != as.numeric(survived)) %>% View

# sex * pclass is powerful too 
titanicTrain %>% transform(survived = as.factor(survived)) %>% 
  group_by(pclass,sex,survived) %>% 
  ggplot(mapping = aes(interaction(pclass,sex),fill = survived)) + geom_bar()

titanicTrain %>% 
  group_by(pclass,sex) %>% summarise(m = sum(survived)/length(survived)) %>% 
  ggplot(mapping = aes(interaction(pclass,sex),m)) + geom_col()

# generate first name column
titanicTrain$name.first = 
  strsplit(titanicTrain$name,",") %>% 
  do.call(rbind,.) %>% .[,2] %>% 
  strsplit(" ") %>% do.call(rbind,.) %>% .[,3] 

titanicTrain %>% group_by(name.first) %>% summarise(m=n()) %>% 
  arrange(desc(m)) %>% head(10) %>% 
  ggplot(mapping = aes(name.first,m)) + geom_col()

# considering ticket
ticket.group = titanicTrain %>% group_by(ticket) %>% summarise(ticket.group=n())
titanicTrain %<>% left_join(ticket.group)

# considering first name
name.group = titanicTrain %>% group_by(name.first) %>% summarise(name.group=n())
titanicTrain %<>% left_join(name.group)

# considering 
home.group = titanicTrain %>% filter((ticket.group != 1)&(ticket.group != name.group)) %>% 
  group_by(ticket,home.dest) %>% summarise(home.group=n())
titanicTrain %<>% left_join(home.group)

# generate ngroup
titanicTrain$index = 1:nrow(titanicTrain)
df1 = titanicTrain %>% filter(ticket.group == name.group) %>% 
  mutate(ngroup = ticket.group) %>% select(index,ngroup)

df2 = titanicTrain %>% filter(ticket.group == home.group) %>% 
  mutate(ngroup = ticket.group) %>% select(index,ngroup)

df3 = titanicTrain %>% filter((ticket.group != home.group)&(name.group == home.group)) %>% 
  mutate(ngroup = name.group) %>% select(index,ngroup)

df4 = df1 %>% full_join(df2) %>% full_join(df3)
 
titanicTrain %<>% left_join(df4)
titanicTrain %>% View

# test ngroup
titanicTrain %>% filter(!is.na(ngroup)) %>% 
  group_by(ngroup,survived) %>% summarise(m=n()) %>% 
  ggplot(mapping = aes(ngroup,m,fill = survived)) + geom_col()

titanicTrain %>% filter(!is.na(ngroup)) %>% 
  group_by(ngroup) %>% summarise(m=sum(survived)/length(ngroup)) %>% 
  ggplot(mapping = aes(ngroup,m)) + geom_col()

titanicTrain %>% View 
