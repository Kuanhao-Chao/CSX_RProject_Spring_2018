require(arules)
require(arulesViz)
library(magrittr)

data <- read.csv("haberman.csv")

data$X30 %<>% lapply(function(i) ifelse(i >= 60,T,F)) %>% unlist %>% as.factor()
data$X1 %<>% cut(.,
                 c(0,1,3,max(.)),
                 include.lowest = T,
                 labels = c("None","1-3","4~")) 
data$X1.1 %<>% lapply(function(i) ifelse(i == 1,T,F)) %>% unlist %>% as.factor()
data$X64 %<>%  lapply(function(i) ifelse(i <= 60,T,F)) %>% unlist %>% as.factor()

colnames(data) <- c("is.old", "treated_after_1960", "n_axillary_nodes", "outlive_5y")
View(data)

rule <- apriori(data, 
  # min support & confidence, 最小規則長度(lhs+rhs)
  parameter=list(minlen=2, supp=0.01, conf=0.85),
  appearance = list(default="lhs",
                    rhs=c("outlive_5y=TRUE","outlive_5y=FALSE")
                    # 右手邊顯示的特徵
  )
)  

inspect(rule)
sort.rule = sort(rule, by="lift")
inspect(sort.rule)

simplify = function(sort.rule){
  subset.matrix <- as.matrix(is.subset(x=sort.rule, y=sort.rule))
  subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
  redundant <- colSums(subset.matrix, na.rm=T) >= 1
  sort.rule <- sort.rule[!redundant]
  return(sort.rule)
}

simplify(sort.rule)
inspect(sort.rule)

require(arulesViz)
plot(sort.rule)
plot(sort.rule, method="graph", control=list(type="items"))
plot(sort.rule, method="grouped")
