###
## init table
load("./data/groupList.RData")
tasks <- sample(5:10, 20, replace = T)
tasks.length <- length(unlist(lapply(tasks, function(x) 1:x)))

## sample
taskTable <- data.frame(
  date = paste0("2017-11-", rep(c(11:30), tasks)),
  ID   = unlist(lapply(tasks, function(x) 1:x)),
  to   = rep(1, tasks.length),
  group1 = sample(groupList$group1[[1]], replace = T, tasks.length),
  group2 = sample(groupList$group2[[1]], replace = T, tasks.length),
  name = paste0(sample(c("case", "info", "uncategorized"), replace = T, tasks.length),"_name"),
  importance = sample(c(1:5), replace = T, tasks.length),
  urgency = sample(c(1:5), replace = T, tasks.length),
  planTime = sample(c(0.3, 0.5, 1, 1.5), replace = T, tasks.length),
  actualTime = sample(c(0, 0.5, 1, 1.5, 2, NA, NA,NA), replace = T, tasks.length),
  comment = paste0(sample(c("case", "info", "uncategorized"), replace = T, tasks.length),"_comment"),
  stringsAsFactors = F
)

taskTable$to <- as.integer(taskTable$to)
taskTable$group1 <- as.factor(taskTable$group1)
taskTable$group2 <- as.factor(taskTable$group2)

save(taskTable, file = "./data/taskTable.RData")
