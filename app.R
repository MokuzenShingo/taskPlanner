##############################################################
#  Init
##############################################################
library(shiny)
library(shinydashboard)
library(dplyr)
library(visNetwork)
library(rhandsontable)
library(DT)
library(xts)
library(rpart)
library(plotly)
library(stringr)
##############################################################
#  UI
##############################################################
## Header content
header <- dashboardHeader(title = "taskPlanner")

## Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Submit", tabName = "submit", icon = icon("pencil")),
    menuItem("Advisor", tabName = "advisor", icon = icon("user")),
    menuItem("Tasks", tabName = "tasks", icon = icon("th-list")),
    menuItem("Utility", tabName = "utility", icon = icon("wrench")),
    menuItem("How to", tabName = "howto", icon = icon("file")),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/rstudio/shinydashboard/")
  )
)

## Body content
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              valueBoxOutput("dashboard_taskPoint"),
              valueBoxOutput("dashboard_taskDiges"),
              valueBoxOutput("dashboard_taskTimes")),
            visNetworkOutput("dashboard_network")),
    tabItem(tabName = "submit",
            visNetworkOutput("submit_network"),
            fluidRow(
              column(width = 6, rHandsontableOutput("submit_nodeTable1")),
              column(width = 6, rHandsontableOutput("submit_nodeTable2"))),
            fluidRow(
              column(width = 4, dateInput("submit_date", label = "", value = Sys.Date()))),
            fluidRow(
              column(width = 2, numericInput("submit_id", label = "ID", value = NA)),
              column(width = 2, numericInput("submit_actualTime", label = "実時間", value = NA)),
              column(width = 8, textInput("submit_comment",label = "コメント", value = "", width = "100%"))),
            fluidRow(
              column(width = 2, actionButton("submit_save",label = "保存")))),
    tabItem(tabName = "advisor",
            fluidRow(
              column(width = 12, uiOutput("advisor_dates"))),
            fluidRow(
              box(width = 12,
                  column(width = 4, plotlyOutput("advisor_pointPA")),
                  column(width = 4, plotlyOutput("advisor_tasksPA")),
                  column(width = 4, plotlyOutput("advisor_timesPA")))),
            fluidRow(
              box(width = 12,
                  h3("消化タスクの傾向"),
                  visNetworkOutput("advisor_network_task"))),
            fluidRow(
              box(width = 12,
                  h3("タスク時間の予実傾向"),
                  p("スコア = 実時間 / 予定時間"),
                  visNetworkOutput("advisor_network_time")))),
    tabItem(tabName = "tasks",
            dataTableOutput("task_table")),
    tabItem(tabName = "utility",
            fluidRow(
              column(width = 12, 
                     textInput("utility_group1",label = "グループ1", value = "", width = "50%"),
                     textInput("utility_group2",label = "グループ2", value = "", width = "50%"),
                     actionButton("utility_save_group",　label = "グループ設定の保存"))),
            hr(),
            fluidRow(
              column(width = 4,
                     actionButton("utility_table_init",　label = "タスクテーブルの初期化"),
                     hr(),
                     actionButton("utility_save_csv",　label = "タスクテーブルをCSVファイルで保存")))),
    tabItem(tabName = "howto",
            includeMarkdown("howto.md"))
  )
)
##############################################################
#  Server , session
##############################################################
server <- function(input, output, session) {
  load("./data/taskTable.RData")
  load("./data/groupList.RData")
  
  value <- reactiveValues()
  value1 <- reactiveValues()
  value2 <- reactiveValues()
  
  observe({
    value$date <- as.character(input$submit_date)
    taskTable_day <- taskTable[taskTable$date == value$date, ]
    if(nrow(taskTable_day) <= 1){
      taskTable_day <-taskTable[1:16,]
      taskTable_day[,] <- NA
      taskTable_day$date <- rep(value$date, 16)
      taskTable_day$ID  <- 1:16
      taskTable_day$group1 <- factor(rep(groupList$group1[[1]][1],16), levels = groupList$group1[[1]])
      taskTable_day$group2 <- factor(rep(groupList$group2[[1]][1],16), levels = groupList$group2[[1]])
      taskTable_day$name <- paste0(1:16, ":")
      taskTable_day$importance <- rep(3, 16)
      taskTable_day$urgency <- rep(3, 16)
    }
    
    newRows <- 16 - nrow(taskTable_day)
    if(newRows != 0){
      new_taskTable_day <- taskTable_day[1:newRows,]
      new_taskTable_day[,] <- NA
      taskTable_day <- rbind(taskTable_day, new_taskTable_day)
    }
    value$taskTable_day <- taskTable_day 
    
    value$viewTable <- taskTable_day[,c("ID", "to", "name","group1","group2","importance","urgency","planTime")]
    
    value1$data <- value$viewTable[1:8,]
    value2$data <- value$viewTable[9:16,]
  })
  
  observe({ if(!is.null(input$submit_nodeTable1)) value1$data <- hot_to_r(input$submit_nodeTable1) })
  observe({ if(!is.null(input$submit_nodeTable2)) value2$data <- hot_to_r(input$submit_nodeTable2) })
  
  output$submit_nodeTable1 <- renderRHandsontable({ rhandsontable(value1$data, rowHeaders = FALSE) })
  output$submit_nodeTable2 <- renderRHandsontable({ rhandsontable(value2$data, rowHeaders = FALSE) })
  
  output$dashboard_network <- renderVisNetwork({
    dat.node <- rbind(value1$data, value2$data)
    dat.node <- dat.node[which(!is.na(dat.node$ID)),]
    dat.node$id <- dat.node$ID
    dat.node$label <- dat.node$name
    taskTable_day_temp <-  value$taskTable_day[!is.na(value$taskTable_day$ID),]
    dat.node$color <- ifelse(is.na(taskTable_day_temp$actualTime),"#A0BBCE",
                             ifelse(taskTable_day_temp$actualTime == 0, "lightgray", "#64A8D7")
    )
    dat.edge <- data.frame(from = dat.node$id, to = dat.node$to)
    
    visNetwork(dat.node, dat.edge, width = "100%") %>%
      visEdges(arrows = "to", smooth = FALSE) %>% 
      visHierarchicalLayout(direction = "LR", levelSeparation = 500, treeSpacing = 0) %>%
      visNodes(borderWidth = 0, shape = "box", font = list(size = 20))
  })
  
  output$dashboard_taskPoint <- renderValueBox({
    dat <- value$taskTable_day[which(!is.na(value$taskTable_day$ID)),]
    dat.comp <- dat[which(dat$actualTime > 0),]
    
    taskPoint_sum <- sum(apply(dat, 1, function(x) as.integer(x["importance"]) * as.integer(x["urgency"])))
    taskPoint_comp <- sum(apply(dat.comp, 1, function(x) as.integer(x["importance"]) * as.integer(x["urgency"])))
    
    valueBox(paste0(round(100 * taskPoint_comp / taskPoint_sum,0), " %"), 
             paste0("タスクポイント (", taskPoint_comp, " / ", taskPoint_sum, ")"), 
             icon = icon("signal"), color = "light-blue")
  })
  output$dashboard_taskDiges <- renderValueBox({
    dat <- value$taskTable_day[which(!is.na(value$taskTable_day$ID)),]
    dat.comp <- dat[which(dat$actualTime > 0),]
    
    valueBox(paste0(round(100 * nrow(dat.comp) / nrow(dat) - sum(dat$actualTime == 0, na.rm = T),0), " %"), 
             paste0("タスク消化率 (", nrow(dat.comp), " / ", nrow(dat) - sum(dat$actualTime == 0, na.rm = T), ")"),
             icon = icon("th-list"), color = "light-blue")
  })
  output$dashboard_taskTimes <- renderValueBox({
    dat <- value$taskTable_day[which(!is.na(value$taskTable_day$ID)),]
    
    actualTime_sum <- sum(as.numeric(dat$actualTime), na.rm = T)
    planTime_sum <- sum(as.numeric(dat$planTime), na.rm = T)
    
    valueBox(paste0(round(100 *  actualTime_sum / planTime_sum, 0), " %"), 
             paste0("タスク時間 (", actualTime_sum, " / ", planTime_sum, ")"),
             icon = icon("tachometer"), color = "light-blue")
  })
  
  output$submit_network <- renderVisNetwork({
    dat.node <- rbind(value1$data, value2$data)
    dat.node <- dat.node[which(!is.na(dat.node$ID)),]
    dat.node$id <- dat.node$ID
    dat.node$label <- dat.node$name
    dat.edge <- data.frame(from = dat.node$id, to = dat.node$to)
    
    visNetwork(dat.node, dat.edge, width = "100%") %>%
      visEdges(arrows = "to", smooth = FALSE) %>% 
      visHierarchicalLayout(direction = "LR", levelSeparation = 500, treeSpacing = 0) %>%
      visNodes(borderWidth = 0, shape = "box", font = list(size = 20))
  })
  observe({
    if(input$submit_save == 0) return()
    input$submit_save
    dat <- rbind(value1$data, value2$data)
    saveTable <- cbind(date = value$taskTable_day$date,
                       dat, 
                       actualTime = value$taskTable_day$actualTime,
                       comment = value$taskTable_day$comment)
    saveTable <- saveTable[which(!is.na(saveTable$date)), ]
    saveTable <- saveTable[which(!is.na(saveTable$ID)), ]
    saveTable$comment <- as.character(saveTable$comment)
    if(!is.na(input$submit_id)) {
      saveTable[saveTable$ID == input$submit_id, "comment"] <- input$submit_comment
      saveTable[saveTable$ID == input$submit_id, "actualTime"] <- input$submit_actualTime  
    }
    
    taskTable <<- rbind(taskTable[taskTable$date != value$date, ], saveTable)
    save(taskTable, file = "./data/taskTable.RData")
    taskTable_day <- taskTable[taskTable$date == value$date, ]
    newRows <- 16 - nrow(taskTable_day)
    if(newRows != 0){
      new_taskTable_day <- taskTable_day[1:newRows,]
      new_taskTable_day[,] <- NA
      taskTable_day <- rbind(taskTable_day, new_taskTable_day)
    }
    value$taskTable_day <- taskTable_day 
    
    value$viewTable <- taskTable_day[,c("ID", "to", "name","group1","group2","importance","urgency","planTime")]
    
    value1$data <- value$viewTable[1:8,]
    value2$data <- value$viewTable[9:16,]
  })
  
  output$advisor_dates <- renderUI({
    load(file = "./data/taskTable.RData")
    taskTable.xts <- as.xts(taskTable, order.by = as.Date(taskTable$date))
    dateRangeInput("advisor_dates", label = "分析対象",
                   start = index(taskTable.xts)[1],
                   end = Sys.Date())
  })
  output$advisor_pointPA <- renderPlotly({
    input$submit_save
    load(file = "./data/taskTable.RData")
    taskTable.xts <- as.xts(taskTable, order.by = as.Date(taskTable$date))
    taskTable_sub <- taskTable.xts[paste0(input$advisor_dates[1], "::", input$advisor_dates[2])]
    taskTable_sub <- fix.class(data.frame(coredata(taskTable_sub), stringsAsFactors = F))
    
    taskTable.daily <- daily.summary(taskTable_sub)
    plot_ly(taskTable.daily, x = ~date, y = ~actuPoint, name = "actual", type = "scatter", mode = "lines") %>%
      add_trace(y = ~planPoint, name = "plan") %>%
      layout(xaxis = list(type = "date", title = ""),
             yaxis = list(title = "タスクポイント"),
             legend = list(orientation = "h", yanchor = "top")) 
  })
  output$advisor_tasksPA <- renderPlotly({
    input$submit_save
    load(file = "./data/taskTable.RData")
    taskTable.xts <- as.xts(taskTable, order.by = as.Date(taskTable$date))
    taskTable_sub <- taskTable.xts[paste0(input$advisor_dates[1], "::", input$advisor_dates[2])]
    taskTable_sub <- fix.class(data.frame(coredata(taskTable_sub), stringsAsFactors = F))
    
    taskTable.daily <- daily.summary(taskTable_sub)
    plot_ly(taskTable.daily, x = ~date, y = ~taskComp, name = "actual", type = "scatter", mode = "lines") %>%
      add_trace(y = ~tasks, name = "plan")  %>%
      layout(xaxis = list(type = "date", title = ""),
             yaxis = list(title = "タスク数"),
             legend = list(orientation = "h", yanchor = "top"))
  })
  output$advisor_timesPA <- renderPlotly({
    input$submit_save
    load(file = "./data/taskTable.RData")
    taskTable.xts <- as.xts(taskTable, order.by = as.Date(taskTable$date))
    taskTable_sub <- taskTable.xts[paste0(input$advisor_dates[1], "::", input$advisor_dates[2])]
    taskTable_sub <- fix.class(data.frame(coredata(taskTable_sub), stringsAsFactors = F))
    
    taskTable.daily <- daily.summary(taskTable_sub)
    plot_ly(taskTable.daily, x = ~date, y = ~actualTime, name = "actual", type = "scatter", mode = "lines") %>%
      layout(xaxis = list(type = "date", title = ""),
             yaxis = list(title = "タスク時間"),
             legend = list(orientation = "h", yanchor = "top"))  %>%
      add_trace(y = ~planTime, name = "plan") 
    
  })
  fix.class <- function(taskTable){
    taskTable$date <- as.character(taskTable$date)
    taskTable$ID <- as.integer(taskTable$ID)
    taskTable$group1 <- as.character(taskTable$group1)
    taskTable$group2 <- as.character(taskTable$group2)
    taskTable$name <- as.character(taskTable$name)
    taskTable$importance <- as.integer(taskTable$importance)
    taskTable$urgency <- as.integer(taskTable$urgency)
    taskTable$planTime <- as.numeric(taskTable$planTime)
    taskTable$actualTime <- as.numeric(taskTable$actualTime)
    taskTable
  }
  daily.summary <- function(taskTable){
    taskTable$taskComp <- ifelse(is.na(taskTable$actualTime), 0, 1)
    taskTable$planPoint <- taskTable$importance * taskTable$urgency
    taskTable$actuPoint <- taskTable$importance * taskTable$urgency * taskTable$taskComp
    
    taskTable.daily <- group_by(taskTable, date)
    dplyr::summarise(taskTable.daily,
                     planTime = sum(planTime),
                     actualTime = sum(actualTime, na.rm = T),
                     time.ratio = 100 * sum(actualTime, na.rm = T) / sum(planTime),
                     tasks = length(ID),
                     taskComp = sum(taskComp),
                     taskComp.ratio = 100 * sum(taskComp) / length(ID),
                     planPoint = sum(planPoint),
                     actuPoint = sum(actuPoint),
                     point.ratio = 100 * sum(actuPoint) / sum(planPoint)
    )
  }
  output$advisor_network_task <- renderVisNetwork({
    input$submit_save
    load(file = "./data/taskTable.RData")
    taskTable.xts <- as.xts(taskTable, order.by = as.Date(taskTable$date))
    taskTable_sub <- taskTable.xts[paste0(input$advisor_dates[1], "::", input$advisor_dates[2])]
    taskTable_sub <- fix.class(data.frame(coredata(taskTable_sub), stringsAsFactors = F))
    
    taskTable_sub$taskComp <- as.factor(ifelse(is.na(taskTable_sub$actualTime), "消化できず", "消化"))
    
    taskTable.daily <- daily.summary(taskTable_sub)
    taskTable_temp <- full_join(taskTable_sub, taskTable.daily[,c("date", "tasks")], key = date)
    
    res <- rpart(taskComp ~ group1 + group2 + importance + urgency + planTime + tasks, data = taskTable_temp)
    visTree(res,  width = "100%", direction = "LR")
  })
  output$advisor_network_time <- renderVisNetwork({
    input$submit_save
    load(file = "./data/taskTable.RData")
    taskTable.xts <- as.xts(taskTable, order.by = as.Date(taskTable$date))
    taskTable_sub <- taskTable.xts[paste0(input$advisor_dates[1], "::", input$advisor_dates[2])]
    taskTable_sub <- fix.class(data.frame(coredata(taskTable_sub), stringsAsFactors = F))
    
    taskTable_sub <- taskTable_sub[!is.na(taskTable_sub$actualTime),] 
    taskTable_sub <- taskTable_sub[which(taskTable_sub$actualTime != 0),] 
    taskTable_sub$timePA <- taskTable_sub$actualTime / taskTable_sub$planTime 
    
    taskTable.daily <- daily.summary(taskTable_sub)
    taskTable_temp <- full_join(taskTable_sub, taskTable.daily[,c("date", "tasks")], key = date)
    
    res <- rpart(timePA ~ group1 + group2 + importance + urgency + planTime + tasks, data = taskTable_temp)
    visTree(res,  width = "100%", direction = "LR")
  })
  
  output$task_table <- renderDataTable({
    input$submit_save
    load(file = "./data/taskTable.RData")
    datatable(taskTable,  rownames = FALSE, filter = "top", extensions = 'ColReorder',
              options = list(autoWidth=TRUE,dom = 'Rlfrtip'))
  })
  
  observe({
    if(input$utility_table_init == 0) return()
    input$utility_table_init
    load("./data/groupList.RData")
    columnLabel <- c("date" , "ID", "to", "group1", "group2", "name", "importance", "urgency", "planTime", "actualTime","comment")
    taskTable <- data.frame(matrix(rep(NA, length(columnLabel) * 16), nrow=16))
    colnames(taskTable) <- columnLabel
    
    taskTable$date <- as.character(taskTable$date)
    taskTable$ID <- as.integer(taskTable$ID)
    taskTable$to <- as.integer(taskTable$to)
    taskTable$group1 <- as.factor(taskTable$group1)
    taskTable$group2 <- as.factor(taskTable$group2)
    taskTable$name <- as.character(taskTable$name)
    taskTable$importance <- as.numeric(taskTable$importance)
    taskTable$urgency <- as.numeric(taskTable$urgency)
    taskTable$planTime <- as.numeric(taskTable$planTime)
    taskTable$actualTime <- as.numeric(taskTable$actualTime)
    taskTable$comment <- as.character(taskTable$comment)
    
    taskTable$date <- rep(Sys.Date(), 16)
    taskTable$ID  <- 1:16
    taskTable$group1 <- factor(rep(groupList$group1[[1]][1],16), levels = groupList$group1[[1]])
    taskTable$group2 <- factor(rep(groupList$group2[[1]][1],16), levels = groupList$group2[[1]])
    taskTable$name <- paste0(1:16, ":")
    taskTable$importance <- rep(3, 16)
    taskTable$urgency <- rep(3, 16)
    
    taskTable <<- taskTable
    save(taskTable, file = "./data/taskTable.RData")
  })
  observe({
    if(input$utility_save_csv == 0) return()
    input$utility_save_csv
    load("./data/taskTable.RData")
    write.csv(taskTable, "./data/taskTable.csv", row.names = F, fileEncoding = "SJIS")
  })
  observe({
    if(input$utility_save_group == 0) return()
    input$utility_save_group
    
    groupList <- list(group1 = str_split(isolate(input$utility_group1), pattern = ","),
                      group2 = str_split(isolate(input$utility_group2), pattern = ","))
    save(groupList, file = "./data/groupList.RData")
  })
}
##############################################################
#  Bild app
##############################################################
ui <- dashboardPage(header, sidebar, body)
shinyApp(ui, server)