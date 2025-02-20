#overdue clients----
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#library----
library(shiny)
library(ggplot2)
library(shiny)
library(readxl) 
library(vroom)
library(tidyverse)
library(bslib)
library(lubridate)

# rsconnect::setAccountInfo(name='twd1r5-shevril',
#                           token='C8006E83F233A8E6FAAD065C24B88691',
#                           secret='SiQQj0138tVDhnMF0+qc6k7AnjIse87cSd2sFaI3')

#ui----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone", version = 5), #https://bootswatch.com/ choose preference theme
  titlePanel("易鑫华东六类客户 & 逾期客户"),
  tabsetPanel(
    tabPanel("导入数据Import data", 
             fileInput("HistoryRawdata", "历史订单明细脱敏版.csv",buttonLabel = "Upload...", multiple = FALSE ),
             fileInput("SalesConsultantList", "顾问花名册.xlsx",buttonLabel = "Upload...", multiple = FALSE ),
             fileInput("ChannelsList", "渠道管理列表.xlsx",buttonLabel = "Upload...", multiple = FALSE )
    ),
    
    tabPanel("设变量Set parameters",
             dateInput("date_one_buttom_direction_pass", "一键直秒日期:", value = "2024-07-01"),
             dateInput("stadate", "生效开始日期:", value = Sys.Date()-3),
             dateInput("enddate", "生效结束日期:", value = Sys.Date()-1),
             textInput("Managers", "补足电话的经理"),
             sliderInput("AmountOfPhonecall", "补足电话数",value = 4, min = 1, max = 10, step = 1, width = "340px"),
             hr(),
             dateInput("Overduedate", "逾期-生效开始日期:", value = "2023/11/01"),
             numericInput("OverdueDays","逾期天数", value = NULL, step = 1, width = 100),
             
             downloadButton("download","下载数据")
    ),
    
    tabPanel("展示数据Visualise results",
             textOutput("manager"),
             textOutput("One_butten_direction_pass"),
             dataTableOutput("preview")
             
    ),
    tabPanel("逾期客户Overdue Clients",
             downloadButton("Overduedownload","下载逾期数据"),
             dataTableOutput("Overdue_Clients_By_Managers"),
             dataTableOutput("overdue_clients")
    ),
    tabPanel("清洗数据Tidy Data",
             textOutput("tidied_data_comments"),
             downloadButton("Tidied_data_download","下载清洗后数据"),
             
             dataTableOutput("tidied_data")
             )
    
  )
)





# ui <- fluidPage(
#   titlePanel("易鑫华东六类客户统计"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       dateInput("date_one_buttom_direction_pass", "一键直过日期:", value = "2024-07-01"),
#       dateInput("stadate", "开始日期:", value = Sys.Date()-3),
#       dateInput("enddate", "结束日期:", value = Sys.Date()-1),
#       textInput("Managers", "补足电话的经理"),
#       sliderInput("AmountOfPhonecall", "补足电话数",value = 4, min = 1, max = 10, step = 1, width = "340px"),
#       fileInput("HistoryRawdata", "历史订单明细脱敏版.csv",buttonLabel = "Upload...", multiple = FALSE ),
#       fileInput("SalesConsultantList", "顾问花名册.xlsx",buttonLabel = "Upload...", multiple = FALSE ),
#       fileInput("ChannelsList", "渠道管理列表.xlsx",buttonLabel = "Upload...", multiple = FALSE )
#     ),
#     mainPanel(
#       textOutput("manager"),
#       dataTableOutput("preview")
#     )
#   ),
#   downloadButton("download","下载数据"),
# )

#设定上传文件容量。----

options(shiny.maxRequestSize = 50 * 1024^2) 


# 自定义函数 ----

fn_drop_parentheses <- function(clean_parentheses_a)
{
  clean_parentheses_a <- gsub("\\(.*\\)", "", clean_parentheses_a)
  clean_parentheses_a <- gsub("\\（.*\\）", "", clean_parentheses_a)
  return(clean_parentheses_a)
}


fn_format_ymd <- function(date_a) {
  date_a <- as.Date(date_a, format = "%Y-%m-%d")
  return(date_a)
}

# Server----

server <- function(input, output, session) {
  #server data manipulation----
  
  # 读取数据----
  
  #读取rawdata
  rawdata <- reactive({
    req(input$HistoryRawdata)
    
    read.csv(input$HistoryRawdata$datapath) %>%
      mutate(
        进件日期 = ymd(进件日期),
        #合同生效日期 = as.Date(合同生效日期, format = "%Y-%m-%d"),
        合同生效日期 = ymd(合同生效日期),
        金融经理名称 = fn_drop_parentheses(金融经理名称),
        金融顾问名称 = fn_drop_parentheses(金融顾问名称)
      ) %>%
      arrange(desc(合同生效日期))
  })
  
  
  #读取顾问表
  华东全员数据 <- reactive({
    req(input$SalesConsultantList)
    
    read_xlsx(input$SalesConsultantList$datapath)
  })
  
  
  #读取渠道表
  合作渠道 <- reactive({
    req(input$ChannelsList)
    
    read_xlsx(input$ChannelsList$datapath) %>% 
      mutate(
        初始生效日期 = fn_format_ymd(初始生效日期),
        合作开始日期 = fn_format_ymd(合作开始日期)
      )
  })
  
  #Function1,六类客户条件筛选----
  #1
  入职2月内新顾问 <- reactive({
    华东全员数据() %>%
      mutate( 入职日期 = as.Date(入职日期,  "%Y-%m-%d")) %>%
      filter(
        入职日期 >= Sys.Date()-60)
  })
  
  入职2月内新顾问名字 <- reactive({
    入职2月内新顾问()$姓名
  })
  
  
  #2
  一年内逾期大于2单及以上渠道list <- reactive({
    合作渠道() %>%
      filter(近1年逾期量 >= 2)
  })
  
  一年内逾期大于2单及以上渠道 <- reactive({
    #c("宁德市鼎旺贸易有限公司","莆田恺顺汽车销售服务有限公司")测试
    一年内逾期大于2单及以上渠道list()$店面名称
  }) 
  
  #3
  一年内逾期大于2单及以上顾问 <- reactive({
    rawdata() %>%
      filter(!is.na(当前逾期级别),
             最新逾期天数 >= 30, # 逾期天数大于30天
             合同生效日期 >= today()-365) %>%  #近一年内的成交
      group_by(金融顾问名称) %>%
      summarise(逾期单数 = n()) %>%
      filter(逾期单数 >= 2 & #逾期单数 >= 2
               金融顾问名称 != "陈伟彬" ) %>% 
      pull(金融顾问名称) #选出顾问名字
  })
  
  #一年内逾期大于2单及以上顾问 <- unique(一年内逾期大于2单及以上顾问list$金融顾问名称)
  
  
  #remove(福州入职2月内新顾问, 一年内逾期大于2单及以上渠道list)
  
  #核验六类型客户_异地订单----
  
  核验六类型客户_异地订单 <- reactive({
    rawdata() %>%
      mutate(是否为异地客户 = if_else(现居住地省份.企业所在省份 != 单位所在省份,"是","否")) %>% 
      mutate(订单编号 = case_when(合同生效日期>= input$stadate & 
                                渠道二级科目 %in%   c("直营二手车","直营综合店") &
                                #渠道二级科目 != "全国代理商"& 
                                合同生效日期<= input$enddate &
                                是否为异地客户 == "是" &
                                申请状态 != "订单已取消"
                              ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      #left_join(x=. , y= Cphone, join_by("申请编号" == "申请编号")) %>%
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目,
             #首付比例...,
             #易鑫分.销售口径.,
             现居住地省份.企业所在省份,
             单位所在省份)
    
  })
  
  #核验六类型客户_超15万订单----
  核验六类型客户_超15万订单 <- reactive({
    rawdata() %>%
      mutate(订单编号 = case_when(合同生效日期>= input$stadate & 
                                渠道二级科目 %in%   c("直营二手车","直营综合店") &
                                合同生效日期<= input$enddate &
                                融资金额 > 150000 &
                                申请状态 != "订单已取消"
                              ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目,
             首付比例...,
             易鑫分.销售口径.)
  })
  
  #核验六类型客户_入职2月内新顾问订单----
  核验六类型客户_入职2月内新顾问订单 <- reactive({
    rawdata() %>%
      mutate(订单编号 = case_when(合同生效日期>= input$stadate &
                                渠道二级科目 %in%   c("直营二手车","直营综合店") &
                                合同生效日期<= input$enddate &
                                金融顾问名称 %in% 入职2月内新顾问名字()  &
                                申请状态 != "订单已取消"
                              ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目,
             首付比例...,
             易鑫分.销售口径.)
  })
  
  #核验六类型客户_365日内逾期大于2单及以上渠道和顾问----
  核验六类型客户_365日内逾期大于2单及以上渠道和顾问 <- reactive({
    rawdata() %>%
      mutate(订单编号 = case_when(合同生效日期>= input$stadate & 
                                合同生效日期<= input$enddate &
                                渠道二级科目 %in%   c("直营二手车","直营综合店") &
                                申请状态 != "订单已取消" &
                                金融顾问名称 %in% 一年内逾期大于2单及以上顾问() |
                                (!is.null(一年内逾期大于2单及以上渠道()) & length(一年内逾期大于2单及以上渠道()) > 0 & #排除筛选后渠道为空的情况
                                   店面名称 %in% 一年内逾期大于2单及以上渠道())
                              #店面名称 %in% !!sym(n_overdue_channels))
                              ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      #left_join(x=. , y= Cphone, join_by("申请编号" == "申请编号")) %>%
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目
             #首付比例...,
             #易鑫分.销售口径.
      )
  })
  
  
  #核验六类型客户_进件日起两个月新渠道订单----
  tempdata <- reactive({
    rawdata() %>%
      mutate(进件日期 = as.Date(进件日期)) %>%
      left_join(合作渠道() %>% 
                  mutate(渠道编号 = as.character(渠道编号),
                         初始生效日期 = as.Date(初始生效日期))
                , by = c("渠道代码" = "渠道编号")
      ) %>% 
      mutate(是否进件日起两个月新渠道 = 
               case_when( 进件日期 <= 初始生效日期 + lubridate::days(60)   ~ "是",
                          TRUE ~ "否")) %>% 
      filter(是否进件日起两个月新渠道 == "是")
  })
  
  
  核验六类型客户_进件日起两个月新渠道订单 <- reactive({
    tempdata() %>% 
      mutate(订单编号 = case_when(合同生效日期 >= input$stadate & 
                                合同生效日期 <= input$enddate &
                                
                                渠道二级科目.x %in%   c("直营二手车","直营综合店") &
                                申请状态 != "订单已取消"
                              ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      #left_join(x=. , y= Cphone, join_by("申请编号" == "申请编号")) %>%
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称.x,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目.x,
             #首付比例...,
             #易鑫分.销售口径.
      )
  })
  
  
  #特殊产品订单，如舒鑫融/一键直秒/直租/理想项目等----
  
  核验六类型客户_特殊产品temp <- reactive({
    rawdata() %>%
      filter( 
        (!is.na(直秒选择) & 直秒选择 != "") | 
          产品类目 == "舒鑫融" | 
          合作项目 == "理想汽车" |
          grepl("直租",产品方案名称)
      )
  })
  
  核验六类型客户_特殊产品 <- reactive({
    核验六类型客户_特殊产品temp() %>% 
      mutate(订单编号 = case_when( 合同生效日期 >= input$stadate & 
                                 合同生效日期 <= input$enddate &
                                 渠道二级科目 %in%   c("直营二手车","直营综合店") &
                                 申请状态 != "订单已取消"
                               ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      #left_join(x=. , y= Cphone, join_by("申请编号" == "申请编号")) %>%
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目,
             首付比例...,
             易鑫分.销售口径.)
  })
  
  #日检查订单----
  日检查订单 <- reactive({
    rawdata() %>%
      mutate(是否为异地客户 = if_else(户籍城市.企业所在城市 != 店面城市,"是","否")) %>% 
      mutate(订单编号 = case_when( 合同生效日期 >= input$stadate &
                                 # 产品类别 != "厂家产品" & #cancel 4S channels
                                 易鑫分.销售口径. <= 500  # modified from 550 to 500 on 14th Jan
                               # 是否为异地客户 == "是" # cancel remoted clients.
                               
                               ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      #left_join(x=. , y= Cphone, join_by("申请编号" == "申请编号")) %>%
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目,
             首付比例...,
             易鑫分.销售口径.)
  })
  
  
  #补足经理电话回访单量----
  
  
  补足经理回访电话数 <- reactive({
    
    # 将输入拆分为多个名字，处理中文/英文逗号和空格
    managers <- input$Managers %>%
      strsplit(split = "[,，]") %>%  # 支持中英文逗号
      unlist() %>%
      trimws()  # 去除前后空格
    
    
    rawdata() %>%
      mutate(订单编号 = case_when( 合同生效日期  >= Sys.Date()- day(Sys.Date()) &
                                 渠道二级科目 %in%   c("直营二手车","直营综合店")
                               ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      #left_join(x=. , y= Cphone, join_by("申请编号" == "申请编号")) %>%
      select(申请编号,
             合同生效日期,
             客户姓名,
             #客户电话,
             产品方案名称,
             金融经理名称,
             金融顾问名称,
             店面名称,
             品牌,
             车系,
             车辆颜色,
             融资金额,
             融资期限,
             每期客户租金,
             渠道二级科目,
             首付比例...,
             易鑫分.销售口径.) %>% 
      filter(金融经理名称 %in% managers ) %>% #input的经理名称
      group_by(金融经理名称) %>% 
      slice_sample( n = as.numeric(input$AmountOfPhonecall) ) %>%  #input的补足经理电话数量
      ungroup()
  })
  
  
  #一键直秒订单近一年逾期跟进----
  
  一键直秒订单近一年逾期跟进 <- reactive({
    rawdata() %>%
      mutate(订单编号 = case_when( 合同生效日期 >= input$date_one_buttom_direction_pass &
                                 直秒类型 == "固定比例" &
                                 最新逾期天数 != 0 #逾期1天就算逾期
                               ~申请编号, .default = FALSE)
      ) %>% 
      subset(订单编号 > 0 )  %>% 
      select(申请编号,
             客户姓名,
             产品方案名称,
             店面名称,
             金融经理名称,
             金融顾问名称,
             合同生效日期,
             已还款期数,
             最新逾期天数,
             当前销售口径剩余本金,
             当前逾期级别,
             直秒选择,
             直秒类型,
             直秒比例...
      )
  })
  
  #一键直秒订单成交数
  一键直秒订单成交数 <- reactive({
    rawdata() %>% #一键直秒订单成交数 筛选下面整个日期开始240701，直秒类型 == "固定比例 "的所有订单
      mutate(订单编号 = case_when( 合同生效日期 >= input$date_one_buttom_direction_pass &
                                 直秒类型 == "固定比例"
                               ~申请编号, .default = FALSE)
      ) %>%
      subset(订单编号 > 0 ) %>%
      {nrow(.)}
    
  })
  
  #一键直秒订单近一年逾期数
  一键直秒订单近一年逾期数 <- reactive({
    一键直秒订单近一年逾期跟进() %>% {nrow(.)}
  })
  
  
  
  
  合并数据 <- reactive({
    list(
      "异地订单" = 核验六类型客户_异地订单(),
      "超15万订单" = 核验六类型客户_超15万订单(),
      "入职2月内新顾问订单" = 核验六类型客户_入职2月内新顾问订单(),
      "365日内逾期大于2单及以上渠道和顾问" = 核验六类型客户_365日内逾期大于2单及以上渠道和顾问(),
      "进件日起两个月新渠道订单" = 核验六类型客户_进件日起两个月新渠道订单(),
      "特殊产品" = 核验六类型客户_特殊产品(),
      "一键直秒订单近一年逾期跟进" = 一键直秒订单近一年逾期跟进(),
      "补足经理回访电话数" = 补足经理回访电话数(),
      "日检查订单"= 日检查订单()
    )
  })
  
  #Function2,逾期231101至今逾期清单----
  逾期清单 <- reactive({
    rawdata() %>% 
      filter(合同生效日期 >= input$Overduedate & 最新逾期天数 >= input$OverdueDays) %>% 
      select(当前逾期级别,已还款期数,产品方案名称,渠道代码,店面主体,申请编号,进件日期,当前销售口径剩余本金,融资金额,
             客户姓名,金融顾问名称,金融经理名称) %>% 
      mutate(跟进情况反馈 = NA, 是否收车 = NA) %>% # 增加2个空column
      arrange(desc(进件日期))
  })
  
  #经理逾期数据————
  经理逾期数据 <- reactive({
    rawdata() %>% 
      filter(合同生效日期 >= input$Overduedate  & 最新逾期天数 >= input$OverdueDays) %>% 
      group_by(金融经理名称) %>% 
      summarise(成交台次 = n_distinct(申请编号),
                成交金额 = sum(车款融资额),
                当前销售口径剩余本金 = sum(当前销售口径剩余本金, na.rm = TRUE)
      ) %>% 
      arrange(desc(成交台次))
  })
  
  
  逾期合并数据 <- reactive({
    list(
      "经理逾期数据" = 经理逾期数据(),
      "逾期清单" = 逾期清单()
    )
  })
  
  
  #Function3,清洗数据----
  
  TidiedRawData <- reactive({
    rawdata() %>% 
      mutate(
        融资金额 = 融资金额 / 10000,
        进件日期 = ymd(进件日期),
        合同生效日期 = ymd(合同生效日期),
        金融经理名称 = fn_drop_parentheses(金融经理名称),
        金融顾问名称 = fn_drop_parentheses(金融顾问名称),
        #pad.by.timedd = pad_by_time(合同生效日期, )
      ) %>% 
      arrange(desc(合同生效日期))
    })
  
  TidiedChannels <- reactive({
    channel <- 合作渠道() %>%
      rename_at('负责员工(金融顾问)', ~'负责员工') %>% 
      mutate(负责员工 = fn_drop_parentheses(负责员工),
             业务上级 = fn_drop_parentheses(业务上级),
             初始生效日期 = ymd(初始生效日期)
      )
    
  })
  
  
  清洗后合并数据 <- reactive({
    list(
      "清洗后历史订单明细" = TidiedRawData(),
      "清洗后渠道列表" = TidiedChannels()
    )
  })
  
  
  
  
  #server output----  
  #展示预览一键直秒逾期
  output$preview <- renderDataTable({
    req(rawdata())
    head(rawdata(), 10)
    head(一键直秒订单近一年逾期跟进(), 10)
  })
  
  #下载六类客户
  output$download <- downloadHandler(
    filename = function(){
      paste0("六类客户",format(Sys.Date(),"%Y%m%d"),".xlsx")
    },
    content = function(file){
      
      writexl::write_xlsx(合并数据(), file)
      
    }
  )
  
  #展示一键直秒一句话统计
  output$One_butten_direction_pass <- renderText({
    paste(month(input$date_one_buttom_direction_pass),"月",day(input$date_one_buttom_direction_pass),"日"
          ,"-",month(Sys.Date()), "月", day(Sys.Date())-1, "日，xx分公司一键直秒合计成交", 
          一键直秒订单成交数(), "单，逾期", 一键直秒订单近一年逾期数(), "单，逾期明细如下：（自动生成成交单量和逾期单量，记得手动修改分公司名称）")
  })
  
  
  #展示经理名字，验证
  output$manager <- renderText({
    input$Managers})
  
  
  
  
  #展示经理逾期数据
  经理逾期数据
  output$Overdue_Clients_By_Managers <- renderDataTable({
    req(rawdata())
    经理逾期数据()
  })
  
  
  #展示逾期清单
  output$overdue_clients <- renderDataTable({
    req(rawdata())
    head(逾期清单(), 10)
  })
  
  #下载逾期部分数据
  output$Overduedownload <- downloadHandler(
    filename = function(){
      paste0("逾期数据",format(Sys.Date(),"%Y%m%d"),".xlsx")
    },
    content = function(file){
      
      writexl::write_xlsx(逾期合并数据(), file)
    }
  )
  
  #展示清洗数据表
  output$tidied_data_comments <- renderText({
    paste0("1，历史订单明细清洗范围 融资金额以万为单位；",
    "2,进件日期，合同生效日期修改为年月日日期格式；",
    "3,单独保留金融经理顾问名字")
  })
  
  
  output$tidied_data <- renderDataTable({
    req(rawdata())
    head(TidiedRawData(), 10)
    head(TidiedChannels(), 10)
  })
  
  #下载清洗后数据源
  output$Tidied_data_download <- downloadHandler(
    filename = function(){
      paste0("清洗后数据源表",format(Sys.Date(),"%Y%m%d"),".xlsx")
    },
    content = function(file){
      
      writexl::write_xlsx(清洗后合并数据(), file)
    }
  )
  
  
  
  
}




shinyApp(ui = ui, server = server)
