library(rvest)
library(httr)
library(jsonlite)
Sys.setenv('R_MAX_VSIZE'=32000000000)
# Install package if not exists
pkgs <- c('data.table', "magrittr", 'reshape2', 'lubridate', 'stringr', 'dplyr', 'tidyr',
          'tidyverse','googlesheets','openxlsx','psych','clipr','formattable',
          'ROracle','dbplyr','DBI')

new.pkgs <- pkgs[!(pkgs %in% installed.packages())]
if (length(new.pkgs)) {
  install.packages(new.pkgs, repos = 'http://cran.csie.ntu.edu.tw/')
}
# Require packages
##lapply : 以迴圈方式require()啟用packages
lapply(pkgs, require, character.only = TRUE)

#------------------------------------#
#useful function

transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_data_frame(x = .)
  return(t_df)
}

get_colnames <- function(df, .collapse = ", ") {
  final <- df %>% colnames() %>% str_c(collapse = .collapse)
  return(final)
}

#Mac本機端與Oracle內的中文問題
#Sys.setenv(NLS_LANG="TRADITIONAL CHINESE_TAIWAN.UTF8")
#Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")

#---------------------------------------------------------#
#開始爬蟲#

###先讀檔案進來
###現在要抓老師的資訊，然後要先想怎麼跟原本的table join
table_course_final <- read_csv("/Users/dtseng02/Documents/Dennis/ntucourse/table_page_final.csv")

###先把老師抓出來
table_teacher_distinct <- table_course_final %>% select(授課教師, link_teacher) %>% distinct(授課教師, link_teacher)

###確定有沒有同名同姓
table_teacher_same_name <- table_teacher_distinct %>% group_by(授課教師) %>% count(sort = T) %>% filter(n > 1) %>% ungroup
value_teacher_same_name <- table_teacher_same_name %>% pull(授課教師)
table_teacher_distinct %>% filter(授課教師 %in% value_teacher_same_name) %>%
  filter(link_teacher != "teacher.php?op=s2&td=000001") %>% count(授課教師, sort = T)
table_teacher_distinct %>% filter(授課教師 == "馬里奧")
table_teacher_distinct %>% filter(授課教師 == "林俊宏")
table_teacher_distinct %>% filter(授課教師 == "陳韻如")
table_course_final %>% filter(授課教師 == "陳韻如")

###a.teacher.php?op=s2&td=000001代表錯誤 不用管這種人 先回去filter掉
###b.發現有同名同姓的老師

###調整策略 
###a.td=000001的用正確的網址去取代
###b.沒有同名同姓就用名字join
###c.同名同姓的人還要加上ID去join

###底下分三種
###a.錯誤的也就是課程網那邊顯示有問題的
table_teacher_error <- table_course_final %>% filter(link_teacher == "teacher.php?op=s2&td=000001") %>%
  select(ID, 授課教師, link_teacher)

###b.沒有同名同姓 跟c.有同名同姓
table_teacher_distinct <- table_course_final %>% filter(link_teacher != "teacher.php?op=s2&td=000001") %>%
  select(授課教師, link_teacher) %>% distinct(授課教師, link_teacher)
table_teacher_same_name <- table_teacher_distinct %>% group_by(授課教師) %>% count(sort = T) %>% filter(n > 1) %>% ungroup

###同名同姓的老師名字抓出來
value_teacher_same_name <- table_teacher_same_name %>% pull(授課教師)

###沒有同名同姓就不留ID
table_teacher_distinct_no_same <- table_teacher_distinct %>% 
  filter(!授課教師 %in% value_teacher_same_name) %>% mutate(link_teacher = str_c("https://nol.ntu.edu.tw/nol/coursesearch/", link_teacher))
###有同名同姓就留ID
table_teacher_distinct_same <- table_course_final %>% filter(授課教師 %in% value_teacher_same_name) %>%
  select(ID, 授課教師, link_teacher)%>% mutate(link_teacher = str_c("https://nol.ntu.edu.tw/nol/coursesearch/", link_teacher))

###寫爬蟲function
###set cookies填寫"登入台大課程網的帳號密碼"的cookies內容
###要找cookies位置: 用chorme瀏覽器的話，按右鍵檢查，
###接下來會跑出一個頁面(google chrome developer)這個可以看原始碼
###打開後找Application的分頁，點開裡面左邊Storage，細項當中的Cookies裡面
###過一段時間要更新裡面內容，因為cookies會失效

crawl_teacher <- function(url) {
  GET(url, 
      set_cookies(user="",
      #             PHPSESSID="",
      #             NOLlang="CH", 
                  user="", ...#還有其他cookie內容記得填
                  ))
}
p_crawl_teacher <- possibly(crawl_teacher, NA)

###測試一下會不會成功
test = p_crawl_teacher("https://nol.ntu.edu.tw/nol/coursesearch/teacher.php?op=s2&td=B46026")
test_df = test %>% read_html() %>% html_nodes(css = "table") %>% html_table(fill=T)
test_df[2] %>% as.data.frame() %>% as_tibble() %>%
  mutate(X1 = str_replace(X1, "：", "")) %>%
  as.data.frame() %>% column_to_rownames(var="X1") %>%
  transpose_df() %>% select(-rowname)

###確定function可以用之後開爬
###先從沒有同名同姓的table開始

###這邊其實只是想創一個空的tibble之後去bind_rows()
table_teacher_df_distinct_tmp <- table_teacher_distinct_no_same[1,] %>% pull(link_teacher) %>%
  p_crawl_teacher() %>% read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
  html_table(fill = T) %>% `[[`(2) %>% 
  mutate(X1 = str_replace(X1, "：", "")) %>% as.data.frame() %>% 
  column_to_rownames(var="X1") %>% transpose_df() %>%
  as_tibble() %>% filter(row_number() < 1)

###從第一頁開始爬，因為一次爬十頁所以算了一下要for loop要跑幾圈
value_total_teacher_start = 1
value_total_teacher = table_teacher_distinct_no_same %>% dim() %>% `[`(1) %>% `/`(10) %>% ceiling()
#value_total_teacher = 243

###有在想會不會有老師在抓的時候沒資料考慮是否要留下ID
###後來結論是不用 只要回去join就知道這人沒資料了

###開始爬蟲
for (i in 1:value_total_teacher) {
  
  j = value_total_teacher_start
  k = j + 9
  
  ###一次爬十個連結
  table_teacher_df_tmp = table_teacher_distinct_no_same[j:k,] %>% filter(!is.na(link_teacher)) %>%
    pull(link_teacher) %>%
    ###裡面的discard是篩掉失效的意思
    map(function(x){x %>% p_crawl_teacher() %>% read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
        html_table(fill = T)})  %>% discard(function(x) length(x) < 1) %>%
    ###這邊只是清資料然後改長相
    map(function(x){x %>% `[[`(2) %>% 
        mutate(X1 = str_replace(X1, "：", "")) %>% as.data.frame() %>% 
        column_to_rownames(var="X1") %>% transpose_df() %>%
        as_tibble()})
  
  ###bind在一起
  table_teacher_df_tmp = table_teacher_df_tmp %>% bind_rows()
  table_teacher_df_distinct_tmp = table_teacher_df_distinct_tmp %>% bind_rows(table_teacher_df_tmp)
  print(value_total_teacher_start)
  value_total_teacher_start = value_total_teacher_start + 10
  
  tmsleep<-sample(8:10,1);tmsleep
  Sys.sleep(tmsleep)
}

table_teacher_distinct_no_same %>% count()
table_teacher_df_distinct_tmp %>% count()

###因為發現名字只有兩個字的老師譬如說你好"在課程頁面會變成"你 好"
###所以清掉空格之後再寫資料
table_teacher_df_distinct_tmp %>% mutate(姓名 = str_replace_all(姓名, " ", "")) %>% write_rds("/Users/dtseng02/Documents/Dennis/table_teacher_df_distinct_tmp.rds")

###確認一下沒抓到的老師是誰，並且確認是不是課程網那邊真的沒資料
table_teacher_distinct_no_same %>% filter(授課教師 %in% c('吳亘承',"黃英哲","李文心")) %>%
  pull(link_teacher) %>%
  map(function(x){x %>% p_crawl_teacher() %>% read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
      html_table(fill = T)}) %>% discard(function(x) length(x) < 1) %>%
  map(function(x){x %>% `[[`(2) %>% 
      mutate(X1 = str_replace(X1, "：", "")) %>% as.data.frame() %>% 
      column_to_rownames(var="X1") %>% transpose_df() %>%
      as_tibble()})
table_teacher_distinct_no_same %>% mutate(ID = row_number()) %>%
  filter(授課教師 %in% c('蔡柏盈'))

###比對一下抓到的跟沒抓到的是誰
table_teacher_gg = table_teacher_df_distinct_tmp %>% select(姓名) %>% mutate(姓名 = str_replace_all(姓名, " ", ""))
table_teacher_distinct_no_same_gg <- table_teacher_distinct_no_same %>% anti_join(table_teacher_gg, by = c("授課教師" = "姓名"))
#table_teacher_distinct_no_same_gg 就是沒有資料的老師們

###兩個真的抓不到的實例子
#吳亘承   https://nol.ntu.edu.tw/nol/coursesearch/teacher.php?op=s2&td=B02042 gg
#李聖珉   https://nol.ntu.edu.tw/nol/coursesearch/teacher.php?op=s2&td=H02032 gg

##########################################

###接著處理同名同姓
###有留ID 回去用ID join 課程/index資料
table_teacher_distinct_same

###這邊其實只是想創一個空的tibble之後去bind_rows()
table_teacher_df_distinct_same_tmp <- table_teacher_distinct_same[1,] %>% pull(link_teacher) %>%
  p_crawl_teacher() %>% read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
  html_table(fill = T) %>% `[[`(2) %>% 
  mutate(X1 = str_replace(X1, "：", "")) %>% as.data.frame() %>% 
  column_to_rownames(var="X1") %>% transpose_df() %>%
  mutate(ID = "") %>%
  as_tibble() %>% filter(row_number() < 1)
value_total_teacher_start_same = 1
value_total_teacher_same = table_teacher_distinct_same %>% dim() %>% `[`(1) %>% `/`(10) %>% ceiling()
#value_total_teacher = 10

###開始爬蟲
for (i in 1:value_total_teacher_same) {
  
  j = value_total_teacher_start_same
  k = j + 9
  
  table_teacher_df_tmp = table_teacher_distinct_same[j:k,] %>% filter(!is.na(link_teacher)) %>%
    pull(link_teacher) %>%
    map(function(x){x %>% p_crawl_teacher() %>% read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
        html_table(fill = T)})  %>% discard(function(x) length(x) < 1) %>%
    map(function(x){x %>% `[[`(2) %>% 
        mutate(X1 = str_replace(X1, "：", "")) %>% as.data.frame() %>% 
        column_to_rownames(var="X1") %>% transpose_df() %>%
        as_tibble()})
  
  names(table_teacher_df_tmp) = pull(filter(table_teacher_distinct_same[j:k,],!is.na(link_teacher)), ID)
  
  table_teacher_df_tmp = table_teacher_df_tmp %>% bind_rows(.id = "ID")
  table_teacher_df_distinct_same_tmp = table_teacher_df_distinct_same_tmp %>% bind_rows(table_teacher_df_tmp)
  print(value_total_teacher_start_same)
  value_total_teacher_start_same = value_total_teacher_start_same + 10
  
  tmsleep<-sample(8:10,1);tmsleep
  Sys.sleep(tmsleep)
}

###寫資料
table_teacher_df_distinct_same_tmp %>% mutate(姓名 = str_replace_all(姓名, " ", "")) %>% write_rds("/Users/dtseng02/Documents/Dennis/table_teacher_df_distinct_same_tmp.rds")

###關掉用不到的連線
closeAllConnections()
gc()
