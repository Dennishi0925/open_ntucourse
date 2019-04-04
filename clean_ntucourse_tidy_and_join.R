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
#開始清理#

###先讀之前爬下來的檔案
###共有四大部分: course index/teacher info(同名同姓/非同名同姓)/course info/course schedule
table_course_info_raw <- read_rds("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_course_info_n.rds")
table_teacher_distinct_raw <- read_rds("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_teacher_df_distinct_tmp.rds")
table_teacher_same_raw <- read_rds("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_teacher_df_distinct_same_tmp.rds")
table_course_schedule_raw <- read_rds("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_course_schedule_n.rds")
table_page_final_raw <- read_csv("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_page_final.csv")

###想法:
###我要弄的網站重點是讓同學們覺得說
###a.可以找到我要找的東西
###b.探索未知的課程
###所以很大的重點是搜尋
###那我的搜尋結果應該也是先顯示課程列表就跟現有的一樣
###他有興趣再點進去
###那我要怎麼讓他點進去
###第一種做法是所有資訊都join一起所以其實有一張巨型table
###點特定課程的時候就告訴table 我要filter那個課程然後select course/schedule information的欄位
###第二種做法是資訊分開 看他在哪個搜尋框搜尋就去哪個table 撈資料
###這樣感覺會遇到一個問題 就譬如說有個人想找任何有提到統計的課
###我撈課程名稱有統計的 and course info 的有統計的
###發現ID = 11的course info有包含統計學 那我還是要去串回來啊 那這樣不如一開始就串好

table_teacher_distinct_raw
table_teacher_same_raw
table_course_info_raw
table_course_schedule_raw
table_page_final_raw

###目標是想創一張大表串所有表 base 是 course index

###先串老師資訊
table_page_final_raw %>% count() #12864
table_teacher_distinct_raw %>% count() #2337
table_teacher_same_raw %>% count() #100

table_page_final_teacher_distinct_tmp <- table_page_final_raw %>% 
  left_join(table_teacher_distinct_raw, by = c("授課教師" = "姓名"))
table_page_final_teacher_same_tmp <- table_page_final_raw %>% 
  left_join(mutate(table_teacher_same_raw, ID = as.double(ID)), by = c("授課教師" = "姓名", "ID" = "ID"))

table_page_final_teacher_tmp <- table_page_final_raw %>% 
  left_join(table_teacher_distinct_raw, by = c("授課教師" = "姓名")) %>% 
  left_join(mutate(table_teacher_same_raw, ID = as.double(ID)), by = c("授課教師" = "姓名", "ID" = "ID"))

###上面那個產生了duplicate column
###底下先把同名同姓分開 各自join再bind_rows
value_teacher_same <- table_teacher_same_raw %>% distinct(姓名) %>% pull()
table_page_final_raw_same <- table_page_final_raw %>% filter(授課教師 %in% value_teacher_same)
table_page_final_raw_distinct <- table_page_final_raw %>% filter(!授課教師 %in% value_teacher_same)
table_page_final_teacher_tmp <- 
  bind_rows((table_page_final_raw_same %>% 
               left_join(mutate(table_teacher_same_raw, ID = as.double(ID)), by = c("授課教師" = "姓名", "ID" = "ID"))),
            (table_page_final_raw_distinct %>%
               left_join(table_teacher_distinct_raw, by = c("授課教師" = "姓名"))))

###串完老師串課程資訊
###看一下各自有什麼column
table_page_final_teacher_tmp %>% filter(ID < 3001 & ID > 2990) %>% select(ID, everything())
table_course_info_raw %>% mutate(ID = as.double(ID)) %>% filter(ID < 3001 & ID > 2990)
###看一下有什麼重複的column
a = table_page_final_teacher_tmp %>% colnames() %>% as_tibble()
b = table_course_info_raw %>% colnames() %>% as_tibble()
a %>% inner_join(b) %>% pull() %>% str_c(collapse = ", ")

###底下是抓出來重複的column
table_page_final_teacher_tmp %>% 
  select(授課對象, 課號, 班次, 學分, 課程識別碼, `全/半年`, `必/選修`, 授課教師, 備註, ID)
table_course_info_raw %>% select(授課對象, 課號, 班次, 學分, 課程識別碼, `全/半年`, `必/選修`, 授課教師, 備註, ID) %>% 
  mutate(ID = as.double(ID))

###踢掉重複的之後把授課對象分成兩部分 資訊比較完整
table_course_info_raw_tmp <- table_course_info_raw %>% 
  select(-c(課號, 班次, 學分, 課程識別碼, `全/半年`, `必/選修`, 授課教師, 備註)) %>% 
  mutate(ID = as.double(ID)) %>%
  mutate(授課對象 = str_remove_all(授課對象," ")) %>%
  mutate(授課對象 = str_replace(授課對象,"[[:space:]]", ";")) %>%
  separate(授課對象, sep = ";", into = c("授課對象_A","授課對象_B"))

table_page_final_course_info_tmp <- table_page_final_teacher_tmp %>% left_join(table_course_info_raw_tmp, by = "ID")

###串完課程資訊串課程進度 大魔王
###因為不同課程的schedule理論上要相同 第一週第二週依此類推
###但因為有連續假期 因為有課一週上兩次 因為有密集課程 因為有老師出國開會
###導致無法用第OO週當column name因為不distinct 所以只好用比較醜的長相呈現
###每個課程都有各自的三個row 滿醜
table_course_schedule_raw %>% 
  select(ID, everything()) %>%
  filter(!is.na(ID_1))
table_course_schedule_raw %>%
  filter(!is.na(ID_70))
table_course_schedule_raw %>% group_by(rowname) %>% count()

###原本在想說要整理成一個統一格式
###但從使用者的角度來看 課程進度不會馬上display出來
###所以也不用整理
###重點是放在 a. NA column踢掉 b. 我要看 ID = 100的課程要怎麼呈現
###第一個想法是 使用者搜尋統計 符合的有ID = c(100,101,103), 使用者點進去課程之後再 filter ID = 100
###第二個想法是 跟上面一模一樣 只是用nested data frame 資料保存不只比較整齊 還可以join 回去

###第一個想法: 不用做任何改變 user call特定頁面再去filter就好
###第二個想法: join一波
table_tidy_tmp <- table_page_final_course_info_tmp %>% left_join(nest(mutate(table_course_schedule_raw, ID = as.double(ID)), -ID, .key = "schedule"), by = "ID")

###搞定了
###this is how we roll!
table_tidy_tmp %>% select(ID, everything()) %>% write_rds("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_tidy_tmp.rds")
