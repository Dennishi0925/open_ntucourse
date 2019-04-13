library(rvest)
library(httr)
library(jsonlite)
Sys.setenv('R_MAX_VSIZE'=32000000000)
# Install package if not exists
pkgs <- c('data.table', "magrittr", 'reshape2', 'lubridate', 'stringr', 'dplyr', 'tidyr',
          'tidyverse','googlesheets','openxlsx','psych','clipr','formattable')

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

###先讀之前join且tidy後的檔案
###現在已經是tidy data的形式
###重點放在整理column 主要在弄string處理

###讀檔案然後抓十筆資料測試
table_tidy_tmp <- read_rds("/Users/dtseng02/Documents/Dennis/ntucourse/raw_data/table_tidy_tmp.rds")
gg = table_tidy_tmp %>% filter(ID %in% c(10847, 3940, 7625, 2990, 832, 970, 9890, 3355, 7038))

###改column name
gg2 <- gg %>% rename(課程名稱_short = `課程名稱查看課程大綱，請點選課程名稱`,
                          全_半年 =`全/半年`, 必_選修 = `必/選修`, 
                          Email = `E-Mail地址`, ceiba = `Ceiba 課程網頁`,
                          評量方式 = `評量方式(僅供參考)`, office_hours = `Office Hours`)

###1. 看相似的column之間要留或丟 2.針對每個column去整理+重新命名
gg2 %>% colnames()
gg2 %>% get_colnames()
gg2 %>% select(課程要求)#課程概述, 課程目標, 課程要求, 

###大概有兩大類問題: 1.column相似或是2.內容需要整理
###1.相似: a.授課對象相關 b.課程名稱相關 c.時間教室 vs. 上課時間/地點相關
###2.整理: a.時間教室 b.總人數("開放台大系統"斷出來) c.選課限制條件 
###        d.備註 研究一下 e.本學期我預計要選的課程 刪掉 f.rowname刪掉 
###        g.所屬系所/個人首頁網址/Email/聯絡電話/辦公室/個人資訊 都是老師相關資訊
###        h.ceiba跟link_ceiba對一下 i.課程簡介影片 是不是根本空的 j.核心能力關聯有連結嗎
###        k.office hours 有沒有統一格式確認 l.schedule 可能要轉一下日期column的格式 
###        m.課程概述(跟領域有關)/課程目標(提到"能力"): 斷詞needed n.link相關

###1.相似

###a.授課對象
gg2 %>% select(matches("授課對象"))
###有些有分所/系 有些沒分
###從使用者角度來講他們當然會想知道課程是誰開的
###但要避免從前課程歸屬於誰的混亂
###不要再有工商管理系/企管組/科管組開課的爛透感覺了
###還是需要清理出所跟系 這個需要抓完整的來處理
table_tidy_tmp

###b.課程名稱
gg2 %>% select(matches("課程名稱")) %>% mutate(gg = str_remove_all(課程名稱,"[\\p{Letter}]"))
gg2 %>% select(matches("課程名稱")) %>% 
  mutate(gg = str_remove_all(課程名稱,"[:lower:]|[:upper:]|[:punct:]|[:blank:]|[:digit:]")) %>% select(-課程名稱_short)
gg2 %>% select(matches("課程名稱")) %>% 
  mutate(課程名稱 = str_remove_all(課程名稱,"[:punct:]")) %>%
  mutate(gg = str_locate(課程名稱,"[:upper:]")[,1]) %>% #[,1]超讚
  mutate(課程名稱_chinese = str_sub(課程名稱, 1, gg - 1 ), 課程名稱_english = str_sub(課程名稱, gg, 1000))

###c.時間教室 vs. 上課時間/地點
###因為要做filter 所以要把時間分成1.星期幾跟2.第幾節3.有兩個時段也要處理
gg2 %>% select(matches("時間|教室|地點"))

gg2 %>% select(時間教室) %>% mutate(gg = str_locate(時間教室, '\\(')[,1], gg2 = str_locate(時間教室, '\\)')[,1]) %>%
  mutate(aa = str_sub(時間教室, 1, gg-1), bb = str_sub(時間教室, gg, gg2), cc = str_sub(時間教室, gg2 + 1, 100))

###問題一 有些課有兩個時段/上課地點
table_tidy_tmp %>% filter(str_length(上課地點) > 7) %>% 
  #filter(str_detect(時間教室, "請洽系所辦")) %>% 
  sample_n(10) %>%
  select(matches("時間|教室|地點|ID"))

###問題一 有些課教室是請洽系所辦
table_tidy_tmp %>% #filter(str_length(上課地點) > 7) %>% 
  filter(str_detect(時間教室, "請洽系所辦")) %>% 
  sample_n(10) %>%
  select(matches("時間|教室|地點|ID"))

###想法
###(1)str_count()算有幾個時間/教室 再用locate分
###(2)str_extract()拆分pattern

###(1)
gg2 %>% select(時間教室) %>% mutate(gg = str_count(時間教室, '\\(')) %>%
  mutate(aa = str_sub(時間教室, 1, gg-1))

###(2)
###舊的解法 一個一個拆開
gg2 %>% select(時間教室) %>% mutate(hi = str_extract(時間教室, pattern = "([一二三四五].*\\(.*\\)$)?"))
gg2 %>% select(時間教室) %>% mutate(hi = str_extract(時間教室, pattern = "(\\([^(]*\\)){1}"))
gg2 %>% select(時間教室) %>% mutate(hi = str_extract_all(時間教室, pattern = "(\\([^(]*\\)){1}")) %>%
  mutate(gggg = map_dbl(hi, ~length(.))) %>%
  mutate(row = row_number()) %>% 
  mutate(hi1 = map2_chr(hi, gggg, function(x,y){ x %>% "[["(1)}),
         hi2 = map2_chr(hi, gggg, function(x,y){ if(y > 1) x %>% "[["(2) else(x = NA)}),
         hi3 = map2_chr(hi, gggg, function(x,y){ if(y > 2) x %>% "[["(3) else(x = NA)}),
         hi4 = map2_chr(hi, gggg, function(x,y){ if(y > 3) x %>% "[["(3) else(x = NA)}))
gg2 %>% select(時間教室) %>% mutate(hi = str_extract_all(時間教室, pattern = "([一二三四五][:digit:][^()]*){1}")) %>%
  mutate(gggg = map_dbl(hi, ~length(.))) %>%
  mutate(row = row_number()) %>% 
  mutate(hi1 = map2_chr(hi, gggg, function(x,y){ x %>% "[["(1)}),
         hi2 = map2_chr(hi, gggg, function(x,y){ if(y > 1) x %>% "[["(2) else(x = NA)}),
         hi3 = map2_chr(hi, gggg, function(x,y){ if(y > 2) x %>% "[["(3) else(x = NA)}),
         hi4 = map2_chr(hi, gggg, function(x,y){ if(y > 3) x %>% "[["(3) else(x = NA)}))
table_tidy_tmp %>% select(時間教室) %>% mutate(lll = str_length(時間教室)) %>%
  arrange(desc(lll)) %>% mutate(ccc = str_count(時間教室, "\\(")) %>% arrange(desc(ccc)) %>% select(ccc)

gg3 = gg2 %>% select(時間教室) %>% 
  mutate(course_time = str_extract_all(時間教室, pattern = "([一二三四五][:digit:][^()]*){1}")) %>%
  mutate(course_time_length = map_dbl(course_time, ~length(.))) %>%
  mutate(course_time_1 = map2_chr(course_time, course_time_length, function(x,y){ x %>% "[["(1)}),
         course_time_2 = map2_chr(course_time, course_time_length, function(x,y){ if(y > 1) x %>% "[["(2) else(x = NA)}),
         course_time_3 = map2_chr(course_time, course_time_length, function(x,y){ if(y > 2) x %>% "[["(3) else(x = NA)}),
         course_time_4 = map2_chr(course_time, course_time_length, function(x,y){ if(y > 3) x %>% "[["(4) else(x = NA)}),
         course_time_5 = map2_chr(course_time, course_time_length, function(x,y){ if(y > 4) x %>% "[["(5) else(x = NA)})) %>%
  mutate(course_location = str_extract_all(時間教室, pattern = "(\\([^(]*\\)){1}")) %>%
  mutate(course_location_length = map_dbl(course_location, ~length(.))) %>%
  mutate(course_location_1 = map2_chr(course_location, course_location_length, function(x,y){ x %>% "[["(1)}),
         course_location_2 = map2_chr(course_location, course_location_length, function(x,y){ if(y > 1) x %>% "[["(2) else(x = NA)}),
         course_location_3 = map2_chr(course_location, course_location_length, function(x,y){ if(y > 2) x %>% "[["(3) else(x = NA)}),
         course_location_4 = map2_chr(course_location, course_location_length, function(x,y){ if(y > 3) x %>% "[["(4) else(x = NA)}),
         course_location_5 = map2_chr(course_location, course_location_length, function(x,y){ if(y > 4) x %>% "[["(5) else(x = NA)})) %>%
  select(-c(course_location, course_location_length, course_time, course_time_length))

###要怎麼像sweety course做到彈性選課非常重要
###現在的課程網只能星期四的六七節 或是全部的六七節 但不能 星期三的六七節|星期四的第五節 要想辦法做到
###目前想法: 所有都貼標籤 變成二5 二6 二7 人家做搜尋變成 去str_detect()
###可能要先remove地點再來extract weekday比較好?
###後來發現a. 不用extract b. 就算時間跟地點合併在一起也沒差
gg4 = gg2 %>%  
  mutate(course_time = str_extract_all(時間教室, pattern = "([一二三四五][:digit:][^()]*){1}")) %>%
  mutate(course_time = map(course_time, function(x){str_replace_all(x, pattern = ",", replacement = str_extract(x, pattern = "[一二三四五]"))})) %>%
  mutate(course_location = str_extract_all(時間教室, pattern = "(\\([^(]*\\)){1}")) %>%
  mutate(course_location = map(course_location, function(x){str_replace_all(x, pattern = "\\(|\\)", replacement = "")})) %>%
  #mutate(course_time = unlist(map(course_time, ~str_c(., collapse = ";")))) %>%
  mutate(course_time = as.character(map(course_time, ~str_c(., collapse = ";")))) %>%
  #mutate(course_location = unlist(map(course_location, ~str_c(., collapse = ";")))) %>%
  mutate(course_location = as.character(map(course_location, ~str_c(., collapse = ";"))))

###2.整理: a.時間教室 b.總人數("開放台大系統"斷出來) c.選課限制條件 
###        d.備註 研究一下 e.本學期我預計要選的課程 刪掉 f.rowname刪掉 
###        g.所屬系所/個人首頁網址/Email/聯絡電話/辦公室/個人資訊 都是老師相關資訊
###        h.ceiba跟link_ceiba對一下 i.課程簡介影片 是不是根本空的 j.核心能力關聯有連結嗎
###        k.office hours 有沒有統一格式確認 l.schedule 可能要轉一下日期column的格式 
###        m.課程概述(跟領域有關)/課程目標(提到"能力"): 斷詞needed n.link相關

#a.已經解決
#b./c.
gg5 <- gg4 %>% 
  mutate(select_n = as.double(str_remove(總人數, "\\(含開放臺大系統人數.*\\)"))) %>%
  mutate(select_n_taida_system = str_extract(總人數, "\\(含開放臺大系統人數.*")) %>%
  mutate(select_n_taida_system = as.double(str_extract(select_n_taida_system, "[:digit:]+"))) %>%
  mutate(select_restriction = str_extract(選課限制條件, "限.*,")) %>%
  mutate(select_precourse = str_extract(選課限制條件, "本課有先修科目規定")) %>% 
  mutate(select_precourse = if_else(is.na(select_precourse), F, T)) %>% 
  mutate(select_n_taida = str_extract(選課限制條件, "本校修課人數上限[^外]*人")) %>%
  mutate(select_n_taida = as.double(str_extract(select_n_taida, "[:digit:]+"))) %>%
  mutate(select_n_other_dep = str_extract(選課限制條件, "外系人數限制.*")) %>%
  mutate(select_n_other_dep = as.double(str_extract(select_n_other_dep, "[:digit:]+")))

#d.應該還好
#原本在想要不要加上英文或中文授課但發現只要又link_
gg6 <- gg5 %>% 
  mutate(course_language = case_when(str_detect(備註,"本課程中文授課") ~ "中文授課",
                                     str_detect(備註,"本課程以英語授課") ~ "英文授課",
                                     str_detect(備註,"中英雙語授課") ~ "中英授課",
                                     TRUE ~ "check")) %>%
  mutate(select_primary_close = str_detect(備註, "初選不開放。"))

#e.f.
#e.本學期我預計要選的課程 其實是有意義的 之後要想怎麼讓同學預先選課
#但這個欄位要刪掉啦
gg7 <- gg6 %>% select(-rowname, -本學期我預計要選的課程)

#g.所屬系所/個人首頁網址/Email/聯絡電話/辦公室/個人資訊/職稱 都是老師相關資訊
gg8 <- gg7 %>% rename_at(vars(matches("職稱|所屬系所|個人首頁網址|Email|聯絡電話|辦公室|個人資訊")), funs(str_c("老師_", .)))

#h.ceiba 是課程的 link_ceiba有些是英文化程度 留下課程的就好了
#i.課程簡介影片是空的 j.核心能力關聯沒有連結
gg9 <- gg8 %>% select(-link_ceiba, -簡介影片, -課程簡介影片, -核心能力關聯, -課程網頁) %>% rename(link_ceiba= ceiba)

#k.office hours 沒有統一格式所以放著就好
gg9 %>% select(matches("hours"))
table_tidy_tmp %>% select(matches("hours")) %>% filter(!is.na(`Office Hours`))

#l.schedule 可能要轉一下日期column的格式 現在先不用管
gg9 %>% select(matches("schedule")) %>% unnest()

#m.課程概述(跟領域有關)/課程目標(提到"能力"): 斷詞needed 
gg9 %>% select(課程概述,課程目標,課程要求)

#n.link相關 link_teacher刪掉 link_map難處理先不管了
gg10 <- gg9 %>% select(-link_teacher, -link_map)

#改column型態並且改名字
gg10 %>% get_colnames(.collapse = "|")
gg10 %>% dim()
gg10 %>% 
  select(ID, 課程名稱, 流水號, 授課對象, 授課對象_A, 授課對象_B, 課號, 班次, 課程名稱_short, 學分, 
         課程識別碼, 全_半年, 必_選修, 授課教師, 加選方式, 開課學期, 選課限制條件, 總人數, 
         時間教室, 備註, link_ceiba, link_course, link_precourse, 
         老師_所屬系所, 老師_職稱, 老師_個人首頁網址, 老師_Email, 老師_聯絡電話, 老師_辦公室, 老師_個人資訊, 
         課程概述, 課程目標, 課程要求, office_hours, 參考書目, 指定閱讀, 評量方式, 上課時間, 上課地點,
         course_time, course_location, #info 
         schedule, select_restriction, course_language, select_primary_close,
         select_n, select_n_taida_system, select_precourse, select_n_taida, select_n_other_dep) %>%
  get_colnames("|")
# select_primary_close = 初選不開放
gg10 %>% select(course_time, course_location)
gg10 %>% select(select_n, select_n_taida_system, select_precourse, select_n_taida, select_n_other_dep)

gg10 %>%
  rename(識別碼 = 課程識別碼, 名稱_short = 課程名稱_short, 名稱 = 課程名稱) %>%
  rename_at(vars(matches("ID|名稱|流水號|授課對象|授課對象_A|授課對象_B|課號|班次|名稱_short|學分|識別碼|全_半年|必_選修|授課教師|加選方式|開課學期|選課限制條件|總人數|時間教室|備註|link_ceiba|link_course|link_precourse")), funs(str_c("課程_", .))) %>%
  rename_at(vars(matches("課程概述|課程目標|課程要求|office_hours|參考書目|指定閱讀|評量方式|上課時間|上課地點|course_time|course_location|schedule|select_restriction|course_language|select_primary_close|select_n|select_n_taida_system|select_precourse|select_n_taida|select_n_other_dep")), funs(str_c("詳細_", .))) #%>%
gg10 %>% select(matches("課程名稱|開課學期|授課對象_A|授課對象_B|link_ceiba|課程簡介影片|課程概述|課程目標|課程要求|office_hours|參考書目|指定閱讀|評量方式"))
gg10 %>% get_colnames()
gg10 %>% select(課程_ID, 課程_流水號, 課程_授課對象, 課程_課號, 課程_班次)

###all
table_tidy_final <- table_tidy_tmp %>% 
  rename(課程名稱_short = `課程名稱查看課程大綱，請點選課程名稱`,
              全_半年 =`全/半年`, 必_選修 = `必/選修`, 
              Email = `E-Mail地址`, ceiba = `Ceiba 課程網頁`,
              評量方式 = `評量方式(僅供參考)`, office_hours = `Office Hours`) %>%
  mutate(course_time = str_extract_all(時間教室, pattern = "([一二三四五][:digit:][^()]*){1}")) %>%
  mutate(course_time = map(course_time, function(x){str_replace_all(x, pattern = ",", replacement = str_extract(x, pattern = "[一二三四五]"))})) %>%
  mutate(course_location = str_extract_all(時間教室, pattern = "(\\([^(]*\\)){1}")) %>%
  mutate(course_location = map(course_location, function(x){str_replace_all(x, pattern = "\\(|\\)", replacement = "")})) %>%
  #mutate(course_time = unlist(map(course_time, ~str_c(., collapse = ";")))) %>%
  mutate(course_time = as.character(map(course_time, ~str_c(., collapse = ";")))) %>%
  #mutate(course_location = unlist(map(course_location, ~str_c(., collapse = ";")))) %>%
  mutate(course_location = as.character(map(course_location, ~str_c(., collapse = ";")))) %>%
  mutate(select_n = as.double(str_remove(總人數, "\\(含開放臺大系統人數.*\\)"))) %>%
  mutate(select_n_taida_system = str_extract(總人數, "\\(含開放臺大系統人數.*")) %>%
  mutate(select_n_taida_system = as.double(str_extract(select_n_taida_system, "[:digit:]+"))) %>%
  mutate(select_restriction = str_extract(選課限制條件, "限.*,")) %>%
  mutate(select_precourse = str_extract(選課限制條件, "本課有先修科目規定")) %>% 
  mutate(select_precourse = if_else(is.na(select_precourse), F, T)) %>% 
  mutate(select_n_taida = str_extract(選課限制條件, "本校修課人數上限[^外]*人")) %>%
  mutate(select_n_taida = as.double(str_extract(select_n_taida, "[:digit:]+"))) %>%
  mutate(select_n_other_dep = str_extract(選課限制條件, "外系人數限制.*")) %>%
  mutate(select_n_other_dep = as.double(str_extract(select_n_other_dep, "[:digit:]+"))) %>%
  mutate(course_language = case_when(str_detect(備註,"本課程中文授課") ~ "中文授課",
                                     str_detect(備註,"本課程以英語授課") ~ "英文授課",
                                     str_detect(備註,"中英雙語授課") ~ "中英授課",
                                     TRUE ~ "check")) %>%
  mutate(select_primary_close = str_detect(備註, "初選不開放。")) %>%
  select(-rowname, -本學期我預計要選的課程) %>%
  rename(識別碼 = 課程識別碼, 名稱_short = 課程名稱_short, 名稱 = 課程名稱) %>%
  select(-link_ceiba, -簡介影片, -課程簡介影片, -核心能力關聯, -課程網頁) %>% rename(link_ceiba= ceiba) %>%
  select(-link_teacher, -link_map) %>%
  rename_at(vars(matches("職稱|所屬系所|個人首頁網址|Email|聯絡電話|辦公室|個人資訊")), funs(str_c("老師_", .))) %>%
  rename_at(vars(matches("課程概述|課程目標|課程要求|office_hours|參考書目|指定閱讀|評量方式|上課時間|上課地點|course_time|course_location|schedule|select_restriction|course_language|select_primary_close|select_n|select_n_taida_system|select_precourse|select_n_taida|select_n_other_dep")), funs(str_c("詳細_", .))) %>%
  rename_at(vars(matches("名稱|流水號|授課對象|授課對象_A|授課對象_B|課號|班次|名稱_short|學分|識別碼|全_半年|必_選修|授課教師|加選方式|開課學期|選課限制條件|總人數|時間教室|備註|link_ceiba|link_course|link_precourse")), funs(str_c("課程_", .))) %>%
  rename(課程_ID = ID)

table_tidy_final

###結論
#待解: 
#m.課程概述(跟領域有關)/課程目標(提到"能力"): 斷詞needed 
#l.schedule
#本學期我預計要選的課程
