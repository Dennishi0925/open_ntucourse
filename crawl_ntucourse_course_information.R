library(rvest)
library(httr)
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

###先讀之前爬下來的index檔案
a = read_csv("/Users/dtseng02/Documents/Dennis/ntucourse/table_page_final.csv")

###取得course的link
table_link_course = a %>% select(link_course, ID) %>% filter(!is.na(link_course))
value_total_course = table_link_course %>% count() %>% pull() %>% `/`(10) %>% round()

###創一個空的data frame for 課程資訊
table_course_info_n = "https://nol.ntu.edu.tw/nol/coursesearch/print_table.php?course_id=207%2012002&class=&dpt_code=2070&ser_no=69379&semester=107-2&lang=CH" %>%
  read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
  html_table(fill = T) %>% `[[`(1) %>% as_tibble() %>% 
  select(X1,X2) %>% filter(!str_detect(X1,"\r")) %>%
  filter(! ( (is.na(X1)|X1=="") & (is.na(X2)|X2=="")) ) %>%
  mutate_all(funs(ifelse(is.na(.), "", .))) %>%
  filter(X1 %in% c(course_column_a,course_column_b)) %>%
  as.data.frame() %>% column_to_rownames(var="X1") %>%
  transpose_df() %>% select(-rowname) %>%
  mutate(ID = "") %>% select(ID, everything()) %>%
  filter(row_number() < 1)

###創一個空的data frame for 課程進度
table_course_schedule_n = "https://nol.ntu.edu.tw/nol/coursesearch/print_table.php?course_id=207%2012002&class=&dpt_code=2070&ser_no=69379&semester=107-2&lang=CH" %>%
  read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
  html_table(fill = T) %>% `[[`(1) %>% as_tibble() %>% 
  select(X1,X2,X3) %>%
  filter(!str_detect(X1,"\r")) %>%
  filter(! ( (is.na(X1)|X1=="") & (is.na(X2)|X2=="") & (is.na(X3)|X3=="") ) ) %>%
  mutate_all(funs(ifelse(is.na(.), "", .))) %>%
  filter(str_detect(X1, "週次|第.*週")) %>%
  mutate(ID = str_c("ID_", row_number())) %>% 
  select(ID, everything()) %>%
  as.data.frame() %>% column_to_rownames(var="ID") %>%
  transpose_df() %>% 
  filter(row_number() < 1)

course_column_a = c('課程名稱','開課學期','授課對象','授課教師','課號','課程識別碼','班次','學分','全/半年','必/選修','上課時間','上課地點','備註','Ceiba 課程網頁','課程簡介影片','核心能力關聯')
course_column_b = c('課程概述','課程目標','課程要求','Office Hours','參考書目','指定閱讀','評量方式(僅供參考)')

###開始爬蟲
for (i in 1:value_total_course) {#value_total_course
  
  j = (10*i - 9)
  k = j + 9
  
  ###讓網址完整，包含補上前綴以及將空白改成"%20"，接下來讀檔
  ###https://stackoverflow.com/questions/1634271/url-encoding-the-space-character-or-20
  table_sub_all = table_link_course[j:k,] %>%
    mutate(link_course = str_c("https://nol.ntu.edu.tw/nol/coursesearch/", link_course)) %>%
    mutate(link_course = str_replace_all(link_course," ", "%20")) %>% pull(link_course) %>%
    map(function(x){x %>% read_html(encoding = "big5") %>% html_nodes(css = "table") %>% 
        html_table(fill = T)}) %>%
    map(function(x){x %>% `[[`(1) %>% as.data.frame() %>% 
        as_tibble() %>% select(X1,X2,X3)})
  
  ###加上ID
  names(table_sub_all) = pull(table_link_course[j:k,], ID)

  ###拆兩部分 一個課程資訊一個課程進度
  
  ###課程資訊
  table_course_info = table_sub_all %>% map(function(x){x %>% select(X1,X2) %>%
    filter(!str_detect(X1,"\r")) %>%
    filter(! ( (is.na(X1)|X1=="") & (is.na(X2)|X2=="")) ) %>%
    mutate_all(funs(ifelse(is.na(.), "", .))) %>%
    filter(X1 %in% c(course_column_a,course_column_b)) %>%
    as.data.frame() %>% column_to_rownames(var="X1") %>%
    transpose_df() %>% select(-rowname)})
  
  ###課程進度
  table_course_schedule = table_sub_all %>% map(function(x){x %>% select(X1,X2,X3) %>%
    filter(!str_detect(X1,"\r")) %>%
    filter(! ( (is.na(X1)|X1=="") & (is.na(X2)|X2=="") & (is.na(X3)|X3=="") ) ) %>%
    mutate_all(funs(ifelse(is.na(.), "", .))) %>%
    filter(str_detect(X1, "週次|第.*週"))}) %>%
    ###因為有些課程沒有任何進度資料，為了避免加入一個條件判斷
    map(function(x){
      if (pull(count(x)) < 1) x %>% add_row(X1 = NA_character_, X2 = NA_character_, X3 = NA_character_) else as_tibble(x)
    }) %>%
    ###原本是直接仿照課程資訊直接transpose
    ###但是因為補課等關係所以一週會有兩堂課column就無法distinct
    ###所以改變流程手動加入ID欄位再transpose就沒有問題了
    ###缺點是轉置後的dataframe會很sparse但至少loop不會斷日後再來處理髒資料
    map(function(x){x %>% mutate(ID = str_c("ID_", row_number())) %>% 
        select(ID, everything()) %>%
        as.data.frame() %>% column_to_rownames(var="ID") %>%
        transpose_df()}) 
  
  ###從list結構轉成data frame結構
  table_course_info = table_course_info %>% bind_rows(.id = "ID")
  table_course_schedule = table_course_schedule %>% bind_rows(.id = "ID")
  
  ###用完整的tibble/list去bind這次迴圈抓下來的tibble
  table_course_info_n = table_course_info_n %>% bind_rows(table_course_info)
  table_course_schedule_n = table_course_schedule_n %>% bind_rows(table_course_schedule)
  
  ###印出數字告訴自己進度然後更新要爬的頁數
  print(i)
  
  ###休息一下
  tmsleep<-sample(3:5,1);tmsleep
  Sys.sleep(tmsleep)
}

###寫檔案出去
table_course_info_n %>% write_rds("/Users/dtseng02/Documents/Dennis/table_course_info_n.rds")
table_course_schedule_n %>% write_rds("/Users/dtseng02/Documents/Dennis/table_course_schedule_n.rds")

###如果爬到一半關掉記得先讀檔案進來
table_course_info_n <- read_rds("/Users/dtseng02/Documents/Dennis/table_course_info_n.rds")
table_course_schedule_n <- read_rds("/Users/dtseng02/Documents/Dennis/table_course_schedule_n.rds")

###關掉用不到的連線
closeAllConnections()
gc()
