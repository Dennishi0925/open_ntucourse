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

###拆解台大課程網網址結構
###一開始有考慮填上系所跟課號
###後來發現直接從所有課程頁面開始爬就好會最完整
course_list_url_first = "https://nol.ntu.edu.tw/nol/coursesearch/search_result.php?alltime=yes&allproced=yes&cstype=1&csname=&current_sem=107-2&op=stu&startrec="
course_list_start = 0 #開始頁數
course_list_page_cnt = 100 #每頁顯示資料筆數
course_list_url_last = "&week1=&week2=&week3=&week4=&week5=&week6=&proced0=&proced1=&proced2=&proced3=&proced4=&procedE=&proced5=&proced6=&proced7=&proced8=&proced9=&procedA=&procedB=&procedC=&procedD=&allsel=yes&selCode1=&selCode2=&selCode3=&page_cnt="

###把所有課程總頁數讀出來
value_total = url %>% read_html() %>%
  html_nodes(css = "#select_sem+ font b") %>% html_text() %>% as.double()
#value_total = 12900 #共有12900頁
value_total_page = round(value_total/course_list_page_cnt,0)

###這邊其實只是想創一個空的tibble之後去bind_rows()
table_page = list()

###開始爬蟲
for (i in 1:value_total_page) {
  
  ###讓網址完整，裡面的course_list_start會隨著迴圈結束更新
  url = str_c(course_list_url_first, course_list_start, course_list_url_last, course_list_page_cnt)
  ###爬下當頁所有資訊
  table_urls <- url %>% read_html() %>% html_nodes("body > table:nth-child(5)")
  table_page_tmp = table_urls %>% html_table() %>% as.data.frame() %>% as_tibble() 
  colnames(table_page_tmp) <- as.character(unlist(table_page_tmp[1,]))
  table_page_tmp = table_page_tmp[-1, ]
  
  ###特別處理課程資訊後面的連結如ceiba與老師等
  table_course_xml <- table_urls %>% html_nodes("tr")
  table_course_list = map(table_course_xml, function(x){x %>% html_nodes( "a") %>% html_attr("href")})
  table_course_list[[1]] <- NULL
  ###若有連結就新增欄位
  table_course_tmp = map(table_course_list, function(x)
  {x %>% unlist() %>% list(link_all = .) %>% as_tibble() %>%
      mutate(link_tag = case_when(str_detect(link_all, "print_table") ~ "link_course",
                                  str_detect(link_all, "teacher") ~ "link_teacher",
                                  str_detect(link_all, "map.ntu") ~ "link_map",
                                  str_detect(link_all, "ceiba") ~ "link_ceiba",
                                  str_detect(link_all, "print_pre_course") ~ "link_precourse",
                                  str_detect(link_all, "question.php") ~ "link_englishlevel",
                                  TRUE ~ NA_character_)) %>%
      filter(!is.na(link_tag)) %>% group_by(link_tag) %>%
      summarise(link = str_c(link_all, collapse = "^")) %>% as.data.frame() %>% column_to_rownames(var="link_tag") %>%
      transpose_df() %>% select(-rowname)}) %>% bind_rows() 
  ###transpose雖然會導致有些column沒有東西譬如說沒有CEIBA連結者
  ###但是bind_rows是以多的為主，所以ceiba_tag還會有那欄變成NA
  table_page[[i]] = bind_cols(table_page_tmp, table_course_tmp) %>% mutate(ID = course_list_start + row_number())
  course_list_start = course_list_start + course_list_page_cnt
  tmsleep<-sample(11:15,1);tmsleep
  Sys.sleep(tmsleep)
}
table_page_final = bind_rows(table_page)
table_page_final %>% write_csv("/Users/dtseng02/Documents/Dennis/ntucourse/table_page_final.csv")

###參考資訊
### purrr list of lists
# https://stackoverflow.com/questions/46392282/map-function-to-second-level-of-nested-list-using-purrr
### list of lists without names to df or tibble
# https://stackoverflow.com/questions/43066762/assign-names-to-data-frame-with-as-data-frame-function
### set cookies
# https://stackoverflow.com/questions/26441865/how-to-properly-set-cookies-to-get-url-content-using-httr
### Get href property for each row in a table using rvest with multiple links on each row
# https://stackoverflow.com/questions/50046894/get-href-property-for-each-row-in-a-table-using-rvest


