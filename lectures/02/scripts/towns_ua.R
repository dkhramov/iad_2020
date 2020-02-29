library(rvest)

## Численность городского населения УССР по переписи 1959 г.

hdoc <- read_html('http://www.demoscope.ru/weekly/ssp/ussr59_reg2.php', 
                  encoding='windows-1251')
table <- hdoc %>% html_node('table table table') %>% html_table()
names(table) <- table[1,]

# Только УССР
ua1959 <- table[4:2244,]

ua1959.1 <- as.data.frame(lapply(ua1959, function(x) gsub('&nbsp',NA,x)))
ua1959.1 <- na.omit(ua1959.1)

# Выделим только города и численность их жителей обоего пола
town_ua_1959 <- ua1959.1[grep('г\\.',ua1959.1[,1]),1:2]

town_ua_1959 <- as.data.frame(lapply(town_ua_1959, function(x) gsub('г\\. ','',x)))
town_ua_1959 <- as.data.frame(lapply(town_ua_1959, function(x) gsub(' \\(рц\\)','',x)))

names(town_ua_1959) <- c('город', 'население')

write.table(town_ua_1959,'town_ua_1959.csv',quote=F,sep=',')


## Численность городского населения УССР по переписи 1989 г.

hdoc <- read_html('http://www.demoscope.ru/weekly/ssp/sng89_reg2.php', 
                  encoding='windows-1251')
table <- hdoc %>% html_node('table table table') %>% html_table()
names(table) <- table[1,]

# Только УССР
ua1989 <- table[2:2049,]

ua1989.1 <- as.data.frame(lapply(ua1989, function(x) gsub('&nbsp',NA,x)))
ua1989.1 <- na.omit(ua1989.1)

# Выделим только города и численность их жителей обоего пола
town_ua_1989 <- ua1989.1[grep('г\\.',ua1989.1[,1]),1:2]

town_ua_1989 <- as.data.frame(lapply(town_ua_1989, function(x) gsub('г\\. ','',x)))
town_ua_1989 <- as.data.frame(lapply(town_ua_1989, function(x) gsub('\\.','',x)))
town_ua_1989 <- as.data.frame(lapply(town_ua_1989, function(x) gsub(' \\(.*\\)','',x)))

names(town_ua_1989) <- c('город', 'население')

write.table(town_ua_1989,'town_ua_1989.csv',quote=F,sep=',')
