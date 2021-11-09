library(tidyverse)
library(dslabs)

# ---- Data import -----

# File provided by the dslabls package
path<-system.file("extdata", package = "dslabs")
list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename) # This function is aware of the / each system uses
fullpath
file.copy(fullpath, getwd())    # Creates a copy of the file from the package to our working directory
file.exists(filename)     # Checks if the file exists in our working directory

# First, take a look at the file to know how to import it
read_lines("murders.csv", n_max = 3)

dat <- read_csv(filename)
head(dat)
dat2<- read.csv(filename)
head(dat2)
# R base functions creates a data frame instead of a tibble, and chracters are converted to factors

# Download files from the internet
tmp_filename <- tempfile()    # Creates a file name unlikely not to be unique
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
download.file(url, tmp_filename)  # Download file from the internet, save it with the name determined before
dat <- read_csv(tmp_filename) # Import data to R
file.remove(tmp_filename)   # Delete file from working directory

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url,  n_max = 3)

data<-read_csv(url, col_names = FALSE)

# ---- Tidy data -----
data("gapminder")
# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)
# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# Compare to original data
path<- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data<-read.csv(filename)
head(wide_data)
# Convert it from wide to tidy
new_tidy_data <- wide_data%>%
  gather(year, fertility, X1960:X2015, convert = TRUE)
head(new_tidy_data)
class(new_tidy_data$year)

# Convert it back to wide data
new_wide_data <- new_tidy_data %>%
  spread(year, fertility)

# Separate and unite
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
dat <- raw_dat %>%
  gather(key, value, -country)
head(dat)
dat%>%
  separate(key, c("year", "variable_name"), "_", extra = "merge") %>%
  spread(variable_name, value)

# Alternative
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill="right")%>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")%>%
  spread(variable_name, value)%>%
  rename(fertility = fertility_NA)

head(co2)
co2_wide <- data.frame(matrix(co2, ncol=12, byrow = TRUE)) %>%
  mutate(year = as.character(1959:1997)) %>%
  rename(`1`=X1, `2`=X2, `3`=X3, `4`=X4, `5`=X5, `6`=X6, `7`=X7, `8`=X8, `9`=X9,
         `10`=X10, `11`=X11, `12`=X12)
head(co2_wide)
co2_tidy<-co2_wide %>%
  gather(month, co2, -year) %>%
  ggplot(aes(as.numeric(month), co2, color=year))+
  geom_line()

data("admissions")
data<-admissions %>% select(-applicants)
head(data)
data%>%
  spread(gender, admitted)
tmp<-gather(admissions, key, value, admitted:applicants)
tmp
tmp2<-unite(tmp, column_name, gender, key, sep="_")

# Combining tables: join, binding, set operators
library(ggrepel)
data("murders")
data("polls_us_election_2016")
identical(murders, results_us_election_2016)

tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb))+
  geom_point()+
  geom_text_repel()+
  scale_x_continuous(trans = "log2")+
  scale_y_continuous(trans = "log2")+
  geom_smooth(method = "lm", se=FALSE)

tab1<-slice(murders, 1:6)%>%
  select(state, population)
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% 
  select(state, electoral_votes)
tab2
tab1
left_join(tab1, tab2) # Includes all rows in x
inner_join(tab1, tab2)  # Includes all rows in x and y
full_join(tab1, tab2)   # Includes all rows in x or y
semi_join(tab1, tab2)   # Return all rows in x with a match in y
anti_join(tab1, tab2)   # Return all rows in x without a match in y

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3) # Bind dataframes together by column
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2) # Bind dataframes together by row

# Operators
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)   # Result is the rows the tables or datasets have in common, provided they have the same columns
union(tab1, tab2)     # Result is the union of both tables provided they have the same columns, but the rows they have in common appear just once
setdiff(tab1, tab2) # Function non-symmetric, different answers if we change the arguments. Shows the rows that are different.
setequal(tab1, tab2)  # Tells us if two sets are equal regardless of order

library(Lahman)
top <- Batting %>%
  filter(yearID==2016)%>%
  arrange(desc(HR))%>%
  slice(1:10)
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_salary<-Salaries %>%
  filter(yearID==2016)%>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

head(AwardsPlayers)
AwardsPlayers%>%
  filter(yearID==2016)%>%
  anti_join(top_names)%>%
  arrange(desc(playerID))%>%
  summarize()
length(unique(x$playerID))

# Web scratching
library(rvest)
url<-"https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state" # Import webpage
h <- read_html(url)
tab <- h %>% html_nodes("table")    # Extracts all nodes from this type in the document
tab <- tab [[2]]    # Choose the node with the data we want
tab <- tab %>% html_table    # Convert to a dataframe
tab <- tab %>%  # Change names of the columns
  setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", 
             "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

# Guacamole recipe
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_text(nodes[[8]])
tab_1<-html_table(nodes[[10]])
tab_1<-tab_1[,-1]
tab_2<-html_table(nodes[[19]])
tab_1<-tab_1[-1,]
tab_1<-tab_1 %>% setNames(c("Team", "Payroll", "Avergae"))
tab_2<-tab_2[-1,]
tab_2<-tab_2 %>% setNames(c("Team", "Payroll", "Avergae"))
full_join(tab_1, tab_2, by="Team")

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h<-read_html(url)
tab <- html_nodes(h, "table")
html_table(tab[[6]], fill=TRUE)

# ---- String processing -----

library(tidyverse)

# Difference between using " and `
s<-'10"'
cat(s) # cat() lets us see the string

murders # From previous video
class(murders$population)
murders$population[1:3]
as.numeric(murders$population)

# detect whether there are commas
commas <- function(x) any(str_detect(x, ",")) # str_detect finds y in x, any() at least 1 value is true
murders %>% summarize_all(funs(commas))
# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders$population, ",", "") # 
test_1 <- as.numeric(test_1)
# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders$population)
identical(test_1, test_2)

murders_new<-murders %>% mutate_at(2:3, parse_number) # Mutate columns 2 and 3 to take away commas
head(murders_new)

data("reported_heights")

reported_heights %>%
  mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height))%>%
  head(n=10)
# In order to convert everything to inches, we get rid of the right entries and keep the problematic ones
not_inches <- function (x, smallest = 50, tallest = 84){ # Create a function
  inches <- suppressWarnings(as.numeric(x)) # Transform to numeric, ignore warnings
  ind <- is.na(inches) | inches < smallest | inches > tallest # Keep values that are NAs or weird measures
  ind
}
problems <- reported_heights %>% # Apply function to data
  filter(not_inches(height)) %>%
  .$height
length(problems) # How many problematic entries do we have
# There are three big groups that follow the following patterns
# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat
# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

str_which(reported_heights$height, "cm")
yes<-c("180cm", "70 inches")
no<-c("180", "70''")
s<-c(yes, no)
str_detect(s, "cm")|str_detect(s, "inches")
str_detect(s, "cm|inches")

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no<-c("", ".", "Five", "six", " 1")
s<-c(yes, no)
pattern<-"\d"
str_detect(s, pattern)
str_view_all(s, pattern)
str_view(s, "[4-7]")
pattern<-"^\\d$"
str_view_all(s, pattern)
pattern <- "^\\d{1,2}$"
yes<-c("1","5","9","12")
no<-c("123", "a4", "b")
str_view(c(yes, no), pattern)
pattern<-"^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
str_match(s, pattern_with_groups)
str_match(s, pattern_without_groups)
str_extract(s, pattern_with_groups)
str_replace(s, pattern_with_groups, "\\1'\\2")

pattern_with_groups<-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") %>%
  head

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches)&
    ((inches >= smallest & inches <= tallest) | 
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}  
problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>%
  str_replace("feet|foot\ft", "'") %>% #Convert feet symbols to '
  str_replace("inches|in\''|\"", "") %>% #Remove inches symbols
  str_replace("^([4-7])'?$", "\\1'0")%>%
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") #Change format
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"
index<-str_detect(converted, pattern)
mean(index)

converted[!index]

animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern<-"moo*"
str_detect(animals, pattern)

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# There is a part missing in the other computer

s <- c("5'10", "6'1")
tab<-data.frame(x = s)
tab %>% separate(x, c("feet", "inches"), sep="'")
# Equivalent code using extract
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
# Why extract? Because regex gives us more flexibility
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right") # It fails, we only want numbers
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11)
    s <- str_replace_all(s, word(i), as.character(i))
  s
}
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>%
    str_replace_all("inches|in|''|\"|cm|and", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>%
    str_replace("^([56])'?$", "\\1'0") %>%
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
    str_trim()
}
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)

# That one does not work, need to put it together with the other computer

# String splitting
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()
x<-str_split(lines, ",")
x %>% head()
col_names<-x[[1]]
x<-x[-1]
library(purrr)
map(x, 1) %>% head()
# To create dataframe, use following code
dat <- data.frame(map_chr(x, 1),
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)
dat %>% head()
# Using purrr package, more simple
dat <- x %>%
  transpose() %>%
  map (~parse_guess(unlist(.))) %>%
  setNames(col_names)%>%
  as.data.frame()
dat%>%head()
# Even more simple
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x<-x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>% 
  mutate_all(parse_guess)
# Case study
library(dslabs)
data("research_funding_rates")
research_funding_rates 
# Raw data
data("raw_data_research_funding_rates")
# Examine object
raw_data_research_funding_rates %>% head()
#Create a list
tab <- str_split(raw_data_research_funding_rates, "\n")
tab
tab <- tab[[1]]
tab %>% head
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1
# Remove the leading space
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1
the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()
identical(research_funding_rates, new_research_funding_rates)

# Recoding
data("gapminder")
gapminder %>%
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color=country))+
  geom_line()
gapminder %>%
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country)
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# Case study
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)
polls
new_names<-c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
pattern<-"%"
polls<-polls %>%
  setNames(new_names) %>%
  filter(remain %in% lines)
polls

str_replace(polls$undecided, "N/A", "0")

temp <- str_extract_all(polls$dates, "\\d?\\s[a-zA-Z]?")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
# ---- Dates and times ----

# Epoch = reference date

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state=="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump))+ # Without us doing anything, months are displayed
  geom_line()

library(lubridate)
# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates
# Functions year, month and day extract those values
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))
month(dates, label = TRUE)    # extract month label
# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)
# Year (four digits, yyyy), month (two digits, mm), day (dd) = ISO 8501 format = YYYY-MM-DD
# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

Sys.time()
Sys.timezone()
now("GMT")
OlsonNames() #List all available timezones

#Case study: Trump tweets
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)
data("trump_tweets")
head(trump_tweets)
names(trump_tweets)
trump_tweets %>% count(source) %>% head()
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
library(tidytext)
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))%>%
  head()
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)
library(textdata)
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)
nrc
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts
tweet_words %>% group_by(source) %>% summarize(n = n())
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#Assessment
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits
data(brexit_polls)
brexit_polls%>%
  round_date(enddate, unit = "week")
sum(ifelse(round_date(brexit_polls$enddate, unit="week")=="2016-06-12", 1, 0))

brexit_polls %>%
  mutate(day = weekdays(enddate)) %>%
  ggplot(aes(x=day))+
  geom_bar()

data("movielens")
head(movielens)

movielens %>%
  mutate(timestamp = as_datetime(timestamp),
         year = year(timestamp),
         hour = hour(timestamp))%>%
  ggplot(aes(x=hour))+
  geom_bar()

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
data<-gutenberg_metadata

gutenberg_works(gutenberg_metadata$title=="Pride and Prejudice")
text<-gutenberg_download("1342", mirror = "http://gutenberg.readingroo.ms/")
words <- text %>% unnest_tokens(word, text) %>% select(word)
pattern<-c("w","o", "r", "d", "s")
words<-words%>%
  filter(!word %in% stop_words$word)%>%
  mutate(x = ifelse(str_detect(word, pattern = "\\d"), 1, 0))%>%
  filter(x == 0) 
afinn_sentiments<-inner_join(afinn, words, by="word")

afinn_sentiments %>%
  filter(value==4)

# ---- Final assignment ----
library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

txt<-pdftools::pdf_text(fn)

x<-str_split(txt[9], pattern = "\n")
s<-x[[1]]
class(s)

s<-s%>%
  str_trim() %>%
  str_replace_all(",\\s.", "")
s
x

str_which(s, "2015")
header_index<-3
header<-s[header_index]
a<-str_split(header, "\\s+", simplify = TRUE)
month<-a[1,1]
header<-a[1,2:5]

tail_index <- 36
n<-str_count(s, "\\d+")

s<-s[-c(1:header_index, 7, 10, tail_index:41)]
s
s<-str_remove_all(s, "[^\\d\\s]")
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s
b<-as.data.frame(s)
colnames(b)<-c("day", "2015", "2016", "2017", "2018")
tab <- b %>%
  mutate(day = as.numeric(day),
         `2015`=as.numeric(`2015`),
         `2016`=as.numeric(`2016`),
         `2017`=as.numeric(`2017`),
         `2018`=as.numeric(`2018`))
tab
mean(tab$`2017`[20:30])

tab <- tab %>% gather (year, deaths, -day)
tab %>%
  filter (year != "2018") %>%
  ggplot(aes(x=day, y=deaths, color=year)) +
  geom_line() +
  geom_vline(xintercept = 20)
