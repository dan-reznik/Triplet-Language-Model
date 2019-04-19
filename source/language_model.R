library(data.table)

get_chapt_words <- function(df) df %>%
  select(book,chapt_num,content) %>%
  mutate(content=content %>%
           str_replace_all("([^[:alnum:]'])"," \\1 ") %>%
           str_squish %>% str_to_lower(),
         word=content %>% str_split(fixed(" "))) %>%
  select(-content) %>%
  unnest(word) %>%
  filter(str_length(word)>0)

get_punct <- function(df) df %>%
  filter(word%>%str_detect("[^[:alnum:]']")) %>%
  count(word,sort=T)

fix_punct <- function(df) df %>%
  filter(word%>%str_detect("[[:alnum:],:;!\\-'\\.\\?]")) %>%
  mutate(word=if_else(word %in% c(";","!"), ".", word))

get_passage <- function(df,ch_num,snippet,window=50) df %>%
  filter(chapt_num==ch_num) %>%
  pull(content) %>%
  str_extract(sprintf(".{%d}%s.{%d}",window,snippet,window))

get_chapt_triplets <- function(df) df %>%
  mutate(word=pmap_chr(list(word,lead(word),lead(word,2)),
                       function(a,b,c)str_c(a,b,c,sep="|"))) %>%
  separate(word,into=c("w1","w2","w3"),sep=fixed("\\|")) %>%
  filter(!is.na(w1),w1!="",
         !is.na(w2),w2!="",
         !is.na(w3),w3!="") %>%
  #,!str_detect(w1,"[:punct:]")) %>%
  count(w1,w2,w3,sort=T) %>%
  mutate(triplet_number=row_number()) %>%
  arrange(w1,w2,desc(n))

chapt_doublets <- function(triplets,min_length=2) triplets %>%
  filter(str_length(w1)>min_length,str_length(w2)>min_length) %>%
  group_by(w1,w2) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  ungroup()

get_triplet_probs <- function(df) df %>%
  #head(200) %>%
  rename(count=n) %>%
  group_by(w1,w2) %>%
  summarise(w3=list(w3),
            w3_count=list(count),
            w3_prob=list(count/sum(count))) %>%
  ungroup %>%
  unnest(w3,w3_count,w3_prob) %>%
  as.data.table(key=c("w1","w2")) # for fast lookups

# df has (book,chapt_num,content). name = "camões.rds","rabelais.rds", etc.
get_triplets_script <- function(df,name) { 
  chapt_words <- df %>% get_chapt_words
  chapt_words_fix_punct <- chapt_words %>% fix_punct
  chapt_triplets <- chapt_words_fix_punct %>% get_chapt_triplets
  chapt_triplet_probs <- chapt_triplets %>% get_triplet_probs
  print(sprintf("size: %s", object.size(chapt_triplet_probs) %>% format(units="MB")))
  chapt_triplet_probs %>% saveRDS(name)
  chapt_triplet_probs
}

get_filt <- function(dt,my_w1,my_w2) dt[w1==my_w1&w2==my_w2]

# retrieves a probable w3 given w1,w2
get_w3 <- function(dt,my_w1,my_w2) {
  if(length(my_w2)==0) print(glue::glue("w1={my_w1},w2={my_w2}"))
  df_filt <- get_filt(dt,my_w1,my_w2) %>%
    as_data_frame %>%
    arrange(w3_prob) %>%
    mutate(w3_prob_cum=cumsum(w3_prob))
  
  r <- runif(1)
  # simulate unfair die: get first prob to be above r
  first_prob <- detect_index(df_filt$w3_prob_cum,~.x>=r)
  df_filt$w3[first_prob]
}

generate_utterance <- function(dt,w1,w2,n=50) {
  words <-vector("list", n+2)
  words[[1]] <- w1
  words[[2]] <- w2
  i <- 2
  while(i<n) {
    w3 <- get_w3(dt,w1,w2)
    if(length(w3)==0) break
    i <- i + 1
    words[[i]] <- w3
    w1 <- w2
    w2 <- w3
  }
  print(i)
  map_chr(words[1:i],1) %>%
    str_c(collapse=" ") %>%
    str_replace_all("\\s+([:punct:])","\\1") %>%
    str_replace_all("-\\s(?=[:alpha:])","-")
}

# devtools::install_github("dgrtwo/drlib")
# generate_utterance <- function(dt,w1,w2,n) {
#   drlib::accumulate_while(.x=c(w1,w2),
#                           .f=~c(.x[2],do.call(get_w3,c(list(dt),.x))),
#                           .p=~length(.x)==2,
#                           #.compare=T,
#                           .max=n) %>%
#     head_while(~length(.x)==2) %>%
#     map_chr(2) %>%
#     prepend(w1) %>%
#     str_c(collapse=" ") %>%
#     str_replace_all("\\s+([:punct:])","\\1")
# }