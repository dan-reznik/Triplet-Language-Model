library(tidyverse, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
#library(DBI)
library(odbc)
library(dbplyr)
library(stringr)
library(stringi)
library(magrittr)
library(forcats)
library(scales)
library(rstudioapi)
library(openxlsx)
# for write.xlsx "zip" operation
if(devtools::find_rtools())
  Sys.setenv(R_ZIPCMD= file.path(devtools:::get_rtools_path(),"zip"))
# library(ggthemes)
# library(psych)
# library(gridExtra)
# library(grid)
# library(plotly)
# library(ggrepel)

safeRatio <- function(a,b) if_else(b==0L,0,a/b)
# distribution mode
getMode <- function(v) names(sort(table(v),decreasing=T)[1])
upperWhisker <- function(v) {
  n5 <- fivenum(v)
  uh <- n5[4]
  lh <- n5[2]
  iqr <- uh-lh
  uh+1.5*iqr
}
lowerWhisker <- function(v) {
  n5 <- fivenum(v)
  uh <- n5[4]
  lh <- n5[2]
  iqr <- uh-lh
  lh-1.5*iqr
}
# outlier range
getWhiskers <- function(v) {
  n5 <- fivenum(v)
  uh <- n5[4]
  lh <- n5[2]
  iqr <- uh-lh
  c(lh-1.5*iqr,uh+1.5*iqr)
}
# get values of factor that appear at least nmin times in v
getFreqMin <- function(v,nmin) {
  t <- table(v)
  names(t)[t>nmin]
}

objSize <- function(obj,units="Gb") format(object.size(obj),units=units) 
longDateTime <- col_datetime(format="%d/%m/%Y %H:%M:%S")
longDate <- col_date(format="%d/%m/%Y %H:%M:%S")
shortDate <- col_date(format="%d/%m/%Y")
shortDateUS <- col_date(format="%m/%d/%Y")
lunique<-function(l) length(unique(l))
interu3 <- function(l1,l2,l3) intersect(intersect(l1,l2),l3)
linteru <- function(l1,l2) length(intersect(l1,l2))

luniqueReport <- function(l1,l2) {
  u1 = unique(l1)
  u2 = unique(l2)
  c(length(u1),length(u2),length(intersect(u1,u2)))
}

luniqueReport3 <- function(l1,l2,l3) {
  u1 = unique(l1)
  u2 = unique(l2)
  u3 = unique(l3)
  c(length(u1),length(u2),length(u3),
    length(intersect(intersect(u1,u2),u3)))
}

ppipe <- function(l) paste0(l,collapse="|")
ppipeS <- function(l) ppipe(sort(l))
ppipeSu <- function(l) ppipeS(unique(l))
unpipe <- function(s) unlist(str_split(s,"\\|"))
lpipe <- function(s) map_int(s,~length(unpipe(.x)))
pipeSorted <- function(s,fn) {
  vals <- fn(unpipe(s))
  all(vals == sort(vals))
}
pastePipes <- function(p1,p2) ppipe(map2_chr(p1,p2,~paste(.x,.y,sep="")))

inRange <- function(v,vmin,vmax) (v>=vmin)&(v<=vmax)


#var0,theLab="",bw=50,showDens=T) {

freqTable <- function(df,col,nmax=0) {
  #arguments <- as.list(match.call())
  #col <- eval(arguments$col0, df)
  dist <- n_distinct(df[[col]])
  tn <- as.integer(ifelse(nmax<1,dist,min(dist,nmax)))
  df %>%
    count_(col,sort=T) %>%
    head(tn) %>%
    mutate(pct=.$n/nrow(df)) # %>%  select(pct,n,everything())
}

#Saves a spreadsheet where each sheet contains reverse frequency tables

freqXLSX <- function(df,nmax=0) {
  wb <- createWorkbook("sd")
  cn <- strPrep(colnames(df))
  df %<>% set_colnames(cn)
  walk(cn,~{addWorksheet(wb,.x);writeData(wb,.x,freqTable(df,.x,nmax))})
  saveWorkbook(wb,choose.files("*.xlsx"),overwrite=T)
}
  
  #function(x) data.frame(prop.table(table(x))) %>%
  #arrange(desc(Freq)) %>%
  #mutate(vars=as.character(x),Freq=round(Freq,2)) %>% select(-x)

intervalType <- function(Amin,Amax,Bmin,Bmax) {
  if (Amin<Bmin) {
    if(Amax<Bmin) return("A<B")
    if(Amax>Bmax) return("BinA")
    return("A<~B")
  } else {
    if(Amin>Bmax) return("B<A")
    if(Amax<Bmax) return("AinB")
    return("B<~A")
  }
}

myFirst <- function(l,fn) fn(unpipe(l)[1])
myLast <- function(l,fn) fn(rev(unpipe(l))[1])

myFirstDate <- function(l) myFirst(l,as.Date)
myLastDate <- function(l) myLast(l,as.Date)
myFirstInt <- function(l) myFirst(l,as.integer)
myLastInt <- function(l) myLast(l,as.integer)

myFirstDateAfter <- function(baseDate,l) {
  dts <- as.Date(unpipe(l))
  keep(dts, ~as.integer(.x-baseDate)>=0)[1]
}

myFirstDateBefore <- function(baseDate,l) {
  dts <- as.Date(unpipe(l))
  # return last occurrence of a negative
  rev(keep(dts, ~as.integer(baseDate-.x)>=0))[1]
}

getFirstLastDays <- function(l) {
  theMin=myFirstDate(l)
  theMax=myLastDate(l)
  theDays=as.integer(theMax-theMin)
  list(min=theMin,max=theMax,days=theDays)
}

bp.n <- function(x) c(y = fivenum(x)[4]+16, label = length(x)) 
bp.med <- function(x) c(y = fivenum(x)[2]-16, label = round(median(x),2))

removeAccents <- function(s) iconv(s,
                                   from="UTF-8",
                                   to='ASCII//TRANSLIT')
strPrep <- function(s,replace_non_words=T,removeAcc=T) { 
  if(removeAcc)
    s <- removeAccents(s)
  if(replace_non_words)
    s <- str_replace_all(s,"(\\W|_)+"," ")
  s <- str_to_lower(str_trim(s,side="both"))
  s
}

strPrepC <- compiler::cmpfun(strPrep)

getFmt <- function(s,remAcc=T) {
    if(remAcc) s <- removeAccents(s)
    s <- ifelse(is.na(s),NA,str_replace_all(s, "\\d", "9"))
    s <- ifelse(is.na(s),NA,str_replace_all(s, "[a-zA-Z]","a"))
    s
}

getFmt2 <- function(s,remAcc=T) {
    s <- getFmt(s,remAcc)
    s <- ifelse(is.na(s),NA,str_replace_all(s, "\\d+", "9"))
    s <- ifelse(is.na(s),NA,str_replace_all(s, "[a-zA-Z]+","A"))
    s
}

reportRle <- function(vals,theColl="|")  {
  r <- rle(vals)
  r <- map2_chr(r$lengths,r$values,~paste0(.y,"=",.x))
  paste0(r,collapse=theColl)
}

capFirst <- function(s) if_else(str_length(s)==0,"",
                                if_else(str_length(s)==1,
                                        str_to_upper(s),
                                        paste0(str_to_upper(str_sub(s,1,1)),
                                               str_sub(s,start=2L),collapse="")))
isInt <- function(s) str_detect(s, "^\\d+$")
isOrd <- function(s) str_detect(s, "^\\d{1,2}[oa]")
stopWords <- c("ao","aos","as","com","da","das","de","dia","do","dois",
               "dos","duas","em","ja","mais","mas","na","nas","no","nos","os",
               "ou","para","pela","pelas","pelo","pelos","pois","por",
               "quais","qual","quatro","que","se","sobre",
               "tres","um","uma","uso","via","foi","sem","com",
               "seu","seus","sua","suas","dele","deles","dela","delas",
               "apos","antes","depois","como","porem","mesmo","mesma",
               "mesmos","mesmas","ate","mais","menos","tem","ha",
               "sim","nao","pro","pra")
validProcWord <- compiler::cmpfun(function(word) str_length(word)>1 & !isInt(word) & !isOrd(word) & !(word %in% stopWords))

# converts a text column of a data frame into a single char array of tokens
getTokens <- function(df,col,coll=".",splitBy=".") unlist(str_split(paste0(df[[col]],collapse=coll),fixed(splitBy)))

#Function to locate a value on a first pipe, and obtain the corresponding
#entry on a second pipe. Vectorized over pipeMatch and pipeFrom only.

locateInPipe <- function(matchVal,pipeMatch,pipeFrom,def=NA) {
  map2_chr(pipeMatch,pipeFrom,
           function(x,y) {
             unpMatch <- unpipe(x)
             unpFrom <- unpipe(y)
             # locates last (latest if pipeFrom is date-ordered) matching event
             # was: match(matchVal,unpMatch)
             i <- last(which(matchVal==unpMatch)) 
             ifelse(is.na(i),NA,
                    ifelse(between(i,1,length(unpFrom)),
                           nth(unpFrom,i,default=def),def))
           }
  )
}

# when all are lists

locateInPipe3 <- function(matchVals,pipeMatch,pipeFrom,def=NA) {
  pmap_chr(list(matchVals,pipeMatch,pipeFrom),
           function(m,x,y) {
             unpMatch <- unpipe(x)
             unpFrom <- unpipe(y)
             # locates last (latest if pipeFrom is date-ordered) matching event
             # was: match(matchVal,unpMatch)
             i <- last(which(m==unpMatch)) 
             ifelse(is.na(i),NA,
                    ifelse(between(i,1,length(unpFrom)),
                           nth(unpFrom,i,default=def),def))
           }
  )
}

getNAs <- function(df) summarise_all(df, function(x) sum(is.na(x))) %>%
  gather(var,naCount,everything()) %>%
  filter(naCount>0) %>% mutate(naPct=round(naCount/nrow(df),2))

# color blind paletter: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c(gray="#999999",
               orange="#E69F00",
               cyan="#56B4E9",
               green="#009E73",
               yellow="#F0E442",
               blue="#0072B2",
               red="#D55E00",
               pink="#CC79A7")
getYQ <- function(dt) sprintf("%4d-%1d",year(dt),quarter(dt))
getYM <- function(dt) sprintf("%4d-%02d",year(dt),month(dt))