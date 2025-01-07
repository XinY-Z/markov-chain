library(stringr)    # process strings
library(data.table)

dt <- fread(r'(C:\Users\XinZ\Box\DATA\Projects\xin_temp\daap\miscglobalsmerged_daap_session_033122.csv)')

## proprocess dataset ####
## split concatenated behavioral codes
dtPreprocess <- function(DT) {
  splits <- as.data.table(DT[, str_split(code, ',', simplify = T)])
  names(splits) <- paste0('code', 1:ncol(splits))
  combined <- cbind(DT, splits)
  sel <- combined[, .SD, .SDcols=c('session', 'speaker', names(splits))]
  return(sel)
}


## sequence extraction ####
## concatenate every two codes with '_'
combineCode <- function(codes) {
  if(length(codes)==1) {
    temps <- codes
  }
  else {
    temps <- c()
    for(i in 1:length(codes)) {
      for(j in 1:length(codes)) {
        if(j>i) {
          temp <- paste(codes[i], codes[j], sep='_')
          temps <- c(temps, temp)
        }
      }
    }
  }
  return(temps)
}

concatCode <- function(codes, concat) {
  sorted <- str_subset(str_sort(unlist(codes)), '.+')
  if(concat) {
    temps <- combineCode(sorted)  
  }
  else temps <- sorted
  return(temps)
}

## define how to identify code sequence of two
extractSequence <- function(unit, concat) {
  temps <- c()
  row1 <- concatCode(unit[1], concat)
  row2 <- concatCode(unit[2], concat)
  for(i in 1:length(row1)) {
    for(j in 1:length(row2)) {
      temp <- paste0(row1[i], '2', row2[j])
      temps <- c(temps, temp)
    }
  }
  return(temps)
}

## identify sequences of codes in a session
mapSequence <- function(input, startWith, concat) {
  temps <- c()
  for(i in 1:(nrow(input)-2)) {
    if(input[i, speaker] == startWith) {
      unit <- input[i:(i+1), .SD, .SDcols=!'speaker']
      temp <- extractSequence(unit, concat)
      temps <- c(temps, temp)
    }
  }
  return(temps)
}

## count the transition sequences
countSequence <- function(input, startWith, concat=T) {
  table(mapSequence(input, startWith, concat))
}

## if focusing on one topic for a speaker
focusTopic <- function(tab, topic) {
  ## define the string pattern based on the topic to be replaced
  strPattern <- sprintf('([:alpha:]+_)?%s(_[:alpha:]+)?', topic)
  
  ## select the transition sequences that involve the topic
  sel <- tab[str_detect(names(tab), topic)]
  
  ## standardize the topic label
  names(sel) <- str_replace_all(names(sel), strPattern, topic)
  
  return(sel)
}

## create a dictionary
createDic <- function(DT, topic, startWith, concat=T) {
  ## collect all therapist codes
  thCodes <- unique(
    concatCode(
      dtPreprocess(DT)[speaker=='T', .SD, .SDcols=!c('session', 'speaker')],
      concat=F
    )
  )
  ## collect all client codes
  ctCodes <- unique(
    concatCode(
      dtPreprocess(DT)[speaker=='P', .SD, .SDcols=!c('session', 'speaker')],
      concat=F
    )
  )
  
  ## if assessing co-occurence of codes, also add these codes to dictionary
  if(concat) {
    thCodes <- c(thCodes, combineCode(thCodes))
    ctCodes <- c(ctCodes, combineCode(ctCodes))
  }
  
  ## create dictionary that aligns to how we describe sequences
  outputs <- c()
  if(topic %in% ctCodes) {
    for(i in 1:length(thCodes)) {
      output <- if(startWith=='P') paste0(topic, '2', thCodes[i]) else paste0(thCodes[i], '2', topic)
      outputs <- c(outputs, output)
    }
  }
  else if(topic %in% thCodes) {
    for(i in 1:length(ctCodes)) {
      output <- if(startWith=='P') paste0(ctCodes[i], '2', topic) else paste0(topic, '2', ctCodes[i])
      outputs <- c(outputs, output)
    }
  }
  return(outputs)
}


## check ####
## select sessions for testing
sel <- dt[ccaps_di < quantile(ccaps_di, .15, na.rm=T)][session==sample(session, 1)]
sel <- dtPreprocess(sel, 'code')

## count identified sequences of codes
# temp <- countSequence(
#   sel[, .SD, .SDcols=!'session'], 
#   startWith='T', concat=F
# )
# temp[order(-temp)][1:10]
# 
# ## focus on particular topic
# focusTopic(temp, topic='MOOD')
# focusTopic(temp, topic='REC')
# 
# ## create a dictionary
# createDic(dt, topic='MOOD', startWith='P', concat=F)
