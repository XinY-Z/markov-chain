source('preprocessors.R')
dt <- fread(r'(C:\Users\XinZ\Box\DATA\Xin_daap\miscglobalsmerged_daap_session_033122.csv)')


## set options
TOPIC <- 'REC'   # focus on code REC
CONCAT <- F       # consider therapist codes that co-occur or not
CONDITIONON <- 'T'# consider on whom to condition
STARTWITH <- 'P'  # consider only transition from client to therapist
#' @note `CONDITIONON` and `STARTWITH` determines the direction of condition \
#' e.g., CONDITIONON=T & STARTWITH=P means looking at P -> T sequences, but \
#' conditioned on T.


## initiate empty table for transition sequences
dic <- data.table()
session <- unique(dt[, session])
dic <- cbind(dic, session)

seqNames <- createDic(dt, TOPIC, STARTWITH, CONCAT)
dic2 <- data.table(matrix(nrow=0, ncol=length(seqNames)))
colnames(dic2) <- seqNames

dic <- cbind(dic, dic2)
dic[is.na(dic)] <- 0
rm(dic2)

dic[, N := 0]

## create a function that calcualtes and assigns transition probabilities to the empty table above
assignProb <- function(DT, dictionary, startWith, topic, concat) {
  sessionNum <- DT[, unique(session)]
  input <- DT[, .SD, .SDcols=!'session']
  
  freq <- countSequence(input, startWith, concat)
  freq <- focusTopic(freq, topic)
  props <- prop.table(freq)
  totalFreq <- sum(freq)
  for(i in 1:length(props)) {
    if(totalFreq!=0) {
      oriValue <- dictionary[session==sessionNum, get(names(props[i]))]
      dictionary[session==sessionNum, names(props)[i] := oriValue + props[i]]
    }
  }
  dictionary[session==sessionNum, N := totalFreq]
}

## fill out the empty table
for(i in unique(dt$session)) {
  sel <- dtPreprocess(dt[session==i])
  assignProb(sel, dic, STARTWITH, TOPIC, CONCAT)
}

## session 8940 did not talk about <TOPIC>, delete it
dic <- dic[N!=0]

## combine the filled-out table with original data table
dt <- dic[dt, on='session']

#fwrite(dt, r'(C:\Users\XinZ\Box\DATA\Xin_daap\miscglobalsmerged_daap_010123.csv)')
