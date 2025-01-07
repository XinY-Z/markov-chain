# This script maps the pipeline to each session in a dataset

source('utils.R')


## set options
INPUT_PATH <- <path to your dataset>
OUTPUT_PATH <- <path to save your processed data>
SESSION_ID <- 'session'   # column name for session ID
TOPIC <- 'REC'            # focus on code REC
CONCAT <- F               # consider therapist codes that co-occur or not
STARTWITH <- 'P'          # consider only transition from client to therapist
# STARTWITH determines the direction of condition, e.g., STARTWITH=P means looking 
# at P -> T sequences.


## read data
dt <- fread(FILE_PATH)


## initiate empty table for transition sequences
dic <- data.table()
session <- unique(dt[, SESSION_ID, with=F])
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
  sessionNum <- unique(DT[, SESSION_ID, with=F])
  input <- DT[, .SD, .SDcols=!SESSION_ID]
  
  freq <- countSequence(input, startWith, concat)
  freq <- focusTopic(freq, topic)
  props <- prop.table(freq)
  totalFreq <- sum(freq)
  for(i in 1:length(props)) {
    if(totalFreq!=0) {
      oriValue <- dictionary[get(SESSION_ID)==sessionNum, get(names(props[i]))]
      dictionary[get(SESSION_ID)==sessionNum, names(props)[i] := oriValue + props[i]]
    }
  }
  dictionary[get(SESSION_ID)==sessionNum, N := totalFreq]
}

## fill out the empty table
for(i in unique(dt[, SESSION_ID, with=F])) {
  sel <- dtPreprocess(dt[get(SESSION_ID)==i])
  assignProb(sel, dic, STARTWITH, TOPIC, CONCAT)
}

## combine the filled-out table with original data table
## This will attach the transition matrix back to your original dataset
dt <- dic[dt, on=SESSION_ID]

## export the file
fwrite(dt, OUTPUT_PATH)
