
rankhospital <- function(state, outcome, num = "best"){
      
      if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
            stop("invalid outcome")
      }
      
      df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      if (!(state %in% df$State)) {
            stop("invalid state")
      }
      
      if (outcome == 'heart attack') {col_id = 11}
      if (outcome == 'heart failure') {col_id = 17}
      if (outcome == 'pneumonia') {col_id = 23}
      
      df_sta <- subset(df, df$State == state)
      
      df_state <- subset(df_sta,!is.na(as.numeric(df_sta[,col_id])))
      
      hosp_rank <- df_state[,2][order(as.numeric(df_state[,col_id]), df_state[,2])]
      
      if (num == 'best'){num = 1}
      if (num == 'worst'){num = length(hosp_rank)}
      
      return(hosp_rank[num])
      
}