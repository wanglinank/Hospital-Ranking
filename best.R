best <- function(state, outcome) {
      
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
      
      df_state <- subset(df, df$State == state)
      
      num <- as.numeric(df_state[,col_id])
      
      lowest_risk <- min(num, na.rm = TRUE)
      
      best_hosp <- subset(df_state[,2], as.numeric(df_state[,col_id]) == lowest_risk)
      
      best_hosp_sorted <- sort(best_hosp)
      
      return(best_hosp_sorted[1])
      
}