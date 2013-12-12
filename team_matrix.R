temp.procTime <- proc.time()

source('team_matrix_function.R')

# for no precious team ids.
# matrix.team$Teams[which(sapply(id_integrated$list_team, length) == 0)] <- teamList(id_integrated$list_team[3])

matrix.team <- data.frame(Id=1:nrow(id_integrated))
# matrix.team$Teams <- sapply(mclapply(id_integrated$list_team, function(x){teamList(x)}), as.vector)
matrix.team$Teams <- sapply(mclapply(id_integrated$id, function(x){teamList(x)}), as.vector)
# sapply(id_integrated$id[1:10], function(x){teamList(x)})


# raw.teams[raw.teams$team_id %in% names(temp.tableTeam[100:120]), c('number_of_members', 'loan_cout' ,'loaned_amount', 'mean_loan')]
# raw.teams[raw.teams$team_id %in% names(temp.tableTeam[1:20]), c('loaned_amount_perLoan', 'loan_cout_perPerson', 'loaned_amount_perPerson')]
# raw.teams[raw.teams$team_id %in% names(temp.tableTeam[100:120]), c('loaned_amount_perLoan', 'loan_cout_perPerson', 'loaned_amount_perPerson')]


#detect country code
# sapply(tolower(c('United States','USA')), function(x)grep(x, tolower(raw.teams$location[1:60])))
# raw.teams$countryCode <- sapply(toupper(c('United States', 'USA')), function(x){
# 	gsub(x, 'US', toupper(raw.teams$location[1:60]))})
# gsub("([\w\W]*)((UNITED STATES)+|(USA)+)", 'US', toupper(raw.teams$location[1:60]))

# idx.us <- grep(
# 	paste("[\\W](UNITED STATES|USA|US)", 
# 		paste0('(', paste(raw.stateCode$Code, collapse='|'), ')', collapse=''), 
# 		paste0('(', paste(raw.stateCode$State, collapse='|'), ')', collapse=''), sep='|')
# 	, toupper(raw.teams$location[1:60]))
# regexp.us <-paste("((UNITED STATES)|(USA)|(US))", 
# 		paste0('((, )', paste0('(', raw.stateCode$Code, ')', collapse='|'), ')', collapse=''), sep='|')


# sapply(toupper(raw.teams$location), function(x){ifelse(grep("((UNITED STATES)|(USA)|(US))", x), 'US', NA)}


write.csv(matrix.team, paste('output/output', Sys.time(), '.csv', collapse=''), row.names=FALSE)



proc.time() - temp.procTime


