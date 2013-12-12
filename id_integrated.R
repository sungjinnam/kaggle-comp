library(multicore)
id_integrated <- data.frame(id = raw.id.user.map$User)

freqScanner <- function(sourceData, sourceDataIdCol, idPrefix, matchedData, matchedDataIdCol){
	#ex. subset of lending from id_map. (to reduce the cost)
	temp.subset <- subset(matchedData, matchedData[[matchedDataIdCol]] %in% sapply(sourceData[[sourceDataIdCol]], function(x){paste0(idPrefix, x, collapse='')}))
	#ex. return index of id_map which matched with the subset of lending. exex. id1 -> appear in 26 different place in the subset
	temp.vec.match <- match(temp.subset[[matchedDataIdCol]], sapply(sourceData[[sourceDataIdCol]], function(x){paste0(idPrefix, x, collapse='')}))
	#ex. count number of lending per id from id_map
	temp.return <- sapply(c(1:nrow(sourceData)), function(x){length(which(temp.vec.match==x))})
	return(temp.return)
}

sumScanner <- function(sourceData, sourceDataIdCol, idPrefix, matchedData, matchedDataIdCol){
	#ex. subset of lending from id_map. (to reduce the cost)
	temp.subset <- subset(matchedData, matchedData[[matchedDataIdCol]] %in% sapply(sourceData[[sourceDataIdCol]], function(x){paste0(idPrefix, x, collapse='')}))
	#ex. return index of id_map which matched with the subset of lending. exex. id1 -> appear in 26 different place in the subset
	temp.vec.match <- match(temp.subset[[matchedDataIdCol]], sapply(sourceData[[sourceDataIdCol]], function(x){paste0(idPrefix, x, collapse='')}))
	#ex. count number of lending per id from id_map
	# temp.return <- sapply(c(1:nrow(sourceData)), function(x){length(which(temp.vec.match==x))})

	# temp.return <- tapply(temp.subset$loan_id, temp.subset$lender_id, function(x){
	# 	sum(raw.loans$funded_amount[raw.loans$loan_id %in% x])
	# 	})

	temp.return <- sapply(c(1:nrow(sourceData)), function(x){
		temp.loaners <- temp.subset$loan_id[which(temp.vec.match==x)]
		return(sum(raw.loans$funded_amount[raw.loans$loan_id %in% temp.loaners]))
			})	
	return(temp.return)
}

id_integrated$freq_lending 		<- freqScanner(id_integrated, 'id', 'l_', raw.lending, 'lender_id')
# id_integrated$freq_borrowing 	<- freqScanner(id_integrated, 'id', 'b_', raw.lending, 'loan_id')
id_integrated$freq_team			<- freqScanner(id_integrated, 'id', 'l_', raw.team.join, 'lender_id')
id_integrated$sum_lending 		<- sumScanner(id_integrated, 'id', 'l_', raw.lending, 'lender_id')
id_integrated$list_team			<- 		  mclapply(id_integrated$id, function(x){raw.team.join$team_id[raw.team.join$lender_id==paste0('l_', x, collapse='')]})
id_integrated$mean_team_members	<- unlist(mclapply(id_integrated$list_team, function(x){mean(raw.teams$number_of_members[raw.teams$team_id %in% unlist(x)], na.rm=TRUE)}))
id_integrated$mean_team_cout	<- unlist(mclapply(id_integrated$list_team, function(x){mean(raw.teams$loan_cout[raw.teams$team_id %in% unlist(x)], na.rm=TRUE)}))
id_integrated$mean_team_amount	<- unlist(mclapply(id_integrated$list_team, function(x){mean(raw.teams$loaned_amount[raw.teams$team_id %in% unlist(x)], na.rm=TRUE)}))
id_integrated$country_code		<- raw.lenders$country_code[match(sapply(id_integrated$id, function(x){paste0('l_', x, collapse='')}), raw.lenders$lender_id)]

id_integrated$freq_team_z <- sapply(id_integrated$freq_team, function(x){(x-mean(id_integrated$freq_team, na.rm=T))/sd(id_integrated$freq_team, na.rm=T)})
id_integrated$sum_lending_z <- sapply(id_integrated$sum_lending, function(x){(x-mean(id_integrated$sum_lending, na.rm=T))/sd(id_integrated$sum_lending, na.rm=T)})
id_integrated$freq_lending_z <- sapply(id_integrated$freq_lending, function(x){(x-mean(id_integrated$freq_lending, na.rm=T))/sd(id_integrated$freq_lending, na.rm=T)})
id_integrated$freq_borrowing_z <- sapply(id_integrated$freq_borrowing, function(x){(x-mean(id_integrated$freq_borrowing, na.rm=T))/sd(id_integrated$freq_borrowing, na.rm=T)})
# unlist(
# 	mclapply(id_integrated$list_team, 

# 		function(x){mean(raw.teams$loaned_amount[raw.teams$team_id %in% unlist(x)], na.rm=TRUE)}))

# sapply(
# 	sapply(id_integrated$id, function(x){paste0('l_', x, collapse='')}),
# 	function(x){x}
# 	)

# sumScanner <- function(sourceData, sourceDataIdCol, idPrefix, matchedData, matchedDataIdCol){
# 	# temp.subset <- subset(raw.lending, lender_id %in% sapply(id_integrated$id, function(x){paste0(idPrefix, x, collapse='')}))
# 	temp.id <- sapply(sourceData[[sourceDataIdCol]], function(x){paste0(idPrefix, x, collapse='')})
# 	temp.subset <- subset(matchedData, matchedData[[matchedDataIdCol]] %in% temp.id)
# 	# temp.return <- tapply(temp.subset$loan_id, temp.subset$lender_id, function(x){
# 	# 	sum(raw.loans$funded_amount[raw.loans$loan_id %in% x], na.rm=T)
# 	# 	})
# 	temp.return <- sapply(temp.id, function(x){
# 		sum(raw.loans$funded_amount[raw.loans$loan_id %in% raw.lending$loan_id[raw.lending$lender_id == x]])
# 		})

# 	return(temp.return)
# }

# tt.3 <- sumScanner(id_integrated, 'id', 'l_', raw.lending, 'lender_id')

# tt.loans <- subset(raw.loans, loan_id %in% 
#          raw.lending$loan_id[raw.lending$lender_id == paste0('l_', id_integrated$id[1], collapse='')])

# tt.fundAmt <- mclapply(id_integrated$id, function(x){
#          raw.loans$funded_amount[raw.loans$loan_id %in% raw.lending$loan_id[raw.lending$lender_id == paste0('l_', x, collapse='')]]})
# id_integrated$mean_loan_amount <- sapply(tt.fundAmt, function(x)mean(x, na.rm=T))



# id_teams <- tapply(raw.team.join$team_id, raw.team.join$lender_id, list)
