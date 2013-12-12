library(multicore)
list.team <- levels(raw.team.join$team_id)

teamList <- function(x){
	#input arg -> id
	temp.initTeam <- unlist(id_integrated$list_team[id_integrated$id == x])

	# temp.initTeam <- unlist(x)
	#if user have previously joined team, get the most frequently selected team with the previous ones.
	if(length(temp.initTeam)){
		temp.listId <- raw.team.join$lender_id[raw.team.join$team_id %in% temp.initTeam]
		temp.listTeam <- raw.team.join$team_id[raw.team.join$lender_id %in% temp.listId]
		temp.tableTeam <- sort(table(temp.listTeam), decreasing=TRUE)
		temp.tableTeam <- temp.tableTeam[-which(names(temp.tableTeam) %in% temp.initTeam)]

		#selecting top10 cals, including same frequencies. -> filter by the order of more cout. 
		temp.candidates1 <- temp.tableTeam[temp.tableTeam > as.numeric(temp.tableTeam[5])]
		temp.candidates2 <- temp.tableTeam[temp.tableTeam <= as.numeric(temp.tableTeam[5]) & temp.tableTeam >= as.numeric(temp.tableTeam[15])]
		# temp.candidates2 <- temp.tableTeam
		temp.candidates2.subset <- raw.teams[raw.teams$team_id %in% names(temp.candidates2),]
		temp.return <- 
			c(names(temp.candidates1), 
			# c(
			as.character(temp.candidates2.subset$team_id[
				# order((
				# 	id_integrated$freq_team_z[id_integrated$id == x]
				# 	+ temp.candidates2.subset$loan_cout_z 
				# 	+ temp.candidates2.subset$loaned_amount_z 
				# 	+ temp.candidates2.subset$number_of_members_z), decreasing=T)
				# ]))
				order((
					(temp.candidates2.subset$loan_cout_z * (id_integrated$freq_lending_z[id_integrated$id == x] + 1))
					+ (temp.candidates2.subset$loaned_amount_z * (id_integrated$sum_lending_z[id_integrated$id == x] + 1))
					+ temp.candidates2.subset$number_of_members_z * (id_integrated$freq_borrowing_z[id_integrated$id == x] + 1)), decreasing=T)
				]))

		# #1step ahead
		# temp.initTeam <- c(as.character(temp.initTeam), names(temp.tableTeam[1:3]))
		# temp.listId <- raw.team.join$lender_id[raw.team.join$team_id %in% temp.initTeam]
		# temp.listTeam <- raw.team.join$team_id[raw.team.join$lender_id %in% temp.listId]
		# temp.tableTeam <- sort(table(temp.listTeam), decreasing=TRUE)
		# temp.tableTeam <- temp.tableTeam[-which(names(temp.tableTeam) %in% temp.initTeam[-(length(temp.initTeam)-2:length(temp.initTeam))])]

		# if((id_integrated$country_code[id_integrated$id == x] == 'US') & !is.na(id_integrated$country_code[id_integrated$id == x])){
		# 	# temp.openTeam.name.us <- temp.openTeam$team_id[temp.openTeam$countryCode == 'US']
		# 	temp.name.us <- subset(raw.teams, countryCode!='US')$team_id
		# 	temp.tableTeam <- temp.tableTeam[which(names(temp.tableTeam) %in% temp.name.us)]
		# }


		# temp.tableTeam <- temp.tableTeam[which(names(temp.tableTeam) %in% temp.listId)]
		# return(paste(names(temp.tableTeam[1:10]), collapse=' '))		
		return(paste(temp.return[1:10], collapse=' '))
		# return(paste(tail(temp.initTeam, 1), names(temp.tableTeam[1:9]), collapse=' '))		
	}
	#NOT IMPROVING - if user do not have any previously joined team, get the opened team with most number of members
	#if user do not have any preciously joined team, get the most frequently joined team by others, with open status.
	#NOT IMPROVING - if user location is US, get the most frequently joined team based in US 
	else{
		temp.openTeam <- raw.teams[raw.teams$membership_type.open.closed.=='open', c('team_id', 'countryCode')]
		temp.tableTeam <- sort(table(raw.team.join$team_id), decreasing=TRUE)
		
		# if((id_integrated$country_code[id_integrated$id == x] == 'US') & !is.na(id_integrated$country_code[id_integrated$id == x])){
		# 	# temp.openTeam.name.us <- temp.openTeam$team_id[temp.openTeam$countryCode == 'US']
		# 	temp.openTeam.name.us <- subset(temp.openTeam, countryCode=='US')$team_id
		# 	temp.tableTeam <- temp.tableTeam[which(names(temp.tableTeam) %in% temp.openTeam.name.us)]
		# }
		# # else{	
		# 	# temp.openTeam.MostMembers <- temp.openTeam$team_id[which(order(temp.openTeam$number_of_members) <= 10)]
		# 	# return(paste(temp.openTeam.MostMembers, collapse=' '))		

		# 	# temp.openTeam <- raw.teams$team_id[raw.teams$membership_type.open.closed.=='open']
		# 	# temp.tableTeam <- sort(table(raw.team.join$team_id), decreasing=TRUE)
		# 	temp.openTeam.name <- temp.openTeam$team_id
		# 	temp.tableTeam <- temp.tableTeam[which(names(temp.tableTeam) %in% temp.openTeam.name)]
		# 	# return(paste(names(temp.tableTeam[1:10]), collapse=' '))
		# }
		temp.openTeam.name <- temp.openTeam$team_id
		temp.tableTeam <- temp.tableTeam[which(names(temp.tableTeam) %in% temp.openTeam.name)]
		temp.candidates2.subset <- raw.teams[raw.teams$team_id %in% names(temp.tableTeam),]

		temp.candidates1 <- temp.tableTeam[temp.tableTeam > as.numeric(temp.tableTeam[5])]
		temp.candidates2 <- temp.tableTeam[temp.tableTeam <= as.numeric(temp.tableTeam[5]) & temp.tableTeam >= as.numeric(temp.tableTeam[15])]
		# temp.candidates2 <- temp.tableTeam
		temp.candidates2.subset <- raw.teams[raw.teams$team_id %in% names(temp.candidates2),]

		temp.return <- NULL
		if(is.na(id_integrated$country_code[id_integrated$id == x])){
			temp.return <- 
			c(names(temp.candidates1), 
				# c(
				as.character(temp.candidates2.subset$team_id[
					# order((
					# 	id_integrated$freq_team_z[id_integrated$id == x]
					# 	+ temp.candidates2.subset$loan_cout_z 
					# 	+ temp.candidates2.subset$loaned_amount_z 
					# 	- temp.candidates2.subset$number_of_members_z), decreasing=T)
					# ]))		
					order((
					(temp.candidates2.subset$loan_cout_z * (id_integrated$freq_lending_z[id_integrated$id == x] + 1))
					+ (temp.candidates2.subset$loaned_amount_z * (id_integrated$sum_lending_z[id_integrated$id == x] + 1))
					- temp.candidates2.subset$number_of_members_z) * (id_integrated$freq_borrowing_z[id_integrated$id == x] + 1), decreasing=T)
				]))
	
		}
		else{
			temp.return <- 
			c(names(temp.candidates1), 
				# c(
				as.character(temp.candidates2.subset$team_id[
					# order((
					# 	- id_integrated$freq_team_z[id_integrated$id == x]
					# 	- temp.candidates2.subset$loan_cout_z 
					# 	- temp.candidates2.subset$loaned_amount_z 
					# 	+ temp.candidates2.subset$number_of_members_z), decreasing=F)
					# ]))			
				order((
					(temp.candidates2.subset$loan_cout_z * (id_integrated$freq_lending_z[id_integrated$id == x] + 1))
					- (temp.candidates2.subset$loaned_amount_z * (id_integrated$sum_lending_z[id_integrated$id == x] + 1))
					+ temp.candidates2.subset$number_of_members_z) * (id_integrated$freq_borrowing_z[id_integrated$id == x] + 1), decreasing=T)
				]))

		}


		# return(paste(names(temp.tableTeam[1:10]), collapse=' '))
		return(paste(temp.return[1:10], collapse=' '))
	}
}