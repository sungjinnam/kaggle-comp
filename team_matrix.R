library(multicore)
# matrix.team <- NULL
list.team <- levels(raw.team.join$team_id)
# matrix.team <- matrix(nrow=length(list.team), ncol=length(list.team))

# for(i in 1:5){
# 	print(i)
# 	temp_subset.byTeam 		<- subset(raw.team.join, team_id == list.team[i])
# 	temp_subset.listTeam 	<- subset(raw.team.join, lender_id %in% temp_subset.byTeam$lender_id)
# 	temp_table.team <- as.vector(table(temp_subset.listTeam$team_id))
# 	matrix.team[i,] <- temp_table.team
# }
# matrix.team <- sapply(list.team, function(x){
# 	print(x)
# 	temp.listId <- raw.team.join$lender_id[raw.team.join$team_id==x]
# 	temp.listTeam <- raw.team.join$team_id[raw.team.join$lender_id %in% temp.listId]
# 	return(as.vector(table(temp.listTeam)))
# 	})
# rownames(matrix.team) <- list.team

teamList <- function(x){
	temp.initTeam <- unlist(x)
	if(length(temp.initTeam)){
		temp.listId <- raw.team.join$lender_id[raw.team.join$team_id %in% temp.initTeam]
		temp.listTeam <- raw.team.join$team_id[raw.team.join$lender_id %in% temp.listId]
		temp.tableTeam <- sort(table(temp.listTeam), decreasing=TRUE)
		temp.tableTeam <- temp.tableTeam[-which(names(temp.tableTeam) %in% temp.initTeam)]
		return(paste(names(temp.tableTeam[1:10]), collapse=' '))		
	}
	else{
		temp.tableTeam <- sort(table(raw.team.join$team_id), decreasing=TRUE)
		return(paste(names(temp.tableTeam[1:10]), collapse=' '))		
	}
}

matrix.team <- data.frame(Id=1:nrow(id_integrated))
matrix.team$Teams <- sapply(mclapply(id_integrated$list_team, function(x){teamList(x)}), as.vector)
# temp.initTeam <- id_integrated$list_team[[1]]
# temp.listId <- raw.team.join$lender_id[raw.team.join$team_id %in% temp.initTeam]
# temp.listTeam <- raw.team.join$team_id[raw.team.join$lender_id %in% temp.listId]
# temp.tableTeam <- sort(table(temp.listTeam), decreasing=TRUE)
# temp.tableTeam <- temp.tableTeam[-which(names(temp.tableTeam) %in% temp.initTeam)]
write.csv(matrix.team, paste('output/output', Sys.time(), '.csv', collapse=''), row.names=FALSE)