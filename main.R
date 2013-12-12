# raw.test / id,lender_id,team_id1 team_id2 ...



raw.lending <-read.table('data/lending.tsv', header=FALSE, sep="\t", na.strings="NULL", 
	col.names=c('lender_id','loan_id'), stringsAsFactors=TRUE)

raw.id.user.map <- read.csv('data/Id-User-map.csv', header=TRUE, stringsAsFactors=TRUE)

raw.team.join <- read.table('data/team_join.tsv', header=FALSE, sep="\t", na.strings="NULL", 
	col.names=c('lender_id','team_id','team_join_date'), 
	colClasses=c('factor', 'factor', 'POSIXct'),
	stringsAsFactors=FALSE)

raw.train <- read.table('data/train.tsv', header=FALSE, sep="\t", na.strings="NULL", 
	col.names=c('lender_id','team_id','member_since'), 
	colClasses=c('factor', 'factor', 'POSIXct'),
	stringsAsFactors=FALSE)

raw.lenders <- read.table('data/lenders2.tsv', header=FALSE, sep="\t", na.strings="NULL", quote="",
	col.names=c('lender_id','country_code','location','occupation','motivaiton','member_since'), 
	colClasses=c('factor', 'factor', 'factor', 'character', 'character', 'POSIXct'), 
	stringsAsFactors=FALSE, comment.char="")

raw.loans <- read.table('data/loans.tsv', header=FALSE, sep="\t", na.strings="NULL",
	col.names=c('loan_id','loan_posted_date','funded_amount','loan_country','loan_use','loan_activity','loan_sector','borrower_gender'), 
	colClasses=c('factor', 'POSIXct', 'numeric', 'factor', 'character', 'factor', 'factor', 'factor'),
	stringsAsFactors=FALSE, comment.char="")

raw.teams <- read.table('data//teams.tsv', header=FALSE, sep="\t", na.strings="NULL",
	col.names=c('team_id','team_since','team_name','team_category','location','motivation','description','membership_type(open/closed)','number_of_members','loan_cout','loaned_amount'), 
	colClasses=c('factor', 'POSIXct', 'character', 'factor', 'character', 'character', 'character', 'factor', 'numeric', 'numeric', 'numeric'),
	stringsAsFactors=FALSE, comment.char="")

raw.teams$loaned_amount_perLoan <- raw.teams$loaned_amount / raw.teams$loan_cout
raw.teams$loan_cout_perPerson <- raw.teams$loan_cout / raw.teams$number_of_members
raw.teams$loaned_amount_perPerson <- raw.teams$loaned_amount / raw.teams$number_of_members

raw.countryCode <- read.table('data/countryCode.tsv', header=TRUE, sep='\t', stringsAsFactors=TRUE)
raw.countryCode$Internet <- toupper(raw.countryCode$Internet)
raw.stateCode <- read.table('data/stateCode.tsv', header=TRUE, sep='\t', stringsAsFactors=TRUE)

regexp.us <- "((UNITED STATES)|(USA)|(US))"
idx.us <- grep(regexp.us, toupper(raw.teams$location))
raw.teams$countryCode <- NA
raw.teams$countryCode[idx.us] <- 'US'
raw.teams$mean_loan <- raw.teams$loaned_amount / raw.teams$loan_cout
raw.teams$loan_cout_z <- sapply(raw.teams$loan_cout, function(x){(x-mean(raw.teams$loan_cout, na.rm=T))/sd(raw.teams$loan_cout, na.rm=T)})
raw.teams$loaned_amount_z <- sapply(raw.teams$loaned_amount, function(x){(x-mean(raw.teams$loaned_amount, na.rm=T))/sd(raw.teams$loaned_amount, na.rm=T)}) <- raw.teams$loan_cout_z + raw.teams$loan_cout_z
raw.teams$number_of_members_z <- sapply(raw.teams$number_of_members, function(x){(x-mean(raw.teams$number_of_members, na.rm=T))/sd(raw.teams$number_of_members, na.rm=T)})
raw.teams$sum.loan_cout_z_loaned_amount_z <- raw.teams$loan_cout_z + raw.teams$loaned_amount_z
raw.teams$sum_z <- raw.teams$loan_cout_z + raw.teams$loaned_amount_z + raw.teams$number_of_members_z