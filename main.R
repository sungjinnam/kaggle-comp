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