
source('./R_Sources/__init__.R')

siteUrls <- c('helsinki-2007', 'athens-2006', 'kyiv-2005', 'istanbul-2004')
# Collect the data
detailVoting <- fProc.votingDetails(siteUrls)
eventMetadata <- fProc.eventMetadata(siteUrls)
eventParticipants <- fProc.participantTable(siteUrls)
eventVotes <- fProc.eventVotes(siteUrls)
juryTable <- fProc.juryTable(siteUrls)

# unify and clean (check for errours)