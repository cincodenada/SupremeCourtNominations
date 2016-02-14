library(ggplot2)

succession = read.csv('succession.csv',stringsAsFactors=F)
nominations = read.csv('nominations.csv')

makedate = function(frame, cols) {
    if(typeof(cols) == 'character') {
        cols = list(cols)
    }
    for(col in cols) {
        frame[,col] = as.POSIXct(as.Date(frame[,col], "%B %e, %Y"))
    }
}

makedate(succession, 'Date')
makedate(nominations, list('Submission.Date','Result.Date'))

succession = subset(succession, Action != 'Oath of office')
# Tried to do tail(strsplit(x," ")) instead of the second gsub, but...shit got cray
succession$Last.Name = sapply(succession$Justice, function(x) { x=gsub("(, \\w+\\.| I+)$","",x); regmatches(x,regexpr("((?:Van )?\\S+)$",x)) })
colnames(succession) <- c('Vacancy.Date','Vacancy.Name','Vacancy.Reason','Last.Name')

joined = merge(nominations,succession,by.x='Replacing',by.y='Last.Name',all.x=T)
