library(ggplot2)

succession = read.csv('succession.csv',stringsAsFactors=F,col.names=c('Vacancy.Date','Vacancy.Name','Vacancy.Reason'))
nominations = read.csv('nominations.csv')

makedate = function(frame, cols) {
    if(typeof(cols) == 'character') {
        cols = list(cols)
    }
    for(col in cols) {
        frame[,paste(col,".orig",sep="")] = frame[,col]
        frame[,col] = as.Date(frame[,col], "%B %e, %Y")
    }
    frame
}

succession = makedate(succession, 'Vacancy.Date')
nominations = makedate(nominations, list('Submission.Date','Result.Date'))

succession = subset(succession, Vacancy.Reason != 'Oath of office')
nominations = subset(nominations, Replacing != 'Inaugural')

# Tried to do tail(strsplit(x," ")) instead of the second gsub, but...shit got cray
succession$Last.Name = sapply(succession$Vacancy.Name, function(x) { x=gsub("(, \\w+\\.| I+)$","",x); regmatches(x,regexpr("((?:Van )?\\S+)$",x)) })

joined = merge(nominations,succession,by.x='Replacing',by.y='Last.Name',all.x=T)

p = ggplot(joined) +
    geom_segment(aes(x=Vacancy.Date,xend=Submission.Date,y=Submission.Date,yend=Submission.Date),color="blue",size=2) +
    geom_segment(aes(x=Submission.Date,xend=Result.Date,y=Submission.Date,yend=Submission.Date),color="green",size=2)
