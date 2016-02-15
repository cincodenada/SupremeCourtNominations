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

maketerm = function(x) {
    (as.numeric(x)/365-3+(20/365))%%4
}

succession = makedate(succession, 'Vacancy.Date')
nominations = makedate(nominations, list('Submission.Date','Result.Date'))

succession = subset(succession, Vacancy.Reason != 'Oath of office')
succession = subset(succession, Vacancy.Reason != 'Promoted')
nominations = subset(nominations, Replacing != 'Inaugural')

# Tried to do tail(strsplit(x," ")) instead of the second gsub, but...shit got cray
succession$Last.Name = sapply(succession$Vacancy.Name, function(x) { x=gsub("(, \\w+\\.| I+)$","",x); regmatches(x,regexpr("((?:Van )?\\S+)$",x)) })
nominations$Last.Name = sapply(nominations$Nominee, function(x) { x=gsub("(, \\w+\\.| I+)$","",x); regmatches(x,regexpr("((?:Van )?\\S+)$",x)) })

#joined = merge(nominations,succession,by.x='Replacing',by.y='Last.Name',all.x=T)
joined = nominations

#joined$Nomination.Time = as.numeric(joined$Submission.Date - joined$Vacancy.Date)/365
joined$Result.Time = as.numeric(joined$Result.Date - joined$Submission.Date)/365

# For our purposes, ignore people that were replaced before they retired
#joined = joined[joined$Nomination.Time > 0,]

#joined$Vacancy.Term = as.numeric(joined$Vacancy.Date)/365
joined$Submission.Term = maketerm(joined$Submission.Date)
joined$Result.Term = joined$Submission.Term + joined$Result.Time
joined$Label = paste(joined$Nominee, " (", format(joined$Result.Date, "%b %Y"), ")", sep="")

joined = with(joined, joined[order(Submission.Term),])
joined$rownum = 1:nrow(joined)

p = ggplot(joined) +
    geom_rect(ymin=0,ymax=nrow(joined),xmin=maketerm(as.Date("2015-11-01")),xmax=maketerm(as.Date("2016-11-01")),color="transparent",fill="gray") +
    geom_segment(aes(x=Submission.Term,xend=Result.Term,y=rownum,yend=rownum,color=Result),size=2) +
    geom_text(aes(label=Label, y=rownum,x=Result.Term),hjust=-0.025,size=2.5) +
    geom_vline(xintercept=maketerm(as.Date(Sys.time())),color="red") +
    scale_x_continuous(expand=c(0,0),limits=c(0,5))
