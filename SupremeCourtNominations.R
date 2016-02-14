library(ggplot2)

succession = read.csv('succession.csv')
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
