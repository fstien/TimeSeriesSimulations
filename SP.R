df <- read.table("SandP.txt")

df <- df[nrow(df):1, ]

plot(ts(dfTest$V2), type="l")

acf(dfTest$V2)
