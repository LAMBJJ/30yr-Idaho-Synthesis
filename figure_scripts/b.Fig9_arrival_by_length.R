
arr.by.len <- read.csv("length_at_arrival.csv",as.is=TRUE,header=TRUE)
names(arr.by.len) <- c("tag.len", "q10", "med", "q90")


plot(c(104,155), c(50,94),type="n", ylab="Length at Release (mm)", xaxt="n", xlab="Arrival Date at LGR", yaxt="n")
axis(2, at=seq(50,95,5), labels=seq(50,95,5),las=2)
axis(1, at=c(105,120,135,151), labels=c("Apr 15", "Apr 30", "May 15", "May 31"))

points(arr.by.len$med, arr.by.len$tag.len, pch=19)
arrows(x0=arr.by.len$q10, y0=arr.by.len$tag.len,
       x1=arr.by.len$q90, y1=arr.by.len$tag.len, code=3, angle=90, length=0.03)

abline(v=c(105,120,135,151), lty=2, col="gray70")


