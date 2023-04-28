bar.lines <- function(bplt,data,first.bar,second.bar,space,height,
	text.height=height*1.3,annotation=NA,cex=1,line.col="black",text.col="black")
{
 # lines
 values<-data[c(first.bar,second.bar)]
 y.cord<-c(values[1]+space,rep(max(values)+height,2),values[2]+space)
 x.cord<-rep(bplt[c(first.bar,second.bar)],each=2)
 lines(x.cord,y.cord,col=line.col)
 # annotation
 if(!is.na(annotation)){
  x.text<-mean(bplt[c(first.bar,second.bar)])
  y.text<-max(y.cord)+text.height
  text(annotation,x=x.text,y=y.text,cex=cex,col=text.col)
 }
}