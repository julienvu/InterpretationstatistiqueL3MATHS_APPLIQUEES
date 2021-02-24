europe<-read.table("europe.csv",dec=".",sep=";",quote="",header=TRUE)
summary(europe)
#boite à moustaches sous un format pdf
pdf(file="boxplot.pdf",width=6,height=6,onefile=TRUE,family="Helvetica",title="Europe boxplot",paper="special")
#boite à moustaches dans la fenêtre plots
boxplot(europe$X.Durée.heures..,ylab="Durée (heures)")
#afficher points moyenne heures de style triangle
points(1,mean(europe$X.Durée.heures..),pch=2)
dev.off()

