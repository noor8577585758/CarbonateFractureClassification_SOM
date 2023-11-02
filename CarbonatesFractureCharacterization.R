require(kohonen)
data=read.csv("C:/Users/hudan/Documents/Pseudowells/P_well")
data_train<- data
data_train_matrix <- as.matrix(scale(data_train))
som_grid<- somgrid(xdim = 20,ydim= 20, topo= "hexagonal")
som_model<- som(data_train_matrix, grid=somgrid(), rlen=100, alpha= c(0.05,0.01))
plot(som_model, type="changes")
plot(som_model, type="count")
plot(som_model, type="dist.neighbours")
plot(som_model, type="codes")
var<-7
var_unscaled<- aggregate(as.numeric(data_train[,var]), 
                          by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var])
mydata <- som_model$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:8) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:8, wss, type="b", xlab="Number of cluters",ylab="within groups sum  of squares",main="within cluster sum of square(WCSS)")

pretty_palette <- c('#FF33F6','#d62728','#1f77b4','#2ca02c','#8c564b','#ff7f0e','#9467bd','#E9967A')
som_cluster<- cutree(hclust(dist(som_model$codes[[1]])),8)
plot(som_model, type='mapping', bgcol=pretty_palette[som_cluster],main= "Clusters")
add.cluster.boundaries(som_model,som_cluster)

cluster_assignment<-som_cluster[som_model$unit.classif]
data$cluster<-cluster_assignment
#write.csv(data,"C:/Users/hudan/Documents/Noor_Lasout/B2/B2_m1_clusters.csv",row.names=FALSE)

