##Making a Venn Diagram for the top 20 most important genes from each of the 4 methods
install.packages('VennDiagram')
library(VennDiagram)

#Import Andrew's data
features<-data.frame(read.table("Top_features.txt",header=T,sep="\t"))

#SVM: Linear Kernel
a1<-c("CD19","CD3D","CD3E","CD79B","PBX1","KCNN1","LRMP","ELOVL5","CD79A","SCHIP1","BLNK","HLA_DMA","NID2","PARP1","HLA_DRA","CD74","ECM1","HLA_DPB1","CD99","ALOX5")
#Random Forest
a2<-most_imp_genes
#Linear Regression
a3<-as.character(features[,1])
#Boosting
a4<-as.character(features[,2])
  
#List area values and intersections
a1_val<-length(a1)
a2_val<-length(a2)
a3_val<-length(a3)
a4_val<-length(a4)

n12<-length(intersect(a1,a2))
n13<-length(intersect(a1,a3))
n14<-length(intersect(a1,a4))
n23<-length(intersect(a2,a3))
n24<-length(intersect(a2,a4))
n34<-length(intersect(a3,a4))

n123<-length(Reduce(intersect,list(a1,a2,a3)))
n124<-length(Reduce(intersect,list(a1,a2,a4)))
n134<-length(Reduce(intersect,list(a1,a3,a4)))
n234<-length(Reduce(intersect,list(a2,a3,a4)))
n1234<-length(Reduce(intersect,list(a1,a2,a3,a4)))

grid.newpage()
draw.quad.venn(area1 = a1_val, area2 = a2_val, area3 = a3_val, area4=a4_val, n12 = n12, n23 = n23, n13 = n13, n14=n14, n24=n24, n34=n34,
                 n123 = n123 , n124=n124, n134=n134 , n234=n234 , n1234=n1234, category = c("SVM:Linear Kernel", "Random Forest", "Linear Regression","Gradient Boosted Machines"), lty = "blank", 
                 fill = c("skyblue", "pink1", "mediumorchid","lightyellow"))

#Print out the genes that are shared by all 4 methods
print(Reduce(intersect,list(a1,a2,a3,a4)))
