library(dplyr)
library(igraph)
library(stringr)
library(ggplot2)
library(ggpubr)

#### Import Processed Collaboration Information ####
HU_stakeholder<-read.csv("~\\HU_stakeholder.csv")
HU_interaction<-read.csv("~\\HU_interaction.csv")

# Count frequency of stakeholder/interaction without considering date
stakeholder_freq<-function(HU_stakeholder){
  HU_stakeholder<-data.frame(stakeholder = HU_stakeholder[,2])
  stakeholder_freq<-HU_stakeholder %>% 
    group_by(stakeholder) %>% 
    summarise(n=n())
  stakeholder_freq<-stakeholder_freq[order(-stakeholder_freq$n),]
  return(stakeholder_freq)
}

interaction_freq<-function(HU_interaction){
  HU_interaction<-data.frame(HU_interaction[,2], HU_interaction[,3])
  df1 <- data.frame(t(apply(HU_interaction[1:2], 1, sort))) # keep the order of the pair-wise name the same
  colnames(df1)<-c("from","to")
  interaction_freq<-df1 %>%
    group_by(from,to) %>% 
    summarise(n=n())
  interaction_freq<-interaction_freq[order(-interaction_freq$n),]
  return(interaction_freq)
}

Hu_SH_freq<-stakeholder_freq(HU_stakeholder)
HU_IN_freq<-interaction_freq(HU_interaction)

############################
#### Network centrality ####
############################
igraph_obj<-function(Hu_SH_freq, HU_IN_freq){
  net <- graph_from_data_frame(d=HU_IN_freq, vertices=Hu_SH_freq$stakeholder, directed=F)
  adj<-as.matrix(get.adjacency(net, type="both", attr="n", names=TRUE))
  # Convert adj matrices to edgelist (for computing centrality measures using igraph)
  edge_list<-graph_from_adjacency_matrix(adj, mode="undirected", weighted=TRUE)
  return(edge_list)
}

edge_list<-igraph_obj(FI_SH_freq,FI_IN_freq)
# Weighted degree centrality
d<-data.frame(value=strength(edge_list))
d$value<-d$value/(nrow(d)-1)
# Weighted closeness
c<-data.frame(value = closeness(edge_list,normalized = TRUE))
# Weighted betweenness
b<-data.frame(value = betweenness(edge_list,normalized = TRUE))

##########################################
#### Temperal Evolution Hurricane Only ####
##########################################
# Split the date into year and others
node_date<-str_split_fixed(HU_stakeholder[,3], "-", 2)
edge_date<-str_split_fixed(HU_interaction[,4], "-", 2)
HU_stakeholder$year<-node_date[,1]
HU_interaction$year<-edge_date[,1]

# Split the dataset on three year basis
s1<-c(2002,2003,2004)
node_s1<-filter(HU_stakeholder, year %in% s1)
edge_s1<-filter(HU_interaction, year %in% s1)

s2<-c(2005,2006,2007)
node_s2<-filter(HU_stakeholder, year %in% s2)
edge_s2<-filter(HU_interaction, year %in% s2)

s3<-c(2008,2009,2010)
node_s3<-filter(HU_stakeholder, year %in% s3)
edge_s3<-filter(HU_interaction, year %in% s3)

s4<-c(2011,2012,2013)
node_s4<-filter(HU_stakeholder, year %in% s4)
edge_s4<-filter(HU_interaction, year %in% s4)

s5<-c(2014,2015,2016)
node_s5<-filter(HU_stakeholder, year %in% s5)
edge_s5<-filter(HU_interaction, year %in% s5)

s6<-c(2017,2018,2019)
node_s6<-filter(HU_stakeholder, year %in% s6)
edge_s6<-filter(HU_interaction, year %in% s6)

# Count frequency
SH_freq1<-stakeholder_freq(node_s1)
IN_freq1<-interaction_freq(edge_s1)

SH_freq2<-stakeholder_freq(node_s2)
IN_freq2<-interaction_freq(edge_s2)

SH_freq3<-stakeholder_freq(node_s3)
IN_freq3<-interaction_freq(edge_s3)

SH_freq4<-stakeholder_freq(node_s4)
IN_freq4<-interaction_freq(edge_s4)

SH_freq5<-stakeholder_freq(node_s5)
IN_freq5<-interaction_freq(edge_s5)

SH_freq6<-stakeholder_freq(node_s6)
IN_freq6<-interaction_freq(edge_s6)

# Create igraph object
edge_list1<-igraph_obj(SH_freq1,IN_freq1)
edge_list2<-igraph_obj(SH_freq2,IN_freq2)
edge_list3<-igraph_obj(SH_freq3,IN_freq3)
edge_list4<-igraph_obj(SH_freq4,IN_freq4)
edge_list5<-igraph_obj(SH_freq5,IN_freq5)
edge_list6<-igraph_obj(SH_freq6,IN_freq6)

# Closeness
c0<-data.frame(d0 = closeness(edge_list0,normalized = TRUE))
c0$Stakeholder <- row.names(c0)

# Calculate centrality
# Weighted degree centrality
d1<-data.frame(d1=strength(edge_list1))
d1$d1<-d1$d1/(nrow(d1)-1)
d1$Stakeholder <- row.names(d1)

d2<-data.frame(d2=strength(edge_list2))
d2$d2<-d2$d2/(nrow(d2)-1)
d2$Stakeholder <- row.names(d2)

d3<-data.frame(d3=strength(edge_list3))
d3$d3<-d3$d3/(nrow(d3)-1)
d3$Stakeholder <- row.names(d3)

d4<-data.frame(d4=strength(edge_list4))
d4$d4<-d4$d4/(nrow(d4)-1)
d4$Stakeholder <- row.names(d4)

d5<-data.frame(d5=strength(edge_list5))
d5$d5<-d5$d5/(nrow(d5)-1)
d5$Stakeholder <- row.names(d5)

d6<-data.frame(d6=strength(edge_list6))
d6$d6<-d6$d6/(nrow(d6)-1)
d6$Stakeholder <- row.names(d6)

cd<-c(d1,d2,d3,d4,d5)
df_lj_d<-d6[1:10,]
for (i in seq(1,10,2)){
  df<-data.frame(cd[i],cd[i+1])
  df_lj_d<-left_join(df_lj_d,df,by="Stakeholder")
}
df_lj_d<-relocate(df_lj_d, d6, .after = d5)

# Weighted closeness
c1<-data.frame(d1 = closeness(edge_list1,normalized = TRUE))
c1$Stakeholder <- row.names(c1)

c2<-data.frame(d2 = closeness(edge_list2,normalized = TRUE))
c2$Stakeholder <- row.names(c2)

c3<-data.frame(d3 = closeness(edge_list3,normalized = TRUE))
c3$Stakeholder <- row.names(c3)

c4<-data.frame(d4 = closeness(edge_list4,normalized = TRUE))
c4$Stakeholder <- row.names(c4)

c5<-data.frame(d5 = closeness(edge_list5,normalized = TRUE))
c5$Stakeholder <- row.names(c5)

c6<-data.frame(d6 = closeness(edge_list6,normalized = TRUE))
c6$Stakeholder <- row.names(c6)

cc<-c(c1,c2,c3,c4,c5)
df_lj_c<-c6[1:10,]
for (i in seq(1,10,2)){
  df<-data.frame(cc[i],cc[i+1])
  df_lj_c<-left_join(df_lj_c,df,by="Stakeholder")
}
df_lj_c<-relocate(df_lj_c, d6, .after = d5)

# Weighted betweenness
b1<-data.frame(d1 = betweenness(edge_list1,normalized = TRUE))
b1$Stakeholder <- row.names(b1)

b2<-data.frame(d2 = betweenness(edge_list2,normalized = TRUE))
b2$Stakeholder <- row.names(b2)

b3<-data.frame(d3 = betweenness(edge_list3,normalized = TRUE))
b3$Stakeholder <- row.names(b3)

b4<-data.frame(d4 = betweenness(edge_list4,normalized = TRUE))
b4$Stakeholder <- row.names(b4)

b5<-data.frame(d5 = betweenness(edge_list5,normalized = TRUE))
b5$Stakeholder <- row.names(b5)

b6<-data.frame(d6 = betweenness(edge_list6,normalized = TRUE))
b6$Stakeholder <- row.names(b6)


cb<-c(b1,b2,b3,b4,b5)
df_lj_b<-b6[1:10,]
for (i in seq(1,10,2)){
  df<-data.frame(cb[i],cb[i+1])
  df_lj_b<-left_join(df_lj_b,df,by="Stakeholder")
}
df_lj_b<-relocate(df_lj_b, d6, .after = d5)

# Plot the graph
# Combine the three columns
df_lj_b_gather<-gather(df_lj_b, date, value, -Stakeholder)
df_lj_b_gather$numDate<-str_remove(df_lj_b_gather$date, "d")

df_lj_d_gather<-gather(df_lj_d, date, value, -Stakeholder)
df_lj_d_gather$numDate<-as.numeric(str_remove(df_lj_d_gather$date, "d"))

df_lj_c_gather<-gather(df_lj_c, date, value, -Stakeholder)
df_lj_c_gather$numDate<-str_remove(df_lj_c_gather$date, "d")

xlab<-c("2002-2004","2005-2007","2008-2010","2011-2013","2014-2016","2017-2019")

p_degree<-ggplot(df_lj_d_gather, aes(x=date, y=value, color=Stakeholder, group=Stakeholder))+
  geom_point(size=2)+
  #geom_text(aes(label=sprintf("%0.3f", round(value, digits = 3))),color="black",hjust=0.7, vjust=-0.8,cex=3.5)+
  geom_line()+
  labs(x = "Year", y = "Degree Centrality") +
  scale_x_discrete(labels= xlab)+
  theme_bw()

p_clos<-ggplot(df_lj_c_gather, aes(x=date, y=value, color=Stakeholder, group=Stakeholder))+
  geom_point(size=2)+
  #geom_text(aes(label=sprintf("%0.3f", round(value, digits = 3))),color="black",hjust=0.7, vjust=-0.8,cex=3.5)+
  geom_line()+
  labs(x = "Year", y = "Closeness Centrality") +
  scale_x_discrete(labels= xlab)+
  theme_bw()

p_betw<-ggplot(df_lj_b_gather, aes(x=date, y=value, color=Stakeholder, group=Stakeholder))+
  geom_point(size=2)+
  #geom_text(aes(label=sprintf("%0.3f", round(value, digits = 3))),color="black",hjust=0.7, vjust=-0.8,cex=3.5)+
  geom_line()+
  labs(x = "Year", y = "Betweenness Centrality") +
  scale_x_discrete(labels= xlab)+
  theme_bw()
ggarrange(p_degree+theme(axis.text.x=element_text(color = "black", size=10, angle=20, vjust=.8, hjust=0.8)),
          p_clos+theme(axis.text.x=element_text(color = "black", size=10, angle=20, vjust=.8, hjust=0.8)),
          p_betw+theme(axis.text.x=element_text(color = "black", size=10, angle=20, vjust=.8, hjust=0.8)), ncol = 1, nrow = 3)



