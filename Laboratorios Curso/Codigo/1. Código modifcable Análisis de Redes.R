library(igraph)
library(readr)

actors <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Actors.csv")
movies <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Movies.csv")

actors
movies

actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)

plot(actorNetwork)

E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", 
                                ifelse(E(actorNetwork)$Movie == "Apollo 13", "black",
                                       "orange"))

# Re-Plot the network

plot(actorNetwork)

V(actorNetwork)$color <- ifelse(V(actorNetwork)$BestActorActress == "Winner", "gold",
                                ifelse(V(actorNetwork)$BestActorActress == "Nominated","grey",
                                       "lightblue"))

#Re-Plot the Network
plot(actorNetwork)

plot(actorNetwork, vertex.frame.color="white")

legend("bottomright", c("Winner","Nominee", "Not Nominated"), pch=21,
       col="#777777", pt.bg=c("gold","grey","lightblue"), pt.cex=2, cex=.8)


legend("topleft", c("Forest Gump","Apollo 13", "The Rock"), 
       col=c("green","black","orange"), lty=1, cex=.8)

degree(actorNetwork, mode="all")

closeness(actorNetwork, mode="all", weights=NA, normalized=T)

betweenness(actorNetwork, directed=F, weights=NA, normalized = T)

distances(actorNetwork, v=V(actorNetwork)["Kevin Bacon"], to=V(actorNetwork), weights=NA)



#####

actors <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/ActorsExercise.csv")
movies <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/MoviesExercise.csv")
actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)
E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", 
                                ifelse(E(actorNetwork)$Movie == "Apollo 13", "black",
                                       ifelse(E(actorNetwork)$Movie == "The Rock", "orange", "red")))
V(actorNetwork)$color <- ifelse(V(actorNetwork)$Gender == "Male", "lightblue", "pink")
plot(actorNetwork)
legend("topleft", c("Male","Female"), pch=21,
       col="#777777", pt.bg=c("lightblue","pink"), pt.cex=2, cex=.8)
legend("bottomright", c("Forest Gump","Apollo 13", "The Rock", "Titanic"), 
       col=c("green","black","orange","red"), lty=1, cex=.8)

degree(actorNetwork, mode="all")

closeness(actorNetwork, mode="all", weights = NA, normalized = T)

betweenness(actorNetwork, directed = F, weights = NA, normalized = T)




