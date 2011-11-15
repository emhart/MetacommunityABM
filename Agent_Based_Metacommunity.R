
   my.args <- commandArgs()
    base.energy <- 20
    start.age <- 1
    foraging.eff <- c(5,6)
    #this is a fractional cost
    dispersal.penalty <- .6
    dispersal.gamma.p <- c(20,1)
    view.size <- 1
    death.age <- 100
    reproduction.energy <- 30
    reproduction.cost <-20
    species.name <- 1
    competition.rate <- c(1,1)
    #this is a fractional cost
    on.patch.movement.cost <- .9
    #body.size is an energy limit, a threshold for the maximum amount of energy allowed
    body.size <- 60
    #how many time steps it takes to handle this animal as prey if caught, a range of
    #integers
    handling.time <- c(8,10)
    #parameters from a beta distribution that define how difficult a prey item is to capture
    capture.difficulty <- c(.41,.42)
    #conversion rate is how much energy a predator gains from a successful capture,
    #gamma distribution parameters
    conversion.rate <- c(45,3)    #45
    




    breeds.list.1 <- list()
    breeds.list.1[[1]] <- base.energy
    breeds.list.1[[2]] <- start.age
    breeds.list.1[[3]] <- foraging.eff
    breeds.list.1[[4]] <-  dispersal.penalty
    breeds.list.1[[5]] <- dispersal.gamma.p
    breeds.list.1[[6]] <- view.size
    breeds.list.1[[7]] <- death.age
    breeds.list.1[[8]] <- reproduction.energy
    breeds.list.1[[9]] <- reproduction.cost
    breeds.list.1[[10]] <- species.name
    breeds.list.1[[11]] <- competition.rate
    breeds.list.1[[12]] <- on.patch.movement.cost
    breeds.list.1[[13]] <- body.size
    breeds.list.1[[14]] <- handling.time
    breeds.list.1[[15]] <- capture.difficulty
    breeds.list.1[[16]] <- conversion.rate

    # for species 2

    base.energy <- 20
    start.age <- 1
    foraging.eff <- c(5,6)
    #this is a fractional cost
    dispersal.penalty <- .55
    dispersal.gamma.p <- c(27,2)
    view.size <- 1
    death.age <- 100
    reproduction.energy <- 30
    reproduction.cost <- 20                                         
    species.name <- 2
    competition.rate <- c(.2,.25)
    #this is a fractional cost
    on.patch.movement.cost <- .9
    #body.size is an energy limit, a threshold for the maximum amount of energy allowed
    body.size <- 60
    #how many time steps it takes to handle this animal as prey if caught, a range of
    #integers
    handling.time <- c(4,7)
    #parameters from a beta distribution that define how difficult a prey item is to capture
    capture.difficulty <- c(.49,.51)
    #conversion rate is how much energy a predator gains from a successful capture,
    #gamma distribution parameters
    conversion.rate <- c(55,3)  #63

    breeds.list.2 <- list()
    breeds.list.2[[1]] <- base.energy
    breeds.list.2[[2]] <- start.age
    breeds.list.2[[3]] <- foraging.eff
    breeds.list.2[[4]] <-  dispersal.penalty
    breeds.list.2[[5]] <- dispersal.gamma.p
    breeds.list.2[[6]] <- view.size
    breeds.list.2[[7]] <- death.age
    breeds.list.2[[8]] <- reproduction.energy
    breeds.list.2[[9]] <- reproduction.cost
    breeds.list.2[[10]] <- species.name
    breeds.list.2[[11]] <- competition.rate
    breeds.list.2[[12]] <- on.patch.movement.cost
    breeds.list.2[[13]] <- body.size
     breeds.list.2[[14]] <- handling.time
    breeds.list.2[[15]] <- capture.difficulty
    breeds.list.2[[16]] <- conversion.rate

    base.energy <- 30
    start.age <- 1
    foraging.eff <- c(2,2.5)
    #this is a fractional cost
    dispersal.penalty <- .86
    dispersal.gamma.p <- c(20,1.6)
    view.size <- 1
    death.age <- 100
    reproduction.energy <- 25
    reproduction.cost <- 20
    species.name <- 3
    competition.rate <- c(1,1)
    #this is a fractional cost
    on.patch.movement.cost <- .915 #.915
    #body.size is an energy limit, a threshold for the maximum amount of energy allowed
    body.size <- 100
    failure.penalty <- .9





   predator.list <- list()
    predator.list[[1]] <- base.energy
    predator.list[[2]] <- start.age
    predator.list[[3]] <- foraging.eff
    predator.list[[4]] <-  dispersal.penalty
    predator.list[[5]] <- dispersal.gamma.p
    predator.list[[6]] <- view.size
    predator.list[[7]] <- death.age
    predator.list[[8]] <- reproduction.energy
    predator.list[[9]] <- reproduction.cost
    predator.list[[10]] <- species.name
    predator.list[[11]] <- competition.rate
    predator.list[[12]] <- on.patch.movement.cost
    predator.list[[13]] <- body.size
    predator.list[[14]] <- failure.penalty






    dim.x <- 150
    dim.y <- 150

    #Now we need to determine what fraction of the total habitat will be usable

    patch.number <- 25
    min.p.size <- 60
    max.p.size <- 150
    buffer.size <- 12

    p.energy <- 30
    n.occupants <- 0
    #this is the probability of a catastrophe
    p.catas <- .008
    #the amount of time a patch is barren after a catastrophe




    #now create a habitat matrix to populate
    hab.matrix <- matrix(0,nrow=dim.y,ncol=dim.x)

    #All the parameters for this function are passed in as two lists
    #The first holds parameters for laying out the coordinates is space
    #patch.number  - the number of patches
    #min.p.size - the minimum patch size
    #max.p.size - the maximum patch size
    #buffer.size - the minimum distance between patches
    #dim.x -  The x and y dimensions of the grid
    #dim.y
    #The next is the actual parameters of our patches
    #p.energy - energy of a patch
    #n.occupants - the number of occupants of

    dims.list <- list()
    dims.list[[1]] <- patch.number
    dims.list[[2]] <- min.p.size
    dims.list[[3]] <- max.p.size
    dims.list[[4]] <- buffer.size
    dims.list[[5]] <- dim.x
    dims.list[[6]] <- dim.y

    params.list <- list()
    params.list[[1]] <- p.energy
    params.list[[2]] <- n.occupants
    params.list[[3]]  <- p.catas
   


  #These are the parameters for the regrowth of patches
  threshold <- 20
  regrow.rate <- .1 * threshold
  #this is the fraction of the full regrowth rate that occupied patches have
  fraction <- .5



#ok, putting it all together as a first pass for running on VACC




  #create.predator.  This function takes a population size and all the predator attributes as a list
  create.predator <- function(breeds.list,pop.size, patch.matrix){
  breed.matrix <- matrix(NA,nrow=pop.size,ncol=12)
  loc.index <- sample(1:dim(patch.matrix)[1],pop.size, replace=T)
  breed.matrix[,1:2] <- as.matrix(patch.matrix[loc.index,1:2])
  breed.matrix[,3] <- runif(pop.size,0,360)
  breed.matrix[,4] <- floor(breed.matrix[,1])
  breed.matrix[,5] <- floor(breed.matrix[,2])
  breed.matrix[,6] <- rep(breeds.list[[1]],pop.size)
  breed.matrix[,7] <- rep(breeds.list[[2]],pop.size)
  breed.matrix[,8] <- rep(1,pop.size)
  breed.matrix[,9] <- breeds.list[[10]]
  breed.matrix[,10] <- 1:pop.size
  breed.matrix[,11] <- find.patch.index(breed.matrix[,1],breed.matrix[,2],dims.list[[5]])
  breed.matrix[,12] <- rep(0,pop.size)
  breed.matrix <- as.data.frame(breed.matrix)
  colnames(breed.matrix) <- c("x.loc","y.loc","direction","x.cell","y.cell","energy","age","handling.time","species.name","index","grid.index","feeding")
  breed.matrix$patch.number <- find.patch.num(breed.matrix,patch.matrix)

  return(as.data.frame(breed.matrix))
  }



#These functions help give animals an awareness of their surroundings.
#It can be integrated into other functions that can then return
#values like coordinates with food on them, etc.
# it takes the current animals position as a vector, the size of the view (cells out
#from the center) and then just the maximum size of the grid to make the view torroidal

animal.view <- function(animal.xy,view.size,max.size)
{
animal.xy <- as.numeric(animal.xy)
 x <- animal.xy[1]
 y <- animal.xy[2]
 x <- vector()
y <- vector()

x[1] <- animal.xy[1] - view.size
x[2] <-animal.xy[1] + view.size
y[1] <-animal.xy[2] - view.size
y[2] <-animal.xy[2] + view.size

x.m<-rep(min(x):max(x),abs(diff(y))+1)
y.m<-sort(rep(min(y):max(y),abs(diff(x))+1))
out.mat <- cbind(x.m,y.m)
  colnames(out.mat) <- c("x.cell","y.cell")
  out.mat <- make.torroid.grid(out.mat,max.size)
  out.mat <- cbind(out.mat,find.patch.index(out.mat[,1],out.mat[,2],max.size))
   colnames(out.mat) <- c("x.cell","y.cell","grid.index")
return(as.data.frame(out.mat))}
#a single competition function that is used to have the first species interfere with the second
#first I find which patches have BOTH animals on them
compete <- function(patch.matrix,interferer,interferee,breeds.list.interferer){
#first I subset so as to exclude competition off patches
interferer.on.patch <-subset(interferer,!is.na(interferer$patch.number))
interferee.on.patch <-subset(interferee,!is.na(interferee$patch.number))

occupied.patches <- match(interferee.on.patch$grid.index,interferer.on.patch$grid.index)
#this returns the index of all interferer's who have at least one other species to compete
#with on them.  
#this is all relative to 
animals.to.compete <- which(!is.na(occupied.patches))

#sometimes no competition will occur, then I will just exit the function

if(length(animals.to.compete)==0){ return(interferee$feed.rate) }


interferee$feed.rate[interferee.on.patch$index[animals.to.compete]] <- interferee$feed.rate[interferee.on.patch$index[animals.to.compete]] * runif(length(animals.to.compete),breeds.list.interferer[[11]][1],breeds.list.interferer[[11]][2])

return(interferee$feed.rate)}

                                                                            

  
   

create.animals <- function(breeds.list,pop.size, patch.matrix){
breed.matrix <- matrix(NA,nrow=pop.size,ncol=11)
loc.index <- sample(1:dim(patch.matrix)[1],pop.size, replace=T)
breed.matrix[,1:2] <- as.matrix(patch.matrix[loc.index,1:2])
breed.matrix[,3] <- runif(pop.size,0,360)
breed.matrix[,4] <- floor(breed.matrix[,1])
breed.matrix[,5] <- floor(breed.matrix[,2])
breed.matrix[,6] <- rep(breeds.list[[1]],pop.size)
breed.matrix[,7] <- runif(pop.size,breeds.list[[3]][1],breeds.list[[3]][2])
breed.matrix[,8] <- rep(breeds.list[[2]],pop.size)
breed.matrix[,9] <- breeds.list[[10]]
breed.matrix[,10] <- 1:pop.size
breed.matrix[,11] <- find.patch.index(breed.matrix[,1],breed.matrix[,2],dims.list[[5]]) 
breed.matrix <- as.data.frame(breed.matrix)
colnames(breed.matrix) <- c("x.loc","y.loc","direction","x.cell","y.cell","energy","feed.rate","age","species.name","index","grid.index")
breed.matrix$patch.number <- find.patch.num(breed.matrix,patch.matrix)

return(as.data.frame(breed.matrix))
}


#feed.predators allows predators to feed on other animals
feed.predators <- function(species.1.matrix,species.2.matrix,predator.matrix,patch.matrix,breeds.list.1,breeds.list.2,predator.list,current.time){ 

predators.on.patch <- match(predator.matrix$grid.index,patch.matrix$grid.index)
predators.on.patch <- predator.matrix$index[!is.na(predators.on.patch)]

#combine all the species together into one possible pool of prey items
breeds.matrix <- rbind(species.1.matrix,species.2.matrix)
#now I need to reindex it to sample from that index
breeds.matrix$index <- 1:dim(breeds.matrix)[1]

#now I will find the predators to feed that are on patches with a prey item on their grid cell


#combine the breed lists into another list so they can be easily accessed by an index

breeds.list <- list(breeds.list.1,breeds.list.2)                                  





predators.to.feed <- predator.matrix[predators.on.patch,]
#further more I need to subset the predators to exclude predators that are "full",
#and those that are currently handling a prey item
predators.to.feed <- subset(predators.to.feed, predators.to.feed$energy < predator.list[[13]])
predators.to.feed <- subset(predators.to.feed, predators.to.feed$handling.time <= current.time)

if(dim(predators.to.feed)[1]==0){ return(list(species.1.matrix,species.2.matrix,predator.matrix)) }



r.order <- sample(1:dim(predators.to.feed)[1],dim(predators.to.feed)[1])

for(r in 1:dim(predators.to.feed)[1]){

#sample has an annoying habit of sampling from 1:x if x is scalar, so if its only of size 1, then we need to make a fake vector
grid.view <- animal.view(predators.to.feed[r.order[r],4:5],predator.list[[6]],dims.list[[5]])

possible.prey.items <- breeds.matrix$index[breeds.matrix$grid.index %in% grid.view$grid.index]


if(length(possible.prey.items) > 0){
prey.item <- sample(rep(possible.prey.items,2),1)

prey.item <- breeds.matrix[prey.item,]

#Did the prey item get caught?
capture <- rbinom(1,1,prob=runif(1,breeds.list[[prey.item$species.name]][[15]][1],breeds.list[[prey.item$species.name]][[15]][2]))

if(capture == 1){
  predator.matrix$energy[predators.to.feed$index[r.order[r]]] <- predator.matrix$energy[predators.to.feed$index[r.order[r]]] + rgamma(1,breeds.list[[prey.item$species.name]][[16]][1], breeds.list[[prey.item$species.name]][[16]][2])
  predator.matrix$handling.time[predators.to.feed$index[r.order[r]]] <- current.time + sample(breeds.list[[prey.item$species.name]][[14]][1]:breeds.list[[prey.item$species.name]][[14]][2],1)
  #finally I will remove the dead animal and reindex the matrix
 breeds.matrix<- breeds.matrix[breeds.matrix$index !=prey.item$index,]
 breeds.matrix$index <- 1:dim(breeds.matrix)[1]
 } 
 
 if(capture == 0){
 predator.matrix$energy[r.order[r]] <- predator.matrix$energy[r.order[r]] * predator.list[[14]]
 }
 }
 
 }
 
 species.1.matrix <- subset(breeds.matrix,breeds.matrix$species.name==1)
 species.1.matrix$index <-1:dim(species.1.matrix)[1]
 species.2.matrix <- subset(breeds.matrix,breeds.matrix$species.name==2)
 species.2.matrix$index <-1:dim(species.2.matrix)[1]
 
 return(list(species.1.matrix,species.2.matrix,predator.matrix))}
 


#disperse.animals will simply return a new set of coordinates for 
# a matrix of animals not on patches
#patch.matrix holds all the current patches, and breeds. matrix holds
#all the relevant parameters for our animals.
#Because this model uses a vectorized approach as much as possible,
#all positions are simultaneously updated.
disperse.animals <- function(patch.matrix,breeds.matrix, breeds.list){

merged.data <- merge(breeds.matrix, patch.matrix,by = c("x.cell","y.cell"))
#I create an index of all the animals not on a habitat patch or that are on a habitat patch with 0
#energy
disp.index <- c(setdiff(breeds.matrix$index,merged.data$index.x),merged.data$index.x[merged.data$p.energy <= 0])
#Now I will capture whether or not there is any dispersal to be done
if(length(disp.index)==0){return(breeds.matrix)}
else{

#I create a random distance
new.distance <- rgamma(length(disp.index),shape=breeds.list[[5]][1], scale = breeds.list[[5]][2])
#I calculate the new position using geometry
breeds.matrix[disp.index,1] <- (cos(breeds.matrix[disp.index,3]) * new.distance) + breeds.matrix[disp.index,1]
breeds.matrix[disp.index,2] <- (sin(breeds.matrix[disp.index,3]) * new.distance) + breeds.matrix[disp.index,2]
#I update the direction and cell location
if(length(disp.index) > 0) {breeds.matrix[disp.index,1:2] <- make.torroid.grid(breeds.matrix[disp.index,1:2],dims.list[[5]])}
breeds.matrix[disp.index,3] <- runif(length(disp.index),0,360)
breeds.matrix[disp.index,4] <-  floor(breeds.matrix[disp.index,1])
breeds.matrix[disp.index,5] <- floor(breeds.matrix[disp.index,2])
#now we penalize dispersal 
breeds.matrix$energy[disp.index] <- breeds.matrix$energy[disp.index]*breeds.list[[4]]


return(breeds.matrix)}
}   
feed.patches <- function(patch.matrix, species.matrix,breeds.list){
#the first thing I do is remove all the individuals who are "full" or at max body size
#and prevent them from feeding



#first we combine all the species together so they can all feed randomly
breeds.matrix <- species.matrix
#now we need to re-index it
#breeds.matrix$index <- 1:dim(breeds.matrix)[1] 

merged.data <- merge(breeds.matrix, patch.matrix,by = c("x.cell","y.cell"))
merged.data <- subset(merged.data,merged.data$energy < breeds.list[[13]])

r.order <- sample(1:dim(merged.data)[1],dim(merged.data)[1])
#This must be done in  a random order
for (i in 1:length(r.order)){
#first we need to make sure that the amount of energy on the patch greater than the amount the organism is going to feed on
if (patch.matrix$p.energy[merged.data$index.y[r.order[i]]] > merged.data$feed.rate[r.order[i]]){  
patch.matrix$p.energy[merged.data$index.y[r.order[i]]] <- patch.matrix$p.energy[merged.data$index.y[r.order[i]]] - merged.data$feed.rate[r.order[i]]  
breeds.matrix$energy[merged.data$index.x[r.order[i]]] <-  merged.data$feed.rate[r.order[i]] + merged.data$energy[r.order[i]]
}


#if it is to low, set the patch energy to 0 and give the rest to the organism
if (patch.matrix$p.energy[merged.data$index.y[r.order[i]]] <= merged.data$feed.rate[r.order[i]]){  
breeds.matrix$energy[merged.data$index.x[r.order[i]]] <-  patch.matrix$p.energy[r.order[i]]  + merged.data$energy[r.order[i]]

patch.matrix$p.energy[merged.data$index.y[r.order[i]]] <- 0 }

}

return(list(patch.matrix,breeds.matrix))


}


#Move on patch is the function to use to move animals on a patch.  
#Once an animal is on a patch it will move randomly within that patch
#within a given view size
#Move on patch is the function to use to move animals on a patch.  
#Once an animal is on a patch it will move randomly within that patch
move.on.patch <- function(patch.matrix,breeds.matrix,breeds.list){
merged.data <- merge(breeds.matrix, patch.matrix,by = c("x.cell","y.cell"))
merged.data$index <- 1:dim(merged.data)[1]
if(dim(merged.data)[1] ==0){return(breeds.matrix)}

occupied.patches <- unique(merged.data$patch.num)
for(r in 1:length(occupied.patches)){
num.new.patches <- length(merged.data$index.y[merged.data$patch.num==occupied.patches[r]])
animal.index <- merged.data$index.x[merged.data$patch.num==occupied.patches[r]]
new.patch.coords <- sample(patch.matrix$index[patch.matrix$patch.num==occupied.patches[r]],num.new.patches,replace=TRUE)
breeds.matrix$x.loc[animal.index] <- patch.matrix$x.cell[new.patch.coords] 
breeds.matrix$x.cell[animal.index] <- patch.matrix$x.cell[new.patch.coords]
breeds.matrix$y.loc[animal.index] <- patch.matrix$y.cell[new.patch.coords]
breeds.matrix$y.cell[animal.index] <- patch.matrix$y.cell[new.patch.coords]
breeds.matrix$energy[animal.index] <- breeds.matrix$energy[animal.index] * breeds.list[[12]] 
}
return(breeds.matrix)}

#This works the same as above, but instead of moving based the patch resource gradient, it 
#individuals move on the other animals there.


move.predator.on.patch <- function(patch.matrix,predators.matrix,species.1.matrix,species.2.matrix,breeds.list){
   
breeds.matrix <- rbind(species.1.matrix,species.2.matrix)

patches.with.prey <- match(breeds.matrix$patch.number,patch.matrix$patch.num)
patches.with.prey <- patches.with.prey[!is.na(patches.with.prey)]  

patches.with.prey <-  unique(patch.matrix$patch.num[patches.with.prey])

patches.with.predators <- match(predators.matrix$patch.number,patch.matrix$patch.num)
patches.with.predators <- patches.with.predators[!is.na(patches.with.predators)]  

patches.with.predators<-  unique(patch.matrix$patch.num[patches.with.predators])



patches.to.disperse <- setdiff(patches.with.predators,patches.with.prey)

patches.to.move <- match(predators$patch.number,patches.with.prey)
patches.to.move <- patches.to.move[!is.na(patches.to.move)]

patches.to.move <- unique(patches.with.prey[patches.to.move])


if(length(patches.to.disperse) > 0){
#I will now disperse all predators on a patch with no prey
for(x in 1:length(patches.to.disperse)){
disp.index <- predators.matrix$index[predators.matrix$patch.number == patches.to.disperse[x]]
#since NA's will also be returned above, I need to remove them
disp.index <- disp.index[!is.na(disp.index)]
new.distance <- rgamma(length(disp.index),shape=breeds.list[[5]][1], scale = breeds.list[[5]][2])
#I calculate the new position using geometry
predators.matrix[disp.index,1] <- (cos(predators.matrix[disp.index,3]) * new.distance) + predators.matrix[disp.index,1]
predators.matrix[disp.index,2] <- (sin(predators.matrix[disp.index,3]) * new.distance) + predators.matrix[disp.index,2]
#I update the direction and cell location
predators.matrix[disp.index,1:2] <- make.torroid.grid(predators.matrix[disp.index,1:2],dims.list[[5]])
predators.matrix[disp.index,3] <- runif(length(disp.index),0,360)
predators.matrix[disp.index,4] <-  floor(predators.matrix[disp.index,1])
predators.matrix[disp.index,5] <- floor(predators.matrix[disp.index,2])
#now we penalize dispersal 
predators.matrix$energy[disp.index] <- predators.matrix$energy[disp.index]*breeds.list[[4]]
}
}

if(length(patches.to.move) > 0){
for(x in 1:length(patches.to.move)){
    predators.to.move <- predators.matrix$index[predators.matrix$patch.number == patches.to.move[x]]
    predators.to.move <- predators.to.move[!is.na(predators.to.move)]
   
   
    new.locations <- sample(patch.matrix$index[patch.matrix$patch.num == patches.to.move[x]],length(predators.to.move),replace=TRUE)
    
    predators.matrix$x.loc[predators.to.move] <- patch.matrix$x.cell[new.locations] 
    predators.matrix$y.loc[predators.to.move] <- patch.matrix$y.cell[new.locations]
    predators.matrix$y.cell[predators.to.move] <- floor(predators.matrix$y.loc[predators.to.move])
    predators.matrix$x.cell[predators.to.move] <- floor(predators.matrix$x.loc[predators.to.move])
    predators.matrix$energy[predators.to.move] <-  predators.matrix$energy[predators.to.move]*breeds.list[[12]]
    }
    }
    
return(predators.matrix)}


predator.disperse <- function(predators.matrix, breeds.list){
disp.index <- predators.matrix$index[is.na(predators.matrix$patch.number)]
#since NA's will also be returned above, I need to remove them
if(length(disp.index)==0){return(predators.matrix)}
new.distance <- rgamma(length(disp.index),shape=breeds.list[[5]][1], scale = breeds.list[[5]][2])
#I calculate the new position using geometry
predators.matrix[disp.index,1] <- (cos(predators.matrix[disp.index,3]) * new.distance) + predators.matrix[disp.index,1]
predators.matrix[disp.index,2] <- (sin(predators.matrix[disp.index,3]) * new.distance) + predators.matrix[disp.index,2]
#I update the direction and cell location
predators.matrix[disp.index,1:2] <- make.torroid.grid(predators.matrix[disp.index,1:2],dims.list[[5]])
predators.matrix[disp.index,3] <- runif(length(disp.index),0,360)
predators.matrix[disp.index,4] <-  floor(predators.matrix[disp.index,1])
predators.matrix[disp.index,5] <- floor(predators.matrix[disp.index,2])
#now we penalize dispersal 
predators.matrix$energy[disp.index] <- predators.matrix$energy[disp.index]*breeds.list[[4]]

return(predators.matrix)}



animal.death <- function(breeds.matrix,breeds.list){


    still.alive <- subset(breeds.matrix, breeds.matrix$age < breeds.list[[7]])
    still.alive <- subset(still.alive,still.alive$energy > .5)
    still.alive$index <- 1:dim(still.alive)[1]
    
    return(still.alive)
    }
    
animal.reproduction <- function(breeds.matrix,breeds.list){


#This function simply reproduces if the animal is above a certain energy threshold
#the offspring are placed in the same location, but with a different direction.
#It uses a beta function to determine probability of reproduction, which ends up with a logarithmic
#function in relation to age.
mothers <- subset(breeds.matrix,breeds.matrix$energy > breeds.list[[8]])

#First if noone has the energy to reproduce we just return the matrix
if(dim(mothers)[1] == 0) {return(breeds.matrix)}
else{
mothers$energy <- breeds.list[[1]]

mothers$direction <- runif(dim(mothers)[1],0,360)

reproduction.p <- reproduction.prob(mothers$age,breeds.list[[7]])

success.repro <- rbinom(length(mothers$age),1,prob=reproduction.p)
mothers$age <- 1
mothers <- mothers[success.repro==1,]
breeds.matrix$energy[mothers$index] <-  breeds.matrix$energy[mothers$index] - breeds.list[[9]]
breeds.matrix <- rbind(breeds.matrix,mothers)
breeds.matrix$index <- 1:dim(breeds.matrix)[1]

return(breeds.matrix)}

}

reproduction.prob <- function(ages,death.age){
rep.p <- rep(NA,length(ages))
ages <- cbind(ages,1:length(ages))
half.age <- death.age / 2
young <- subset(ages,ages[,1] <= half.age)
old  <- subset(ages,ages[,1] > half.age)
rep.p[young[,2]] <- young[,1] / death.age 
rep.p[old[,2]] <- 1- (old[,1]/death.age) 
return(rep.p)}


grow.all.patches <- function(dims.list,params.list) {
patch.number   <-  dims.list[[1]]
min.p.size <-  dims.list[[2]]
max.p.size  <-  dims.list[[3]]
buffer.size <-  dims.list[[4]]
dim.x <-  dims.list[[5]]
dim.y <-  dims.list[[6]]
buffers <- matrix(-1,nrow=1,ncol=2,dimnames=list(c("r"),c("x","y")))
initloop <- 1
#this matrix will hold all the information of our patches
patch.attrib.matrix <- matrix(0,nrow=1,ncol=5)

for (x in 1:patch.number){
#First I randomly choose the patch size                            
patch.size <- sample(min.p.size:max.p.size,1)
#Now I choose random starting coordinates
y.vec <- sample(1:dim.y,1)
x.vec <- sample(1:dim.x,1)
# if those coordinates are within another patch or buffer I draw new ones
while (dim(merge(matrix(c(x.vec,y.vec),nrow=1,ncol=2,dimnames=list(c("r"),c("x","y"))),buffers,by=c("x","y")))[1] > 0){

y.vec <- sample(1:dim.y,1)
x.vec <- sample(1:dim.x,1)
initloop <- initloop +1
#this is the stopping rule if the buffer becomes too big
if(initloop > 150){
cat("fail")
patch.attrib.matrix <- cbind(patch.attrib.matrix,find.patch.index(patch.attrib.matrix[,1],patch.attrib.matrix[,2],dims.list[[5]]))
return(patch.attrib.matrix[2:dim(patch.attrib.matrix)[1],])}


}
initloop <- 1
 p.coord <- grow.patch(patch.size,c(x.vec,y.vec),buffers,x)
p.coord <- p.coord[1:min(patch.size,dim(p.coord)[1]),]
temp.buffer <- create.buffer(p.coord,buffer.size) 
temp.buffer <-  make.torroid.grid(temp.buffer,dim.x)
buffers <- rbind(buffers,temp.buffer)
buffers <- unique(buffers)
#Now I will make the coordinates torroidal
n.p.coord <- make.torroid.grid(p.coord,dim.x)
patch.attrib.matrix <- rbind(patch.attrib.matrix,cbind(n.p.coord,rep(x,min(patch.size,dim(p.coord)[1])),rep(params.list[[1]],min(patch.size,dim(p.coord)[1])),rep(params.list[[2]],min(patch.size,dim(p.coord)[1]))))
 
}

cat("I did it")
patch.attrib.matrix <- cbind(patch.attrib.matrix,find.patch.index(patch.attrib.matrix[,1],patch.attrib.matrix[,2],dims.list[[5]]))
return(patch.attrib.matrix[2:dim(patch.attrib.matrix)[1],])

}




#This function will grow a single new patch
grow.patch <- function(patch.size,origin,buffers,p.num) {
 place.counter <- 2
new.growth <- c(0,1)
p.coord <- cbind(origin[1],origin[2])

while (dim(p.coord)[1] < patch.size){
y.vec.total <- vector()
x.vec.total <- vector()
       
   for (i in new.growth[place.counter - 1]:new.growth[place.counter]){
   x.vec <- p.coord[i ,1]
   y.vec <- p.coord[i ,2]
   y.vec.total <- c(y.vec.total,y.vec + sample(c(-1,0,1),4,replace=TRUE))
   x.vec.total <- c(x.vec.total,x.vec + sample(c(-1,0,1),4,replace=TRUE))
   
   }
   p.coord <- rbind(p.coord,cbind(x.vec.total,y.vec.total))
   #I check to make sure the proposed new patch coordinates aren't within the patch 
   #buffer

  p.coord <- buffer.check(p.coord,buffers,origin)[[2]]
  #Here we strip out duplicate patches 
  
   p.coord <- unique(p.coord)
   place.counter <- place.counter + 1
   new.growth[place.counter] <- dim(p.coord)[1]
  if(length(new.growth[new.growth==max(new.growth)]) > 20){return(p.coord)}
 
 
  #if(place.counter > 125){return(p.coord)} 

}

p.coord <- p.coord[1:patch.size,]
return(p.coord)


}



 


#This function will make the patch habitat torriodal, it assumes a square
#grid, modifications will need to be made in order for it to work with other shapes
make.torroid.grid <- function(mat.coords,max.size)
{
if(!is.null(dim(mat.coords))){
for(i in 1:dim(mat.coords)[1]){
  for(x in 1:2){
            if(mat.coords[i,x] < 0) {
                  mat.coords[i,x] <- max.size + mat.coords[i,x]}
             if(mat.coords[i,x] == 0) {mat.coords[i,x] <- max.size  }  
            
            if(mat.coords[i,x] > max.size) { mat.coords[i,x] <- mat.coords[i,x] - max.size}               
                 }
            }
   return(mat.coords)  }
else{

  for(x in 1:2){
 if(mat.coords[x] < 0) {
                  mat.coords[x] <- max.size + mat.coords[x]}
             if(mat.coords[x] == 0) {mat.coords[x] <- max.size  }  
            
            if(mat.coords[x] > max.size) { mat.coords[x] <- mat.coords[x] - max.size} 
   
  } 
  
  return(mat.coords)
  }
   } 

#This function will create a buffer and return a vector containing the coords
#of a buffer in the form x-min x-max y-min y-max

create.buffer <- function(mat.coords,buffer.size){ 
x <- vector()
y <- vector()

x[1] <- min(mat.coords[,1]) - buffer.size
x[2] <- max(mat.coords[,1]) + buffer.size
y[1] <- min(mat.coords[,2]) - buffer.size
y[2] <- max(mat.coords[,2]) + buffer.size

x.m<-rep(min(x):max(x),abs(diff(y))+1)
y.m<-sort(rep(min(y):max(y),abs(diff(x))+1))
out.mat <- cbind(x.m,y.m)
colnames(out.mat) <- c("x","y")

return(out.mat)}



#This function will perform a buffer check and eliminate squares that 
#encroach on a square buffer around a set of points
#I will need to know the current number of patches, and the buffer around those patches
#Buffers will be stored as a list, if the current points are in a buffer zone
#they will be returned a1s the origin points and then later stripped out.
#It works by comparing two matrices, dataframe of x,y coordinates of all the
#points not allowed to be occupied, and a second, a matrix of potential coordinates
#in the form of x,y,index.  The index of the position in pot.coords is then replaced
#with the origin coordinates and stripped out later

buffer.check <- function(pot.coords,buffers,origin){
if (length(buffers)==2){return(list(0,pot.coords))}

else {
my.length <- dim(pot.coords)[1]
#buffers <- as.data.frame(buffers)
pot.coords <- cbind(pot.coords,1:my.length)
#pot.coords <- as.data.frame(pot.coords)
colnames(buffers) <- c("x","y")
colnames(pot.coords) <- c("x","y","index")

matches <- merge(pot.coords,buffers,by=c("x","y"))
pot.coords[matches$index,1] <- origin[1]
pot.coords[matches$index,2] <- origin[2]
return(list(1,as.matrix(pot.coords[,1:2])))}

}


patch.regrowth <- function(patch.matrix,breeds.matrix,regrow.rate,fraction,threshold){


#now we need to subset the ones that are fully "grown"
to.grow <- subset(patch.matrix,patch.matrix$p.energy < threshold)
#first we find all the patches with animals on them
occupied.grids <- match(to.grow$grid.index,breeds.matrix$grid.index)
empty.cells <- which(is.na(occupied.grids))
occupied.cells <- which(!is.na(occupied.grids)) 


#then we find the empty ones

patch.matrix$p.energy[to.grow$index[occupied.cells]] <- fraction*regrow.rate  + patch.matrix$p.energy[to.grow$index[occupied.cells]]
patch.matrix$p.energy[to.grow$index[empty.cells]] <- regrow.rate + patch.matrix$p.energy[to.grow$index[empty.cells]]

return(patch.matrix)}


#in order to speed up the locating of surrounding organisms they are linked
# to a patch index, that patch index is returned by this function, as well as
#the patch number
find.patch.index <- function(x.coord,y.coord,max.x){
return.index <- vector()
return.index <- ((y.coord -1)*max.x) + x.coord

return(return.index)}

find.patch.num <- function(breeds.matrix, patch.matrix){

patches <- match(breeds.matrix$grid.index,patch.matrix$grid.index)
patch.number <- patch.matrix$patch.num[patches]
return(patch.number)}


catastrophe <- function(patch.matrix,species.1,species.2,predators,params.list)
{
#first I choose if any patches will go extinct
extinct.patches <- which(rbinom(max(patch.matrix$patch.num),1,prob=params.list[[3]])==1)

if(length(extinct.patches)==0){return(list(species.1,species.2,predators,0))}


#now I loop through, removing all the organisms, and setting the regen time
for(x in 1:length(extinct.patches)){
species.1 <- subset(species.1,species.1$patch.number != extinct.patches[x])
species.1$index <- 1:dim(species.1)[1]

species.2 <- subset(species.2,species.2$patch.number != extinct.patches[x])
species.2$index <- 1:dim(species.2)[1]

predators <- subset(predators,predators$patch.number != extinct.patches[x])
predators$index <- 1:dim(predators)[1]
 
 cat("extinction ",extinct.patches[x]," ")

}

return(list(species.1,species.2,predators,sum(extinct.patches)))}


time.steps <- as.integer(my.args[6])



z<- grow.all.patches(dims.list,params.list)    
z <- cbind(z,1:dim(z)[1])
colnames(z) <- c("x.cell","y.cell","patch.num","p.energy","pop.size","grid.index","index")

z <- as.data.frame(z)

species.1 <- create.animals(breeds.list.1,100,z)
species.2 <- create.animals(breeds.list.2,100,z)
predators <- create.predator(predator.list,75,z)
    



hab.matrix <- matrix(0,nrow=dim.y,ncol=dim.x)

hab.matrix[as.matrix(z[,1:2])] <- 2



 p.size <-vector()
 p.size.1  <- vector()
 p.size.2 <- vector()
 p.size.p <- vector()
 
patches.1 <- matrix(0,nrow=0,ncol=patch.number)
patches.2 <- matrix(0,nrow=0,ncol=patch.number)
patches.p <- matrix(0,nrow=0,ncol=patch.number)
patches.t <- matrix(0,nrow=time.steps,ncol=patch.number)

 
 
 surivors.1 <- matrix()
 surivors.2 <- matrix()
  survivors.p <- matrix()
  
  
 full.data <- matrix(0,nrow=0,ncol=13)
extinction.count <- vector()

  
write.csv(z,file="Grid.csv")

for(x in 1:time.steps)
{

    p.size[x] <- dim(species.1)[1] +  dim(species.2)[1] 
    p.size.1[x] <- dim(species.1)[1]
    p.size.2[x] <- dim(species.2)[1]                     
    p.size.p[x] <- dim(predators)[1]
    # update the grid index


#plot(,0,main=x)

#image(hab.matrix,x=1:50,y=1:50,main=x)
#points(species.1[,1:2],pch="1",cex=.6)
#points(species.2[,1:2],pch="2",cex=.6)
#points(predators[,1:2],pch="P",cex=.6)
#first I have species 1 and species 2 compete , competition works by
#adjusting the feeding rate parameter


 

species.1$feed.rate <- compete(z, species.2,species.1,breeds.list.2)
species.2$feed.rate <- compete(z, species.1,species.2,breeds.list.1)
#undebug(feed.predators)
predation <- feed.predators(species.1,species.2,predators,z,breeds.list.1,breeds.list.2,predator.list,x)

species.1 <- predation[[1]]
species.2 <- predation[[2]]
predators <- predation[[3]]

#then the animals eat
extinction <- catastrophe(z,species.1,species.2,predators,params.list)

species.1 <- extinction[[1]]
species.2 <- extinction[[2]]
predators <- extinction[[3]]
extinction.count[x] <- extinction[[4]]
write.csv(extinction.count,"extinction.csv")


write.csv(cbind(p.size,p.size.1,p.size.2,p.size.p),file="Population.csv")
#write.csv(rbind(species.1,species.2),file="Species_data.csv")                     
#write.csv(predators,file="predators.csv")
temp.matrix <-matrix(0,nrow=1,ncol=patch.number)
temp.matrix[1,as.integer(names(table(species.1$patch.number)))] <- table(species.1$patch.number)
patches.1 <- rbind(patches.1,temp.matrix)
temp.matrix <-matrix(0,nrow=1,ncol=patch.number)
temp.matrix[1,as.integer(names(table(species.2$patch.number)))] <- table(species.2$patch.number)
patches.2 <- rbind(patches.2,temp.matrix)
temp.matrix <-matrix(0,nrow=1,ncol=patch.number)
temp.matrix[1,as.integer(names(table(predators$patch.number)))] <- table(predators$patch.number)
patches.p <- rbind(patches.p,temp.matrix)
temp.matrix <-matrix(0,nrow=1,ncol=patch.number)

patches.t[x,] <- patches.1[x,] + patches.2[x,]  
write.csv(patches.t,file="total_patches_p.csv")
write.csv(patches.1,file="patches_1.csv")
write.csv(patches.2,file="patches_2.csv")
write.csv(patches.p,file="patches_p.csv")


#temp.data <-rbind(as.matrix(species.1),as.matrix(species.2))
#full.data <- rbind(full.data,cbind(as.matrix(temp.data),rep(x,dim(temp.data)[1])))
#write.csv(full.data,file="full_data.csv")

eating<- feed.patches(z,species.2,breeds.list.2)

z <- eating[[1]]
species.2 <- eating[[2]]



eating<- feed.patches(z,species.1,breeds.list.1)

z <- eating[[1]]
species.1 <- eating[[2]]




 
predators <- move.predator.on.patch(z,predators,species.1,species.2,predator.list)
species.1 <- move.on.patch(z,species.1,breeds.list.1)
species.2 <- move.on.patch(z,species.2,breeds.list.2)




species.1$grid.index <- find.patch.index(species.1[,4],species.1[,5],dims.list[[5]])
species.2$grid.index <- find.patch.index(species.2[,4],species.2[,5],dims.list[[5]])
predators$grid.index <- find.patch.index(predators[,4],predators[,5],dims.list[[5]])

#now I need to update the patch number of each species
 species.1$patch.number <- find.patch.num(species.1,z)
 species.2$patch.number <- find.patch.num(species.2,z)
predators$patch.number <- find.patch.num(predators,z)


 
#then they disperse if they are on cells that are 0
predators <- predator.disperse(predators,predator.list)
species.1 <- disperse.animals(z,species.1,breeds.list.1)
species.2 <- disperse.animals(z,species.2,breeds.list.2)

  #undebug(move.predator.on.patch)


#now I need to update the gridindex and patch locations

species.1$grid.index <- find.patch.index(species.1[,4],species.1[,5],dims.list[[5]])
species.2$grid.index <- find.patch.index(species.2[,4],species.2[,5],dims.list[[5]])
predators$grid.index <- find.patch.index(predators[,4],predators[,5],dims.list[[5]])

#now I need to update the patch number of each species
 species.1$patch.number <- find.patch.num(species.1,z)
 species.2$patch.number <- find.patch.num(species.2,z)
predators$patch.number <- find.patch.num(predators,z)

#now i'll regrow the patches, this takes all the animals in a single matrix
z <- patch.regrowth(z,rbind(species.1,species.2),regrow.rate,fraction,threshold)


#b.deaths <- dim(i)[1]

survivors.1 <- animal.death(species.1,breeds.list.1)
survivors.2 <- animal.death(species.2,breeds.list.2)
survivors.p <- animal.death(predators,predator.list)
if(length(surivors.1) == 0 || length(survivors.2)==0){
stop("someone went extinct")}      

species.1 <- survivors.1
species.2 <- survivors.2
predators <- survivors.p

#death[x] <- b.deaths - dim(i)[1]
#b.birth <- dim(i)[1]

species.1 <- animal.reproduction(species.1,breeds.list.1)
species.2 <- animal.reproduction(species.2,breeds.list.2)
predators <- animal.reproduction(predators,predator.list)

#next I need to have the species feeding rates reset so that they 
#to avoid evolution as well as undo competitive effects from before
species.1$feed.rate <- runif(dim(species.1)[1],breeds.list.1[[3]][1],breeds.list.1[[3]][2])
species.2$feed.rate <- runif(dim(species.2)[1],breeds.list.2[[3]][1],breeds.list.2[[3]][2])


#I then age the animals
species.1$age <- species.1$age + 1
species.2$age <- species.2$age + 1
predators$age <- predators$age + 1
cat(x," ")

}


