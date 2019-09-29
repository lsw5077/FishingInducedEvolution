
# idea for later: the life history of fishers.

# maybe we can use the find length/find mass functions and drop the vb equation altogether.

# for now, going to add an interference term that makes interference the same for everyone.

# stop worrying so much about making it true to life. just make it work.
# Add interference effect.


# Let's check out the equilibrium dynamics of the resource without preds

r = 3
K = 500000
R = 400000
eaten = .6


temp <- data.frame(t = seq(1,100),
                   R = NA)

temp[1,2] <- 500000

for(i in 2:max(temp$t)){
  
  dRdt <- (r * temp[i-1,2] * (1 - temp[i-1,2] / K)) - eaten*temp[i-1,2]
  
  temp[i,2] <- temp[i-1,2] + dRdt
  
  #temp[i,2]  <- temp[i-1,2] + (temp[i-1,2]*bigr*(1-(temp[i-1,2]/Rmax))) - bigWS
}


# ok, so the chemostat dynamics are burning themselves out b/c the fluctuations are too big.
# it seems reasonable to do a logistic, b/c logistic describes bacterial cells well enough
# i.e., seems ok for biomass.
# Also, de Roos et al do use it sometimes.
# Just let it crash if it needs to crash.
# and just let the oscillations be. They're fine.

temp %>%
  ggplot()+
  geom_line(aes(x = t, y = R))+
  theme_bw()






bigBiomassForaged = bigA*bigR*mass/
  (1 + bigA*bigH*bigR)




a = 10
h = 0.1

# Can we just do individual mass/total mass raised to some power?

# ok, so quick and dirty interference function works.
# small interference function = prop_small*biomass*a/sum(prop_small*biomass)

data.frame(R = rep(seq(1, 1000, 1), 3)) %>%
mutate(f = a*R/(1 + a*h*R)) %>%
ggplot()+
  geom_point(aes(x = R, y = f))+
  theme_bw()
  
data.frame(length = seq(1, 400, 1)) %>%
mutate(propFish = 
forageCurve(length = length,
            maxPropFish = maxPropFish,
            k = k,
            x0 = x0)) %>%
ggplot()+
  geom_point(aes(x = length, y = propFish))+
  theme_bw()



# size at maturity keeps dipping and hten shooting back up, possibly b/c of the size-dependent foraging term
# If the diet thing doesn't work out, could always examine the consequences of size-dependent foraging
# for fisheries-induced evolution.



# Initial demonstration that the model works and size goes down.
# log10Ws = -5.186 + 3.103 log10TL

# make predator population

# make outcome dataframe

bigWS = 0
smallWS = 0

bigR = 500000
bigr = 1 # flux rate, basically.
bigK = 1000000


smallr = 1
smallR = 500000
smallK = 1000000

N = 500
nSteps = 200

lStartMean = 200
lStartSD = 30

harvestIntensity = 0.2
adultMortRate = 0.2
juvMortRate = 0.2
lowerLimit = 200
upperLimit = 900

k = 0.03
x0 = 200
a = 10
h = 0.005
int_constant = 0.0005

eGrowth = 0.1
eRepro = 0.1
massConstant = -5.186
massMultiplier = 3.103
maxPropFish = 0.5

offspringMass = 10
h2 = 0.3

nSim = 50

out <- data.frame(smallR = as.numeric(),
                  bigR = as.numeric(),
                  Adults = as.numeric(),
                  AdultBiomass = as.numeric(),
                  Juveniles = as.numeric(),
                  JuvenileBiomass = as.numeric(),
                  meanLength = as.numeric(),
                  cvLength = as.numeric(),
                  meanlRepro = as.numeric(),
                  cvlRepro = as.numeric())



for (i in 1:nSim){
  
  # Model setup and global variables that need to be reset each run.

  bigWS = 0
  smallWS = 0
  
  bigR = 500000
  bigr = 1 # flux rate, basically.
  bigK = 1000000
  
  
  smallr = 1
  smallR = 500000
  smallK = 1000000
  

  data.frame(spp = rep("brook", N),
             lRepro = rnorm(N, lStartMean, lStartSD),
             age = rgamma(N, 5, 2)) %>%
    mutate(ID = row_number()) %>%
    rowwise() %>%
    mutate(length = vonB(lRepro + 10, k = 1.25, t = age),
           mass = findMass(massConstant, massMultiplier, length),
           stage = ifelse(length >= lRepro, "Adult", "Juvenile"),
           biomassToRepro = ifelse(stage == "Juvenile", 0, rgamma(1, 30, 5))) %>%
    as.data.frame() -> pop

  temp <- data.frame(timestep = seq (1,nSteps),
                     nSim = i,
                     smallR = NA,
                     bigR = NA,
                     Adults = NA,
                     AdultBiomass = NA,
                     Juveniles = NA,
                     JuvenileBiomass = NA,
                     meanLength = NA,
                     cvLength = NA,
                     meanlRepro = NA,
                     cvlRepro = NA)   
  
  temp[1, "smallR"] <- smallR
  temp[1, "bigR"] <- bigR
  temp[1, "Adults"] <- length(pop$stage[pop$stage == "Adult"])
  temp[1, "AdultBiomass"] <- sum(pop$mass[pop$stage == "Adult"], na.rm = TRUE)
  temp[1, "Juveniles"] <- length(pop$stage[pop$stage == "Juvenile"]) 
  temp[1, "JuvenileBiomass"] <- sum(pop$mass[pop$stage == "Juvenile"], na.rm = TRUE)
  temp[1, "meanLength"] <- mean(pop$length, na.rm = TRUE)
  temp[1, "cvLength"] <- var(pop$length, na.rm = TRUE)/mean(pop$length, na.rm = TRUE)
  temp[1, "meanlRepro"] <- mean(pop$lRepro, na.rm = TRUE)
  temp[1, "cvlRepro"] <- var(pop$lRepro, na.rm = TRUE)/mean(pop$lRepro, na.rm = TRUE)
  
  
for (j in 2:nSteps){
  
  print(paste("sim", i, "step", j))

# big prey pop reproduces

  bigR <- dRdt(r = bigr,
               K = bigK,
               R = bigR,
               WS = bigWS)

# small prey pop reproduces
  
  smallR <- dRdt(r = smallr,
                 K = smallK,
                 R = smallR,
                 WS = smallWS)
  
# predators fished
# leave harvest deterministic for now, turn into rate for later

harvPop <- harvest(pop = pop,
          harvestIntensity = harvestIntensity,
          lowerLimit = lowerLimit,
          upperLimit = upperLimit)
  
# predators forage
# 
foragePop <- forage(pop = harvPop,
              smallR = smallR,
              bigR = bigR,
              eGrowth = eGrowth,
              eRepro = eRepro,
              maxPropFish = maxPropFish,
              k = k,
              x0 = x0,
              a = a,
              h = h)

# predators grow

growPop <- grow(pop = foragePop,
            massConstant = massConstant,
            massMultiplier = massMultiplier)

# predators die natural deaths
# 
diePop <- naturalDeath(pop = growPop,
                    adultMortRate = adultMortRate,
                    juvMortRate = juvMortRate)

# predators reproduce
# 
pop <- repro(pop = diePop,
             h2 = h2,
             offspringMass = offspringMass,
             massConstant = massConstant,
             massMultiplier = massMultiplier)

# store outcome
# need to update this to hold the first value

temp[j, "smallR"] <- smallR
temp[j, "bigR"] <- bigR
temp[j, "Adults"] <- length(pop$stage[pop$stage == "Adult"])
temp[j, "AdultBiomass"] <- sum(pop$mass[pop$stage == "Adult"], na.rm = TRUE)
temp[j, "Juveniles"] <- length(pop$stage[pop$stage == "Juvenile"]) 
temp[j, "JuvenileBiomass"] <- sum(pop$mass[pop$stage == "Juvenile"], na.rm = TRUE)
temp[j, "meanLength"] <- mean(pop$length, na.rm = TRUE)
temp[j, "cvLength"] <- var(pop$length, na.rm = TRUE)/mean(pop$length, na.rm = TRUE)
temp[j, "meanlRepro"] <- mean(pop$lRepro, na.rm = TRUE)
temp[j, "cvlRepro"] <- var(pop$lRepro, na.rm = TRUE)/mean(pop$lRepro, na.rm = TRUE)


}
  
   
  out <- rbind(out, temp)

}

#harvest_out <- out
#no_harvest_out <- out


my_theme <- theme_bw()+
            theme(panel.grid = element_blank(),
                  axis.text = element_text(size = 40, color = "white"),
                  axis.title = element_text(size = 48, color = "white"),
                  plot.background = element_rect(fill = "black"),
                  panel.background = element_rect(fill = "black"),
                  axis.line = element_line(color = "white", size = 1),
                  panel.border = element_blank())
 
# Build blank graph first for numbers


out %>%
  ggplot()+
  geom_line(aes(x = timestep, y = log(Adults), group = nSim), color = "black")+
  geom_line(aes(x = timestep, y = log(Juveniles), group = nSim), color = "black")+
  labs(x = "Timestep", y = "N")+
  my_theme


# Numbers of adults and juveniles, no harvest

no_harvest_out %>%
    ggplot()+
    geom_line(aes(x = timestep, y = log(Adults), group = nSim), color = "darkorange3")+
    geom_line(aes(x = timestep, y = log(Juveniles), group = nSim), color = "cyan")+
    coord_cartesian(ylim = c(0,9)) +
    scale_y_continuous(breaks = seq(2, 8, 2)) +
    labs(x = "Timestep", y = "N(log)") +
    my_theme

# Numbers of adults and juveniles, harvest

harvest_out %>%
  ggplot()+
  geom_line(aes(x = timestep, y = log(Adults), group = nSim), color = "darkorange3")+
  geom_line(aes(x = timestep, y = log(Juveniles), group = nSim), color = "cyan")+
  coord_cartesian(ylim = c(0,9)) +
  scale_y_continuous(breaks = seq(2, 8, 2)) +
  labs(x = "Timestep", y = "N(log)") +
  my_theme

# Biomass no harvest

no_harvest_out %>%
  ggplot()+
  geom_line(aes(x = timestep, y = log(AdultBiomass), group = nSim), color = "darkorange3")+
  geom_line(aes(x = timestep, y = log(JuvenileBiomass), group = nSim), color = "cyan")+
  geom_line(aes(x = timestep, y = log(smallR), group = nSim), color = "grey")+
  geom_line(aes(x = timestep, y = log(bigR), group = nSim), color = "white")+
  labs(x = "Timestep", y = "Biomass")+
  my_theme

# Biomass harvest

harvest_out %>%
  ggplot()+
  geom_line(aes(x = timestep, y = log(AdultBiomass), group = nSim), color = "darkorange3")+
  geom_line(aes(x = timestep, y = log(JuvenileBiomass), group = nSim), color = "cyan")+
  geom_line(aes(x = timestep, y = log(smallR), group = nSim), color = "grey")+
  geom_line(aes(x = timestep, y = log(bigR), group = nSim), color = "white")+
  labs(x = "Timestep", y = "Biomass")+
  my_theme

# no harvest size at first reproduction

no_harvest_out %>%
  ggplot()+
  geom_line(aes(x = timestep, y = (meanlRepro), group = nSim),
            size = 1.5, alpha = 0.3, color = "white") +
  labs(x = "Timestep", y = "1st reproduction length") +
  coord_cartesian(ylim = c(180, 215)) +
  my_theme

# Harvest size at first reproduction

harvest_out %>%
  ggplot()+
  geom_line(aes(x = timestep, y = (meanlRepro), group = nSim),
            size = 1.5, alpha = 0.3, color = "white") +
  labs(x = "Timestep", y = "1st reproduction length") +
  coord_cartesian(ylim = c(180, 215)) +
  my_theme

# Two smoothed lines

  ggplot()+
  geom_line(aes(x = timestep, y = (meanlRepro), group = nSim),
            size = 1.5, color = "white", data = no_harvest_out) +
  geom_line(aes(x = timestep, y = (meanlRepro), group = nSim),
             size = 1.5, color = "grey69", data = harvest_out) +
  labs(x = "Timestep", y = "1st reproduction length") +
  coord_cartesian(ylim = c(180, 215)) +
  my_theme


# fake haphazard sampling designs to build informative prior
# restructuring of population can possibly create resilience to FIE.


# Need to add biomass tracking, start fiddling with resource levels, actually test what we wanted to test.
# get biomass levels from literature. Or just guess... or make it 10X real biomass levels.