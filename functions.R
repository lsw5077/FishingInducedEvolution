# Functions

## Supporting Functions

`%notin%` <- Negate(`%in%`)

# add in probability-driven harvest and mortality, maybe starvation

### von Bertanalffy function for finding sizes under baseline assumptions of maturing around 2-4 years old

vonB <- function(lRepro,k,t){
  Lt = lRepro*(1 - exp(-k*t))
  
  return(Lt)
  
}

### finding mass from length

findMass <- function(massConstant,
                     massMultiplier,
                     length){
  
  logmass = massConstant + (massMultiplier*log10(length))
  mass = 10^logmass
  
  return(mass)
}

### finding length from mass

findLength <- function(massConstant,
                       massMultiplier,
                       mass){
  
  loglength = (log10(mass) - massConstant)/massMultiplier
  
  length = 10^loglength

  return(length)
}



#### predicting portion of mass-specific attack rate dedicated to small and large prey

forageCurve <- function(length,
                        maxPropFish,
                        k, # steepness constant,
                        x0 # x value of midpoint
){
  
  propFish = maxPropFish/(1 + exp(-k*(length - x0)))
  
  return(propFish)
  
}


## Process Functions

### Prey reproduce

dRdt <- function(r, # instantaneous rate of increase
                 K, # prey biomass carrying capacity
                 R,# prey biomass
                 WS){ # biomass consumed
                  
  dR =  ifelse(R == 0, 0,
              (r * R * (1 - R/K)) - WS )
    

  # Maybe replace with logistic model.

  
  # change in R from reproduction and being eaten. Assume no other mortality for now
  # ro is per unit biomass
  # everything calculated in terms of biomass turnover 
  # Look at literature/ check out what realistic values of turnover should be, >= 1?
  
  R = ifelse((R + dR) < 0, 0, (R + dR))
  
  return(R)

}

### Harvest

harvest <- function(pop, # population from which to harvest
                    harvestIntensity, # proportion of total fish population to harvest
                    lowerLimit, # lower length limit
                    upperLimit){ # upper length limit
  
  nHarvest <- round(harvestIntensity*dim(pop)[1])
  
  sizePop <- pop %>%
             filter(length >= lowerLimit, length <= upperLimit) 
  
  # will harvest at given intensity unless there are too few fish, 
  # in which case it will harvest all within size bounds.
  
  if (dim(sizePop)[1] < nHarvest){ 
    
    harvested <- sizePop} else{
      
    harvested <-  sizePop %>%
                  sample_n(nHarvest) 
      
    }

    popOut <- pop %>%
              filter(ID %notin% harvested$ID)
    
    return(popOut)

}
                    
### Forage

#### Biomass to growth and biomass to repro get assigned up here as function of asymptotic size.
#### we'll have a static growth efficiency for now, can update to function later.

# Let's add in a quick and dirty size-competition value

forage <- function(pop,
                   smallR,
                   bigR,
                   eGrowth,
                   eRepro,
                   maxPropFish,
                   k,
                   x0,
                   a,
                   h){
  
  
  # take integral over continuous, i.e. foraging events on course of season.
  
  # how do we make a personalized interference term?
  # maybe a mass-specific interference term?
  
  
# f = aC^mR/(1 + aC^mhR)
  
  pop_size = nrow(pop)
  
# Just make interference dependent upon the number of fish for now. Can change to biomass later.
  pop %>%
    #rowwise() %>%
    mutate(propBig = forageCurve(length = length,
                                 maxPropFish = maxPropFish,
                                 k = k,
                                 x0 = x0),
           bigA = a*propBig,
           bigH = h*(propBig),
           smallA = a*(1-propBig),
           smallH = h*(1-propBig)) %>% 
    #ungroup() %>%
    mutate(bigBiomassForaged = bigA * bigR * mass /
                              (1 + bigA * (bigH + int_constant * pop_size) * bigR),
           smallBiomassForaged = smallA * smallR * mass/
                              (1 + smallA * (smallH + int_constant * pop_size) * smallR),
           biomassToGrowth = ifelse(length < lRepro, (bigBiomassForaged + smallBiomassForaged)*eGrowth, 0),
           biomassToRepro = ifelse(length < lRepro, biomassToRepro,
                                   biomassToRepro +((bigBiomassForaged + smallBiomassForaged)*eRepro))) -> popOut
  
  # Functional responses happen at the indivual level.
  # We should not be adding them.
  # Functional responses are instantaneous. Everyone experiences the same inital prey density.
  
  # Apply functional response to each individual, then add up all individuals' foraging.
  # Thinking of attack rate and handling time on annual scale = crude version of integral model
  
  # CHANGE TO SIZE AT MATURITY
# 
#   bigA <- sum(predPop$bigA, na.rm = T)
#   bigH <- sum(predPop$bigH, na.rm = T)
# 
#   smallA <- sum(predPop$smallA, na.rm = T)
#   smallH <- sum(predPop$smallH, na.rm = T)
#   
#   C <- sum(pop$mass)
# 
#   bigWS <- bigA*bigR*C/(1+bigA*bigH*bigR)
#   smallWS <- smallA*smallR*C/(1+smallA*smallH*smallR)
#   
  bigWS <<- sum(popOut$bigBiomassForaged, na.rm = TRUE)
  smallWS <<- sum(popOut$smallBiomassForaged, na.rm = TRUE)
  
  # Can't have half a prey. work that out somewhere or go to all biomass.
  
  # now assign biomass based on size.
  # biomass from growth gets used up each time.
  # repro biomass can be reserved for future use.
  
  
  # switch vs. slow transition to repro growth probably doesn't make a huge difference/ add any inference 
  
# popOut <- predPop %>% 
#           mutate(bigBiomassForaged = bigWS*propBig*mass/sum(mass),
#                  smallBiomassForaged = smallWS*(1-propBig)*mass/sum(mass),
#                  biomassToGrowth = ifelse(length < 0.75*lAssym, (bigBiomassForaged + smallBiomassForaged)*eGrowth,
#                                           (bigBiomassForaged + smallBiomassForaged)*eGrowth*((lAssym - length)/lAssym)),
#                  biomassToRepro = ifelse(length < 0.75*lAssym, biomassToRepro,
#                                          biomassToRepro +
#                                          ((bigBiomassForaged + smallBiomassForaged)
#                                          *eRepro*
#                                          (1-((lAssym - length)/lAssym))
#                        )
#                     )
#                  )
      

return(popOut)
  
}


### Growth

grow <- function(pop, 
                 massConstant,
                 massMultiplier) { 
  
  pop %>%
    rowwise() %>%
    mutate(mass = mass + biomassToGrowth,
           length = findLength(massConstant = massConstant,
                               massMultiplier = massMultiplier,
                               mass = mass),
           age = age + 1,
           stage = ifelse(length >= lRepro, "Adult", "Juvenile"),
           biomassToGrowth = 0,
           smallBiomassForaged = 0,
           bigBiomassForaged = 0) -> popOut
  
  return(popOut)
    
  }


### Natural Death

#### Right now, we'll set death up as static rates for adults and juveniles. 
#### Can change it to a function later.

naturalDeath <- function(pop,
                         adultMortRate,
                         juvMortRate){
  
  liveAdults <- pop %>%
    filter(stage == "Adult") %>%
    sample_frac(1 - adultMortRate)
  
  liveJuvs <- pop %>%
    filter(stage == "Juvenile") %>%
    sample_frac(1 - juvMortRate)
  
  popOut <- rbind (liveAdults, liveJuvs)
  
  return(popOut)
  
}

### Repro

repro <- function(pop,
                  h2,
                  #trait, # add in later. Hard-code to asymptotic size for now.
                  offspringMass,
                  massConstant,
                  massMultiplier){
  
  # grab reproductive individuals
  
  pop %>% rowwise() %>%
    mutate(nOffspring = round(biomassToRepro/offspringMass)) %>%
    filter(nOffspring > 0) -> offspring
  
  if (dim(offspring)[1] == 0){
   
     pop %>%
      rbind(offspring) %>%
      mutate(biomassToRepro = biomassToRepro - (round(biomassToRepro/offspringMass))) %>%
      as.data.frame() %>%
      mutate(ID = row_number()) -> popOut
    
    return(popOut)
    
    } else{

  
  # make new babies and assign them traits
      
  pop_sd <- sd(pop$lRepro, na.rm = TRUE)
  pop_mean <- mean(pop$lRepro, na.rm = TRUE)
  
  offspring[rep(seq(nrow(offspring)), offspring$nOffspring),] %>%
    rowwise() %>%
    mutate(lRepro = rnorm(n = 1,
                          mean = (lRepro*h2 + lStartMean*(1 - h2)),
                          sd = (h2*pop_sd + (1 - h2)*lStartSD)),
           mass = offspringMass,
           length = findLength(massConstant = massConstant,
                               massMultiplier = massMultiplier,
                               mass = offspringMass),
           stage = "Juvenile",
           biomassToGrowth = 0,
           biomassToRepro = 0,
           age = 0,
           bigA = NA,
           bigH = NA,
           smallA = NA,
           smallH = NA,
           propBig = NA) %>%
           dplyr::select(-nOffspring) -> offspring
  
  # join back to df.
  # We'll let individuals maintain repro biomass if they don't use it.
  
  pop %>%
    rbind(offspring) %>%
    mutate(biomassToRepro = biomassToRepro - (round(biomassToRepro/offspringMass))) %>%
    as.data.frame() %>%
    mutate(ID = row_number()) -> popOut
      
  return(popOut)
  
      }
}

