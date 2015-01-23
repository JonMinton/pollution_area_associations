# 22/1/2015

# Script to explore ecological associations between pollution estimated at datazone level 
# and socioeconomic characteristics of neighbourhoods 

# n.b. using packrat to manage dependencies 

# Data to include

# pollution
## air pollution  - latest year
## air pollution  - 2001 (ish)

# sns
#  simd
#  tenure
#  derelict sites


# create file to fetch

require(repmis)
require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)


# simd vars

simd_2009 <- source_DropboxData(
    file="simd_2009.csv",
    key="ghiu8n9db6rch9y"    
    ) %>% tbl_df() 
simd_2009 <- simd_2009 %>% gather("simd_type", "simd_value", c(2,4,5))

qplot(data=simd_2009, x=simd_value, facets = . ~ year )
# clear no real values for 2007, so drop

simd_2009 <- simd_2009 %>% filter(year !=2007)

qplot(data=simd_2009, x=simd_value, facets = . ~ year)

# correlation between 2001 and 2008 simd?

simd_2009 <- simd_2009 %>% 
    select(-simd_type) %>% 
    group_by(datazone) %>% 
    filter(!is.na(simd_value)) %>% 
    spread(key=year, value=simd_value) 

names(simd_2009)[2:3] <- c("simd_2001", "simd_2008")

qplot(data=simd_2009,
      x=simd_2001, y=simd_2008
) + stat_smooth()


# OK - now pollution for same years

pollution <- source_DropboxData(
    file="pollution_by_datazone.csv",
    key="dziu2mknif22rmp"    
) %>% tbl_df() 

#is pm10 available for both years?
# yes
# pm2.5 first available from 2003

# keep pm10, datazone, year
pm10 <- pollution %>% 
    filter(year== 2001 | 2008) %>%
    select(datazone, year, pm10)

simd <- simd_2009 %>% gather(key=year, value=simd, -datazone)
# move simd back to long format for better merging
simd$year <- as.character(simd$year)
simd$year[simd$year=="simd_2001"] <- "2001"
simd$year[simd$year=="simd_2008"] <- "2008"
simd$year <- as.numeric(simd$year)

joined <- simd %>% inner_join(pm10)

qplot(
    x=simd,
    y=pm10,
    facets = . ~ year,
    data=joined
    )

# main differences: overall rates from 2001-2008
# range of simds increased (different categories?)
# no obvious association in either year 

lm(pm10 ~ simd, data=joined) %>% summary()

lm(pm10 ~ simd, data=joined, subset=year==2001) %>% summary()
lm(pm10 ~ simd, data=joined, subset=year==2008) %>% summary()



# Now let's look at income deprivation
income_deprivation <- source_DropboxData(
    file="income_deprivation.csv",
    key="tp66cm0xppfh07b"    
) %>% tbl_df() %>% 
    filter(!is.na(CS.incdeprived)) %>% 
    rename(inc_deprivation=CS.incdeprived)

# from 2002 to 2008

# do a left join with income_deprivation as first argument

joined <- income_deprivation %>% left_join(pollution)

# pm2.5 both years

qplot(
    x=inc_deprivation, 
    y=pm2.5,
    facets= . ~ year,
    data=joined
    )

# no obvious association 

qplot(
    x=inc_deprivation, 
    y=pm10,
    facets= . ~ year,
    data=joined
)


# However, looking at it with linear models:

lm(pm10 ~ inc_deprivation, data=joined) %>% summary()
lm(pm10 ~ inc_deprivation, data=joined, subset=year==2002) %>% summary()
lm(pm10 ~ inc_deprivation, data=joined, subset=year==2005) %>% summary()
lm(pm10 ~ inc_deprivation, data=joined, subset=year==2008) %>% summary()

# i.e. statistically significant association in each year
# but adjusted R-squared values are VERY low


########## Now - spri sites 
# (special pollution releases inventory)



# CS-CSdSPRIdpctpop2    Population within 500m of a site on the Scottish Pollutant Release Inventory
# CS-CSdSPRIdpctpop4	Population within 1000m of a site on the Scottish Pollutant Release Inventory
# CS-CSdSPRIdpctpop6	Population within 2000m of a site on the Scottish Pollutant Release Inventory
# CS-CSdSPRIdpctpop8	Population over 2000m from a site on the Scottish Pollutant Release Inventory


spri_proximity <- source_DropboxData(
    file="proximity_to_a_spri_site.csv",
    key="h9uqboghy1up0bj"    
) %>% tbl_df() %>%
    rename(
        pop_within_500m=CS.CSdSPRIdpctpop2,
        pop_within_1000m=CS.CSdSPRIdpctpop4,
        pop_within_2000m=CS.CSdSPRIdpctpop6,
        pop_over_2000m=CS.CSdSPRIdpctpop8
        )

# both income deprivation and spri proximity are available in 2005

joined <- income_deprivation %>% inner_join(spri_proximity)

# really want proportions
joined <- joined %>% mutate(
    total_pop = pop_within_500m + pop_within_1000m + pop_within_2000m + pop_over_2000m
                                )
joined <- joined %>% transmute(
    datazone=datazone, inc_deprivation=inc_deprivation,
    prop_within_500m = pop_within_500m / total_pop, 
    prop_within_1000m = pop_within_1000m / total_pop,
    prop_within_2000m = pop_within_2000m / total_pop,
    prop_over_2000m = pop_over_2000m / total_pop
    )

qplot(
    x=inc_deprivation,
    y= prop_within_500m,
    data=joined
    ) + stat_smooth(method="lm")
# very slight effect

qplot(
    x=inc_deprivation,
    y= prop_within_1000m,
    data=joined
) + stat_smooth(method="lm")
# stat sig effect

qplot(
    x=inc_deprivation,
    y= prop_within_2000m,
    data=joined
) + stat_smooth(method="lm")

qplot(
    x=inc_deprivation,
    y= prop_over_2000m,
    data=joined
) + stat_smooth(method="lm")

# Now that we have some evidence of disproportionate siting, albeit only in 2005, we can look at 
# whether the tenure make-up of an area mediates this effect

tenure_households <- source_DropboxData(
    file="tenure_households.csv",
    key="kng5wc40le9kapj"    
) %>% tbl_df() %>% select(
    datazone, year, 
    all_households=HO.allhouseholds,
    council_houses=HO.council,
    rented_from_employer=HO.employ,
    owned_with_mortgage=HO.ownmortloan,
    owned_outright=HO.ownoutright,
    private_rented=HO.privlet,
    rented_from_relative=HO.relative,
    shared_ownership=HO.sharedown,
    other_social_rented=HO.social
    ) %>% 
    mutate(
        social=council_houses + other_social_rented,
        rented=rented_from_employer + private_rented+ rented_from_relative,
        owned=owned_with_mortgage + owned_outright + shared_ownership
        ) %>%
    mutate(
        council_houses=council_houses/all_households,
        rented_from_employer=rented_from_employer/all_households,
        owned_with_mortgage=owned_with_mortgage/all_households,
        owned_outright=owned_outright/all_households,
        private_rented=private_rented/all_households,
        rented_from_relative=rented_from_relative/all_households,
        shared_ownership=shared_ownership/all_households,
        other_social_rented=other_social_rented/all_households,
        social = social/all_households,
        rented = rented/all_households,
        owned=owned/all_households
        )


# then to pollution

# tried income_deprivation: no common year
# but there is for simd. for 2001

joined <- tenure_households %>% inner_join(simd)
joined <- pollution %>% select(datazone, year, pm10) %>% inner_join(joined)


qplot(
    x=simd,
    y=social,
    data=joined
    )
# Strong positive association - but isn't this by design? (component of SIMD)
# Not within the 2012 SIMD - not sure about older types of SIMD

# What about rented?

qplot(
    x=simd,
    y=rented,
    data=joined
)
# no clear assocaition

qplot(
    x=simd,
    y=owned,
    data=joined
)
# strong negative association

# This seems reassuring as to the choice of categories applied. 


# Found a variable on ambient air quality _ There SHOULD be a strong correlation 
# between this and our pollution estimates!


# CS-AQdNO2dpopwtd1    Air Quality 2002-2004 - Nitrogen Dioxide concentration - Population weighted	NO2 concentration 2002-2004	ratio
# CS-AQdPM10dpopwtd1	Air Quality 2002-2004 - PM10 concentration - Population weighted	PM10 concentration 2002-2004	ratio

ambient_air_quality <- source_DropboxData(
    file="ambient_air_quality.csv",
    key="4gzgg8r3174xhfh"
    ) %>% tbl_df() %>%
    select(
        datazone, year,
        pop_weighted_no2=CS.AQdNO2dpopwtd1,
        pop_weighted_pm10=CS.AQdPM10dpopwtd1
        )

# If pop weighted we should also include population counts

populations <- source_DropboxData(
    file="persons.csv",
    key="vcz7qngb44vbynq"
    ) %>% tbl_df() %>% select(
        datazone, year,
        GR.hspeop,
        GR.sapepeop
        )

populations <- populations %>% mutate(
    population_count=ifelse(is.na(GR.sapepeop), GR.hspeop, GR.sapepeop)
    ) %>% select(datazone, year, population_count)

joined <- populations %>% 
    inner_join(ambient_air_quality) %>% 
    inner_join(pollution)


qplot(
    x=pm10,
    y=pop_weighted_pm10,
    data=joined
    )
# R2 of 0.84
# Suggests we did something similar to what was done before

qplot(
    x=no2,
    y=pop_weighted_no2,
    data=joined
)
# R2 of 0.91


##############################################################################################################
# To do now:

# regression: pm10 by income deprivation, 
# effect of adding prop social housing as a covariate

joined <- pollution %>% 
    inner_join(income_deprivation)

# income deprivation is available for 2002, 2005, 2008
# tenure_households is available for 2001
# pollution includes 2002 among other years

p_tmp <- pollution %>% filter(year==2002) %>% select(-year)
i_tmp <- income_deprivation %>% filter(year==2002) %>% select(-year)
t_tmp <- tenure_households %>% filter(year==2001) %>% select(-year)

joined <- p_tmp %>% inner_join(i_tmp) %>% inner_join(t_tmp)

    
mod_01 <- lm(
    pm10 ~ inc_deprivation,
    data=joined)
summary(mod_01)

mod_02 <- mod_01 %>% update(
    . ~ . + social
    )
summary(mod_02)

mod_03 <- lm(pm10 ~ inc_deprivation * social, 
             data=joined)
summary(mod_03)

mod_social <- lm(
    pm10 ~ inc_deprivation * social, 
    data=joined
    )
summary(mod_social)

mod_rental <- lm(
    pm10 ~ inc_deprivation * rented,
    data=joined
    )
summary(mod_rental)

mod_owner <- lm(
    pm10 ~ inc_deprivation * owned,
    data=joined
    )
summary(mod_owner)

joined$social_quartile <- joined$social %>% ntile(4)
qplot(
    y=pm10,
    x=inc_deprivation,
    colour=social,
    data=joined
    ) + scale_colour_gradient(limits=c(0,1)) + 
    geom_smooth(data=subset(joined, subset=social_quartile==4), method="lm")
    

# now to normalise deprivation levels on a 0-1 scale

j2 <- joined %>% mutate(inc_deprivation=(inc_deprivation - min(inc_deprivation))/(max(inc_deprivation)-min(inc_deprivation)))

# This *might* be helpful for understanding the relative importance of the interaction
# between household type proportions and income deprivation

norm_social <- lm(
    pm10 ~ inc_deprivation * social, 
    data=j2
)
summary(norm_social)

norm_rental <- lm(
    pm10 ~ inc_deprivation * rented,
    data=j2
)
summary(norm_rental)

norm_owner <- lm(
    pm10 ~ inc_deprivation * owned,
    data=j2
)
summary(norm_owner)
###################################################################################

# As a crude measure of mix, let's use the produce of the three proportions 
# of household type, then normalised to a 0-1 scale

j3 <- joined %>% mutate(mix=owned *rented * social) %>% mutate(mix =mix/max(mix))

qplot(x=mix, data=j3)

qplot(x=inc_deprivation, y=mix, data=j3) + stat_smooth()
qplot(y=inc_deprivation, x=mix, data=j3) + stat_smooth()


mix_social <- lm(
    pm10 ~ inc_deprivation * social *mix, 
    data=j3
)
summary(mix_social)

mix_rental <- lm(
    pm10 ~ inc_deprivation * rented * mix,
    data=j3
)
summary(mix_rental)

mix_owner <- lm(
    pm10 ~ inc_deprivation * owned * mix,
    data=j3
)
summary(mix_owner)

# Renove 3 way interaction to make interpretation simpler
summary(lm(pm10 ~ inc_deprivation*social + mix, data=j3))
summary(lm(pm10 ~ inc_deprivation*owned + mix, data=j3))
summary(lm(pm10 ~ inc_deprivation*rented + mix, data=j3))


### One additional thing: what if we encode proportion social housing as a colour with 
# y: pm10 
# x: income deprivation

joined <- income_deprivation %>% inner_join(tenure_households) %>% inner_join(pollution) 
