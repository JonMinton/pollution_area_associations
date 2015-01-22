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

# correlation between 2001 and 2008 simd?

simd_2009 <- simd_2009 %>% 
    select(-simd_type) %>% 
    group_by(datazone) %>% 
    gather(-datazone, key=year, value=simd) %>% 
    filter(!is.na(simd))

simd_2009 <- simd_2009 %>% spread(key=year, value=simd)
simd_2009 <- simd_2009 %>% dplyr::rename(2001=simd_2001, 2008=simd_2008)
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

# > lm(pm10 ~ inc_deprivation, data=joined) %>% summary()
# 
# Call:
#     lm(formula = pm10 ~ inc_deprivation, data = joined)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4.8277 -1.0726  0.0898  1.1551  3.9237 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     10.018307   0.019157   523.0   <2e-16 ***
#     inc_deprivation  0.034135   0.001019    33.5   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.632 on 19426 degrees of freedom
# Multiple R-squared:  0.05462,    Adjusted R-squared:  0.05457 
# F-statistic:  1122 on 1 and 19426 DF,  p-value: < 2.2e-16
# 
# > lm(pm10 ~ inc_deprivation, data=joined, subset=year==2002) %>% summary()
# 
# Call:
#     lm(formula = pm10 ~ inc_deprivation, data = joined, subset = year == 
#            2002)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.5662 -1.1556  0.0728  1.1859  3.0548 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     10.894664   0.028295  385.04   <2e-16 ***
#     inc_deprivation  0.032244   0.001464   22.03   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.422 on 6433 degrees of freedom
# Multiple R-squared:  0.07015,	Adjusted R-squared:   0.07 
# F-statistic: 485.3 on 1 and 6433 DF,  p-value: < 2.2e-16
# 
# > lm(pm10 ~ inc_deprivation, data=joined, subset=year==2005) %>% summary()
# 
# Call:
#     lm(formula = pm10 ~ inc_deprivation, data = joined, subset = year == 
#            2005)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4.8729 -1.1561 -0.0835  1.3019  4.4643 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     9.270329   0.034934  265.37   <2e-16 ***
#     inc_deprivation 0.050661   0.001949   25.99   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.726 on 6486 degrees of freedom
# Multiple R-squared:  0.09433,	Adjusted R-squared:  0.09419 
# F-statistic: 675.6 on 1 and 6486 DF,  p-value: < 2.2e-16
# 
# > lm(pm10 ~ inc_deprivation, data=joined, subset=year==2008) %>% summary()
# 
# Call:
#     lm(formula = pm10 ~ inc_deprivation, data = joined, subset = year == 
#            2008)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4.2543 -0.8497  0.3489  1.0620  3.0339 
# 
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     9.941806   0.028410  349.94   <2e-16 ***
#     inc_deprivation 0.017953   0.001486   12.08   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.355 on 6503 degrees of freedom
# Multiple R-squared:  0.02196,	Adjusted R-squared:  0.02181 
# F-statistic:   146 on 1 and 6503 DF,  p-value: < 2.2e-16


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

mod_02 <- mod_01 %>% update(
    . ~ . + social
    )

mod_03 <- lm(pm10 ~ inc_deprivation * social, 
             data=joined)


mod_social <- lm(
    pm10 ~ inc_deprivation * social, 
    data=joined
    )

mod_rental <- lm(
    pm10 ~ inc_deprivation * rented,
    data=joined
    )

mod_owner <- lm(
    pm10 ~ inc_deprivation * owned,
    data=joined
    )

# now to normalise deprivation levels on a 0-1 scale

j2 <- joined %>% mutate(inc_deprivation=(inc_deprivation - min(inc_deprivation))/(max(inc_deprivation)-min(inc_deprivation)))

# This *might* be helpful for understanding the relative importance of the interaction
# between household type proportions and income deprivation

norm_social <- lm(
    pm10 ~ inc_deprivation * social, 
    data=j2
)

norm_rental <- lm(
    pm10 ~ inc_deprivation * rented,
    data=j2
)

norm_owner <- lm(
    pm10 ~ inc_deprivation * owned,
    data=j2
)

###################################################################################

# As a crude measure of mix, let's use the produce of the three proportions 
# of household type, then normalised to a 0-1 scale

j3 <- joined %>% mutate(mix=owned *rented * social) %>% mutate(mix =mix/max(mix))

qplot(x=mix, data=j3)

qplot(x=inc_deprivation, y=mix, data=j3) + stat_smooth()
qplot(y=inc_deprivation, x=mix, data=j3) + stat_smooth()

