Poverty data is also released by census tract, meaning that we can look at poverty rates within towns. The maps below display poverty rates for census tracts in Hartford and Tolland counties, for the most recent ACS data (2011 5-year estimates). 

```{r echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10}
library(acs)
library(maps)
library(maptools)
key = "ba67d3a427e1f785987b9c8bc59341bf7c8a7cc1"
api.key.install(key)
hartford.tracts = geo.make(state = "CT", county = c("Hartford","Tolland"), tract = "*", check = T)
B17017 = acs.fetch(geography = hartford.tracts, table.number = "B17017", col.names = "pretty")
poverty.estimate = divide.acs(numerator=B17017[,2], denominator=B17017[,1], method="proportion")
poverty = data.frame(tract=geography(B17017)[[1]], poverty=as.numeric(estimate(poverty.estimate)))
poverty$tract= gsub("Census Tract ", "", poverty$tract)
poverty$tract= gsub(", Tolland County, Connecticut", "", poverty$tract)
poverty$tract= gsub(", Hartford County, Connecticut", "", poverty$tract)
#Load the UConn tract and town-level shapefiles
CTTracts <- readShapeSpatial(fn="../tractct_37800_0000_2010_s100_census_1_shp/wgs84/tractct_37800_0000_2010_s100_census_1_shp_wgs84")
CTTracts <- fortify(CTTracts, region = "NAME10")
CTTracts <- CTTracts[order(CTTracts$order),]
#Merge with poverty data
choropleth2=merge(CTTracts, poverty, by.x = "id", by.y="tract")
choropleth2=choropleth2[order(choropleth2$order), ]
choropleth2$poverty=cut(choropleth2$poverty, 
                        breaks=c(0,.1,.2,.3,.4), include.lowest=T)
#Make the map
ggplot(choropleth2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = poverty)) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = NULL, y = NULL) + 
  coord_equal() +
  geom_polygon(data = CTTowns, colour = "grey", alpha = 0.5, fill = NA) +
  scale_fill_brewer(palette = "Purples", name = "Poverty\nrate") +
  theme_minimal()
```

Here is another view of the same data, with a different way of coloring the same map. 

```{r echo = FALSE, warning = FALSE, message = FALSE, fig.width = 10}
#Merge with poverty data
choropleth2=merge(CTTracts, poverty, by.x = "id", by.y="tract")
choropleth2=choropleth2[order(choropleth2$order), ]
choropleth2$poverty=cut(choropleth2$poverty, 
                        breaks=c(0,.04,.07,.16,.4), include.lowest=T)
#Make the map
ggplot(choropleth2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = poverty)) + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = NULL, y = NULL) + 
  coord_equal() +
  geom_polygon(data = CTTowns, colour = "grey", alpha = 0.5, fill = NA) +
  scale_fill_brewer(palette = "Purples", name = "Poverty\nrate") +
  theme_minimal()
```

Similar maps could be produced for the other poverty measures currently available or with demographic breakouts. Brookings and others have looked at 'concentrated poverty' measures and identified areas of concentrated or racially concentrated poverty (generally based on combined thresholds for poverty and racial segregation). 
