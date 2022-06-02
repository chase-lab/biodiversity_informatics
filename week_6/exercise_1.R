#-----------------------------------------------------------------------------#
# setup
#-----------------------------------------------------------------------------#

setwd('exercise_1/')

require(ggplot2)
require(raster)
require(readr)
require(sf)

#-----------------------------------------------------------------------------#
# load data
#-----------------------------------------------------------------------------#

# lion ranges
sp = st_read('00_data/IUCN/IUCN-panthera_leo.shp')

# environmental data
canopy_density_2000 = raster('00_data/GFC/GFC-canopyDensity_20000000_1deg.tif')
canopy_density_2018 = raster('00_data/GFC/GFC-canopyDensity_20180000_1deg.tif')
canopy_change = canopy_density_2018 - canopy_density_2000

#-----------------------------------------------------------------------------#
# derive samples
#-----------------------------------------------------------------------------#

# reference grid
r = raster(extend(extent(extent(sp)), 1), res=1)

# rasterize polygons
p = raster::rasterize(as(sp, 'Spatial'), r, 'PRESENCE')

# extract samples
ind = which.max(p > 0)
xy = as.data.frame(xyFromCell(r, ind))
xy$presence = extract(p, xy)
xy$class = ''
xy$class[which(xy$presence == 1)] = 'Extant'
xy$class[which(xy$presence == 4)] = '(Possibly) extinct'
xy$class = factor(xy$class, levels=c('Extant','(Possibly) extinct'))

#-----------------------------------------------------------------------------#
# extract data
#-----------------------------------------------------------------------------#

samples_2000 = extract(canopy_density_2000, xy[,c('x','y')])
samples_2018 = extract(canopy_density_2018, xy[,c('x','y')])
samples_change = extract(canopy_change, xy[,c('x','y')])

#-----------------------------------------------------------------------------#
# compare distributions for 2000 and 2018
#-----------------------------------------------------------------------------#

# build data.frame to plot
gdf = rbind(
  data.frame(density=samples_2000, group='2000', presence=xy$class), 
  data.frame(density=samples_2018, group='2018', presence=xy$class)
)

ggplot(gdf, aes(x=density, fill=presence)) + theme_bw() + 
  geom_histogram(binwidth=5, alpha=0.4, aes(y=(..count..)/sum(..count..))) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  coord_cartesian(xlim=c(0,100)) + 
  scale_fill_manual(values=c('green4','orange3')) + 
  theme(panel.background=element_blank(), 
        panel.grid=element_blank(), 
        panel.spacing = unit(2, "lines")) + 
  facet_wrap(~group)

ggsave('01_analysis/range_analysis/canopy_density_2000-2018.csv', 
       width=12, height=7, dpi=300)

#-----------------------------------------------------------------------------#
# compare distribution of changes
#-----------------------------------------------------------------------------#

gdf = data.frame(density=samples_change, presence=xy$class)

ggplot(gdf, aes(x=density, fill=presence)) + theme_bw() + 
  geom_histogram(binwidth=0.5, alpha=0.4, aes(y=(..count..)/sum(..count..))) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  coord_cartesian(xlim=c(-10,10)) + 
  scale_fill_manual(values=c('green4','orange3')) + 
  theme(panel.background=element_blank(), 
        panel.grid=element_blank(), 
        panel.spacing = unit(2, "lines"))

ggsave('01_analysis/range_analysis/canopy_change_2000-2018.csv', 
       width=12, height=7, dpi=300)

# collect the trash
gc()
