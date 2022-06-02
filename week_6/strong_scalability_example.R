#-----------------------------------------------------------------------------#
# setup
#-----------------------------------------------------------------------------#

require(data.table)
require(raster)
require(readr)

input_file = '' # input file

#-----------------------------------------------------------------------------#
# specify number of runs and chunk size
#-----------------------------------------------------------------------------#

# size of file
file_size = file.size(input_file)/1e+6

# nr of runs to complete
nruns = ceiling(file_size/1)

# number of rows in original file
rows = length(count.fields(input_file))

# number of rows to process per iteration
nrows = rep(floor(rows/nruns), nruns)

# starting indexes of eac iteration
index = unname(sapply(1:nruns, function(r) nrows[r]*(r-1)))

# update last value if needed
nrows[nruns] = nrows[nruns] + (rows-(index[nruns]+nrows[nruns]))

#-----------------------------------------------------------------------------#
# count number of samples per year across chunks
#-----------------------------------------------------------------------------#

years = 2000:2022
year_count = vector('numeric', length(years))

for (r in 1:nruns) {
  samples = read_csv(input_file, n_max=nrows[r], skip=index[r])
  year_count = year_count + sapply(years, function(y) sum(samples$year == y))
}

#-----------------------------------------------------------------------------#
# plot distribution of samples per year
#-----------------------------------------------------------------------------#

gdf = data.frame(year=years, count=year_count)
gdf$relative_count = gdf$count / sum(gdf$count) * 100

ggplot(gdf, aes(x=year, y=relative_count)) + theme_bw() + 
  geom_bar(stat='identity', fill='grey60') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  coord_cartesian(xlim=c(min(gdf$year),max(gdf$year)+1), ylim=c(0,30)) + 
  labs(x='Observation year', y='Sample frequency (%)') + 
  theme(panel.background=element_blank(), 
        panel.grid=element_blank(), 
        panel.spacing = unit(2, "lines"))
