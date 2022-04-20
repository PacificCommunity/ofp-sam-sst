
# Nicholas Ducharme-Barth
# 05/03/2019
# pull sst data from https://podaac.jpl.nasa.gov/dataset/REYNOLDS_NCDC_L4_MONTHLY_V5
# capture pacific wide data begining in 1950 (100E to 70W) or 100 to 290 lon, and -60 to 60 lat
# create a sst.raster.list and storage.df for the sst data

# load packages
	library(raster)
	library(sp)
	library(RNetCDF)

# open connection to the data and extract from the server
# remember ncdf files index from 0 so subtract 1 off R index
	# time.seq = seq(ISOdate(1854,1,1), ISOdate(2018,12,1), "months")[1153:1980]
	# lat.seq = seq(from=-88,to=88,by=2)[15:75]
	# lon.seq = seq(from=0,to=358,by=2)[49:148]

	time.q = "[1152:1:2015]"
	lat.q = "[14:1:74]"
	lon.q = "[48:1:147]"
	url.nc = paste0("https://thredds.jpl.nasa.gov/thredds/dodsC/OceanTemperature/REYNOLDS_NCDC_L4_MONTHLY_V5.nc?lat",lat.q,",lev[0:1:0],lon",lon.q,",sst",time.q,"[0:1:0]",lat.q,lon.q,",time",time.q)
	# url.nc = "https://thredds.jpl.nasa.gov/thredds/dodsC/OceanTemperature/REYNOLDS_NCDC_L4_MONTHLY_V5.nc?lat[0:1:88],lev[0:1:0],lon[0:1:179],sst[1500:1:1500][0:1:0][0:1:88][0:1:179],time[1500:1:1500]"
	## open connection
		A = proc.time()
		nc = open.nc(url.nc)
		sst.array = var.get.nc(nc, "sst") # dims are lon, lat, time
		lon.vec = var.get.nc(nc, "lon") # raw is 0 to 358 by 2
		lat.vec = var.get.nc(nc, "lat") # raw is -88 to 88 by 2
		time.vec = var.get.nc(nc,"time") # raw is monthly
	##close connection
		close.nc(nc)
		B = proc.time()
		B-A # ~50 seconds on my machine using Microsoft R Open
		
# create list of rasters (avg sst by year x quarter)
	rotate.cc = function(x){apply(t(x),2,rev)} # helper function
	time.seq = seq(ISOdate(1854,1,1), ISOdate(2021,12,1), "months")[1153:2016]
	tail(time.seq)
	yrqtr.seq = sapply(time.seq,function(x)paste0(substr(x,1,4),0,ifelse(as.numeric(substr(x,6,7))<=3,0,ifelse(as.numeric(substr(x,6,7))<=6,1,ifelse(as.numeric(substr(x,6,7))<=9,2,3)))))
	u.yrqtr = unique(yrqtr.seq)

	sst.ras.list = as.list(rep(NA,length(u.yrqtr)))
	names(sst.ras.list) = u.yrqtr
	
	for(i in 1:length(sst.ras.list))
	{
		m.index = which(yrqtr.seq == names(sst.ras.list)[i]) # 3 months in each quarter
		m1.slice = rotate.cc(sst.array[,,m.index[1]])
		m2.slice = rotate.cc(sst.array[,,m.index[2]])
		m3.slice = rotate.cc(sst.array[,,m.index[3]])
		q.slice = (m1.slice + m2.slice + m3.slice)/3

		proj4string = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
		sst.slice = raster(q.slice,xmn=min(lon.vec),xmx=max(lon.vec),ymn=min(lat.vec),ymx=max(lat.vec),crs=proj4string)
		# plot(sst.slice,col=viridis::viridis(500))

		# assign to list
			sst.ras.list[[i]] = sst.slice
		# clean-up
			rm(list=c("m.index","m1.slice","m2.slice","m3.slice","q.slice","sst.slice"))
	}


# create sst.storage.df with NA's removed
# time (yrqtr), x (lon), y (lat), sst

	# intitialize
		sst.storage.df = data.frame(time = rep(names(sst.ras.list)[1],ncell(sst.ras.list[[1]])),
			                        x = xyFromCell(sst.ras.list[[1]],1:ncell(sst.ras.list[[1]]))[,1],
			                        y = xyFromCell(sst.ras.list[[1]],1:ncell(sst.ras.list[[1]]))[,2],
			                        sst = values(sst.ras.list[[1]]))
	# loop through remaining yrqtr's and fill in
		for(i in 2:length(sst.ras.list))
		{
			temp.df = data.frame(time = rep(names(sst.ras.list)[i],ncell(sst.ras.list[[i]])),
			                        x = xyFromCell(sst.ras.list[[i]],1:ncell(sst.ras.list[[i]]))[,1],
			                        y = xyFromCell(sst.ras.list[[i]],1:ncell(sst.ras.list[[i]]))[,2],
			                        sst = values(sst.ras.list[[i]]))
			sst.storage.df = rbind(sst.storage.df,temp.df)
			rm(list=c("temp.df"))
		}

	# remove NA's
		sst.storage.df = na.omit(sst.storage.df)

# save the output
		setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
	save(sst.ras.list,file="../Data/sst.ras.list_2021.RData")
	save(sst.storage.df,file="../Data/sst.storage.df_2021.RData")

