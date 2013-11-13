####### Author: Vsiliki Aspasiou
####### Date: 12-11-2013
####### Name: Exercise 4



sp_polys <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,
                    capStyle="FLAT")

sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = 2.0)
sp_polys <- gBuffer(sp_polys, byid=T,id=rownames(sp_polys), width = -2.0) 
sp_polys_df <- SpatialPolygonsDataFrame(sp_polys, sp_lines_df@data)
tmp_lines <- sp_lines_df@lines # just a list with geometries
for (i in 2:length(sp_lines_df)){
  tmline <- sp_lines_df[i,]$datim 
  for (j in 1:(i-1)){
    tmpoly <- sp_polys_df[j,]$datim
    if (difftime(tmline, tmpoly, units = "secs") > 0){
      tmp_line <- SpatialLines(tmp_lines[i], prj_string_RD) 
      if (gIntersects(tmp_line, sp_polys_df[j,])){
        # compute difference
        tmp_lines[[i]] <- gDifference(tmp_line, sp_polys_df[j,])@lines[[1]] 
        tmp_lines[[i]]@ID <- sp_lines_df[i,]@lines[[1]]@ID
      } 
    } 
  }
} 
tmp_lines <- SpatialLines(tmp_lines, prj_string_RD) 
cln_lines_df <- SpatialLinesDataFrame(tmp_lines, sp_lines_df@data)


###################Assignment 1###########
################ex_4#############

cln_lines_df <- gBuffer(sp_lines_df,byid=T, width=0.5*sp_lines_df$width,
                        capStyle="FLAT")






# 1. Buffer lines to make harvesting blocks
cln_buffer <- gBuffer(cln_lines_df,byid = T, width=0.75*cln_lines_df$width,
                      capStyle="ROUND")

# 2. Fill small holes by swelling and shrinking
cln_buffer <- gBuffer(cln_buffer, byid=T,id=rownames(cln_buffer), width = 2.0)
cln_buffer <- gBuffer(cln_buffer, byid=T,id=rownames(cln_buffer), width = -2.0)
cln_lines_df <- SpatialPolygonsDataFrame(cln_buffer, cln_lines_df@data)

# 3. Calculate tield per hectare

area_metres <- gArea(cln_lines_df, byid=T)
area_ha <- area_metres/10000
yield_per_ha <- cln_lines_df$loads / area_ha
cln_lines_df$yield_per_ha <- yield_per_ha

head (cln_lines_df)

plot (cln_lines_df)

# 4. Plot yield per hectare and block

spplot(cln_lines_df, zcol="yield_per_ha", colorkey=T, zlim=c(0,100),
       col.regions=c(bpy.colors(25)), pch=19,
       cex=0.25, main="Yield per block in ton/ha")

# 5. Export to KML

prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
linesforGM <- spTransform(cln_lines_df, prj_string_WGS)

writeOGR(linesforGM, file.path("data", "PolyTonHa.kml"),
         "yield ton/ha", driver="KML", overwrite_layer=TRUE)
