library(ggplot2)


path <- here::here("posts", "2025-01-28-parallel-computing-in-r")


## Import study area ----

study_area <- sf::st_read(file.path(path, "data", "study_area.gpkg"))


## Import species distribution ----

sp_polygons <- sf::st_read(file.path(path, "data", "species_polygons.gpkg"))


## Subset one species ----

species <- "Curruca cantillans"

sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == species, ]


## Map layers ----

ggplot() +
  theme_bw() +
  geom_sf(data = study_area) +
  geom_sf(data = sub_polygons, 
          fill = "#4F000088", 
          col  = "#4F0000")


## Extract species names ----

sp_names <- unique(sp_polygons$"blife_binomial")
sp_names <- sort(sp_names)

length(sp_names)


## Function to rasterize ----

polygon_to_grid <- function(grid, polygon) {

  species <- unique(polygon$"blife_binomial")
  species <- gsub(" ", "_", species) |> 
    tolower()
  
  cells <- sf::st_intersects(grid, polygon, sparse = FALSE)
  cells <- apply(cells, 1, any)
  cells <- which(cells)
  
  grid[ , species]     <- 0
  grid[cells, species] <- 1
  
  sf::st_drop_geometry(grid)
}


## Example w/ one species -----

sp_grid <- polygon_to_grid(study_area, sub_polygons)
sf::st_geometry(sp_grid) <- sf::st_geometry(study_area)

sp_grid$"curruca_cantillans" <- as.factor(sp_grid$"curruca_cantillans")

ggplot() +
  theme_bw() +
  geom_sf(data = sp_grid, aes(fill = curruca_cantillans)) +
  scale_fill_manual(values = c("0" = "#FFFFFF", "1" = "#9F0000")) +
  geom_sf(data = sub_polygons, col = "#000000", fill = NA, linewidth = 0.5)


## for loop ----

for_bm <- system.time({

  grids <- list()
  
  for (i in 1:length(sp_names)) {
    
    sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == sp_names[i], ]
    
    grids[[i]] <- polygon_to_grid(study_area, sub_polygons)
  }
})


## lapply ----

lapply_bm <- system.time({

  grids <- lapply(1:length(sp_names), function(i) {
  
    sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == sp_names[i], ]
    polygon_to_grid(study_area, sub_polygons)
  })
})



## mclapply (Forking) ----

mclapply_bm <- system.time({

  grids <- parallel::mclapply(1:length(sp_names), function(i) {
    
    sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == sp_names[i], ]
    polygon_to_grid(study_area, sub_polygons)
  
  }, mc.cores = 15)
})


## parLapply (Socket) ----

parlapply_bm <- system.time({

  cl <- parallel::makeCluster(15)
  
  invisible(parallel::clusterEvalQ(cl, { library(sf) }))
  
  parallel::clusterExport(cl, c("sp_names", "sp_polygons", "study_area", 
                                "polygon_to_grid"), envir = environment())
  
  
  grids <- parallel::parLapply(X = 1:length(sp_names), fun = function(i) {
    
    sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == sp_names[i], ]
    polygon_to_grid(study_area, sub_polygons)
    
  }, cl = cl)
  
  parallel::stopCluster(cl)
})


## foreach (Socket) ----

library(foreach)

foreach_bm <- system.time({

  cl <- parallel::makeCluster(15)
  
  invisible(parallel::clusterEvalQ(cl, { library(sf) }))
  
  parallel::clusterExport(cl, c("sp_names", "sp_polygons", "study_area", 
                                "polygon_to_grid"), envir = environment())
  
  doParallel::registerDoParallel(cl)
  
  grids <- foreach(i = 1:length(sp_names)) %dopar% {
    
    sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == sp_names[i], ]
    polygon_to_grid(study_area, sub_polygons)
  }
  
  parallel::stopCluster(cl)
})


## Benchmark ----

rbind(for_bm, 
      lapply_bm, 
      mclapply_bm, 
      parlapply_bm, 
      foreach_bm)


## Aggregate ----

grids <- parallel::mclapply(1:length(sp_names), function(i) {
  
  sub_polygons <- sp_polygons[sp_polygons$"blife_binomial" == sp_names[i], ]
  polygon_to_grid(study_area, sub_polygons)
  
}, mc.cores = 15)

grids <- do.call(cbind, grids)
grids <- grids[ , which(colnames(grids) != "id")]

grids$"richness" <- apply(grids, 1, sum)

sf::st_geometry(grids) <- sf::st_geometry(study_area)


ggplot() +
  theme_bw() +
  geom_sf(data = grids, aes(fill = richness)) + 
  scale_fill_gradient(low = "yellow", high = "#9F0000")
