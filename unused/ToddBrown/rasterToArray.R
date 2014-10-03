rasterToArray <- function(grid){
    edges <- c(xmin(grid), xmax(grid), ymin(grid), ymax(grid))
    delta <- res(grid)

    if(abs(edges[1]-edges[2]) == 360){
        lon <- seq(from=edges[1], to=edges[2]-delta[1], by=delta[1])+delta[1]/2
        lat <- seq(from=edges[3], to=edges[4]-delta[2], by=delta[2])+delta[2]/2

    }else{
        lat <- seq(from=edges[1], to=edges[2]-delta[1], by=delta[1])+delta[1]/2
        lon <- seq(from=edges[3], to=edges[4]-delta[2], by=delta[2])+delta[2]/2
    }

    lon[lon<0] <- lon[lon<0]+360

    ans <- array(NA, dim=c(length(lon), length(lat), dim(as.array(grid))[3]))
    ans <- as.array(grid)
    ans <- aperm(ans, c(2,1,3))
    ans <- ans[order(lon),length(lat):1,1:dim(ans)[3]]
    return(ans)
}

