load_training <-  function(datapath, proj){
    data = read.csv(file.path(datapath, "GEOSTAT_SPCg_2017 - Training.csv"))
    coordinates(data) <- ~Longitude+Latitude
    projection(data) <- "+init=epsg:4326"
    spTransform(data, proj)
}
load_validation <-  function(datapath, proj){
    data = read.csv(file.path(datapath, "GEOSTAT_SPCg_2017 - Validation.csv"))
    coordinates(data) <- ~Longitude+Latitude
    projection(data) <- "+init=epsg:4326"
    spTransform(data, proj)
}

                    
make3d <- function(pts, value){
    cbind(coordinates(pts),value)
}

mean_predictor <- function(d_input){
    f = function(data){
        return(rep(mean(d_input$pH),nrow(data)))
    }
    f
}

duff_predictor <- function(d_input){
    phs = d_input$pH
    f = function(data){
        n = nrow(data)
        sample(phs, n, replace=TRUE)
    }
    f
}
    

gam_predictor <- function(d){
    g1 = gam(pH~s(Upper) +s(Lower) + s(Longitude,Latitude),data=as.data.frame(d))
    f = function(data){
        data = data.frame(data)
        predict(g1, data)
    }
    f
}

split_test <- function(data, f=0.5, predictor_gen){
    nin <- sample(1:nrow(data), nrow(data)*f)
    predictor = predictor_gen(data[nin,])
    rmse(predictor, data[-nin,],data$pH[-nin])
}

split_n <- function(data, f, predictor, N){
    force(predictor)
    doi = function(i){
        split_test(data, f, predictor)
    }
    ret =unlist( foreach(i=1:N) %dopar% doi(i) )

    ret
}

rmse <- function(predictor, newdata, correct){
        est = predictor(newdata)
        sqrt(mean((est-correct)^2))
}

compare_predictors <- function(data, f, N, predictors){
    do.call(rbind,
    lapply(1:length(predictors), function(i){
        pred = predictors[[i]]
        n = names(predictors)[[i]]
        data.frame(name = n,
                   v = split_n(data, f, pred, N)
                   )
    }
    ))
}

krig_predictor <- function(train_data){
    f = function(new_data){
        km = autoKrige(pH~Upper+Lower, input_data=train_data, new_data=new_data)
        km$krige_output$var1.pred
    }
    f
}

mkrig_predictor <- function(train_data){
    f = function(new_data){

        km = autoKrige(pH~
            Upper+
            bare2010_100m + treecover2000 +
            L01GLC3a+L02GLC3a+L03GLC3a+
            L04GLC3a+L05GLC3a+L06GLC3a+
            L08GLC3a+L09GLC3a+
            MERIT_100m+
            Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6,
            input_data=train_data, new_data=new_data)
        km$krige_output$var1.pred
    }
    f
}

km <- function(input, output){
    kmm = autoKrige(
        pH ~
            Upper+
            bare2010_100m + treecover2000 +
            L01GLC3a+L02GLC3a+L03GLC3a+
            L04GLC3a+L05GLC3a+L06GLC3a+
            L08GLC3a+L09GLC3a+
            MERIT_100m+
            Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6,
        input_data=input, new_data=output)
    kmm
}

rastersampling <- function(pts, rasters){
    do.call(cbind,
    lapply(rasters, function(raster_file){
        r = raster(raster_file)
        extract(r, pts)
    }
    )
    )
}

imerg_covars <- function(d, npc=6){
    imerg = rastersampling(d,list.files("./IMERG_10km_Africa/","*.tif$",full=TRUE))
    pc_imerg = princomp(imerg)
    ppc = predict(pc_imerg)[,1:npc]
    data.frame(ppc)
}

misc_covars <- function(d){
    rasters = list.files("./covs_1km_Africa/","*.tif$",full=TRUE)
    misc <- rastersampling(d, rasters)
    colnames(misc)=gsub("\\.tif","",basename(rasters))
    as.data.frame(misc)
}

fit_glm <- function(m){
    glm(pH ~ Upper+
            bare2010_100m + treecover2000 +
            L01GLC3a+L02GLC3a+L03GLC3a+
            L04GLC3a+L05GLC3a+L06GLC3a+
            L08GLC3a+L09GLC3a+
            MERIT_100m+
            Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6,
        data = m)
}

fit_gam <- function(m){
    gam(
        pH ~ s(Upper) +
            bare2010_100m + treecover2000 +
            s(L01GLC3a)+s(L02GLC3a)+L03GLC3a+
            L04GLC3a+L05GLC3a+L06GLC3a+
            L08GLC3a+L09GLC3a+
            s(MERIT_100m)+
            s(Comp.1)+s(Comp.2)+s(Comp.3)+s(Comp.4)+s(Comp.5)+s(Comp.6)+
       s(Longitude,Latitude),
        data = data.frame(m))
}

fg_predictor <- function(m){
    g = fit_gam(m)
    f = function(data){
        predict(g,newdata=data)
    }
    f
}

build_full <- function(dc){
    mc = misc_covars(dc)
    ic = imerg_covars(dc)
    dc@data = cbind(dc@data, mc, ic)
    dc
}

compare_p <- function(p1,p2,v){
    n1 = deparse(substitute(p1))
    n2 = deparse(substitute(p2))
    d = data.frame(v=c(p1[[v]],p2[[v]]))
    d$name = c(rep(n1,nrow(p1)),rep(n2,nrow(p2)))
    ggplot(d,aes(x=v))+geom_histogram()+facet_wrap(~name,ncol=1)

}
