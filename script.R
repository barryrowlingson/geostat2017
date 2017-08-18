r = raster("./IMERG_10km_Africa/3B-MO-L.GIS.IMERG.20140401.V04A.tif")
train = build_full(
    load_training("./Data/",projection(r))
    )

validation <- build_full(load_validation("./Data/",projection(train)))


k2 = mkrig_predictor(train)

pred = k2(validation)
