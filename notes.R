
pp = compare_predictors(train, .5, 100, list(gam=gam_predictor, duff=duff_predictor, mean=mean_predictor, krig=krig_predictor))

bf <- build_full(d)

pp1 = compare_predictors(
    bf, .5, 100, 
    list(
        gam=gam_predictor,
        gam2 = fg_predictor))

ggplot(pp1, aes(x=v)) + geom_histogram() + facet_wrap(~name,ncol=1)


 k2 = mkrig_predictor(train)
 k2p = k2(validation)

# posted k2p
