#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
density10 = replicate(5, bml.sim(25, 25, .1))
density15 = replicate(5, bml.sim(25, 25, .15))
density20 = replicate(5, bml.sim(25, 25, .2))
density25 = replicate(5, bml.sim(25, 25, .25))
density30 = replicate(5, bml.sim(25, 25, .3))
density35 = replicate(5, bml.sim(25, 25, .35))
density40 = replicate(5, bml.sim(25, 25, .4))
density45 = replicate(5, bml.sim(25, 25, .45))
density50 = replicate(5, bml.sim(25, 25, .5))
density55 = replicate(5, bml.sim(25, 25, .55))
density60 = replicate(5, bml.sim(25, 25, .6))
density65 = replicate(5, bml.sim(25, 25, .65))
density70 = replicate(5, bml.sim(25, 25, .7))
density75 = replicate(5, bml.sim(25, 25, .75))
density80 = replicate(5, bml.sim(25, 25, .8))

results = data.frame(d10 = unlist(density10[2, ]), d15 = unlist(density15[2, ]), d20 = unlist(density20[2, ]), d25 = unlist(density25[2, ]),
                     d30 = unlist(density30[2, ]), d35 = unlist(density35[2, ]), d40 = unlist(density40[2, ]), d45 = unlist(density50[2, ]),
                     d50 = unlist(density50[2, ]), d55 = unlist(density55[2, ]), d60 = unlist(density60[2, ]), d65 = unlist(density65[2, ]),
                     d70 = unlist(density70[2, ]), d75 = unlist(density75[2,]), d80 = unlist(density80[2, ]))
means = c(mean(results$d10), mean(results$d15), mean(results$d20), mean(results$d25), mean(results$d30), mean(results$d35), mean(results$d40),
          mean(results$d45), mean(results$d50), mean(results$d55), mean(results$d60), mean(results$d65), mean(results$d70), mean(results$d75),
          mean(results$d80))

barplot(means, width = 1, xlab = "density as percentage", ylab = "number of timesteps to gridlock", main = "Averages by density for 25 x 25 lattace",
        sub ="if it's at 10000 timesteps, then it is in freeflow", names.arg = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))

cdensity10 = replicate(5, bml.sim(19, 31, .1))
cdensity15 = replicate(5, bml.sim(19, 31, .15))
cdensity20 = replicate(5, bml.sim(19, 31, .2))
cdensity25 = replicate(5, bml.sim(19, 31, .25))
cdensity30 = replicate(5, bml.sim(19, 31, .3))
cdensity35 = replicate(5, bml.sim(19, 31, .35))
cdensity40 = replicate(5, bml.sim(19, 31, .4))
cdensity45 = replicate(5, bml.sim(19, 31, .45))
cdensity50 = replicate(5, bml.sim(19, 31, .5))
cdensity55 = replicate(5, bml.sim(19, 31, .55))
cdensity60 = replicate(5, bml.sim(19, 31, .6))
cdensity65 = replicate(5, bml.sim(19, 31, .65))
cdensity70 = replicate(5, bml.sim(19, 31, .7))
cdensity75 = replicate(5, bml.sim(19, 31, .75))
cdensity80 = replicate(5, bml.sim(19, 31, .8))

results2 = data.frame(d10 = unlist(cdensity10[2, ]), d15 = unlist(cdensity15[2, ]), d20 = unlist(cdensity20[2, ]), d25 = unlist(cdensity25[2, ]),
                     d30 = unlist(cdensity30[2, ]), d35 = unlist(cdensity35[2, ]), d40 = unlist(cdensity40[2, ]), d45 = unlist(cdensity50[2, ]),
                     d50 = unlist(cdensity50[2, ]), d55 = unlist(cdensity55[2, ]), d60 = unlist(cdensity60[2, ]), d65 = unlist(cdensity65[2, ]),
                     d70 = unlist(cdensity70[2, ]), d75 = unlist(cdensity75[2,]), d80 = unlist(cdensity80[2, ]))
means2 = c(mean(results2$d10), mean(results2$d15), mean(results2$d20), mean(results2$d25), mean(results2$d30), mean(results2$d35), mean(results2$d40),
          mean(results2$d45), mean(results2$d50), mean(results2$d55), mean(results2$d60), mean(results2$d65), mean(results2$d70), mean(results2$d75),
          mean(results2$d80))
barplot(means2, width = 1, xlab = "density as percentage", ylab = "number of timesteps to gridlock", main = "Averages by density for 19 x 31 lattice", 
        sub ="if it's at 10000 timesteps, then it is in freeflow", names.arg = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
