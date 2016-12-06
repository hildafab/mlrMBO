# EXPECTED IMPROVEMENT WITH Meta-Learning
# (useful for deterministic, for noisy only with reinterpolation)
infillCritEIMtL = function(points, models, control, par.set, design, iter) {
  points = points[order(points$selected.learner),]
  model = models[[1L]]
  maximize.mult = ifelse(control$minimize, 1, -1)
  y = maximize.mult * design[, control$y.name]
  p = predict(model, newdata = points)$data
  p.mu = maximize.mult * p$response
  p.se = p$se
  y.min = min(y)
  d = y.min - p.mu
  xcr = d / p.se
  xcr.prob = pnorm(xcr)
  xcr.dens = dnorm(xcr)
  ei = d * xcr.prob + p.se * xcr.dens
  
  #meta learning part here  
  openml.did = control$infill.crit.eimtl.openmldid
  #   print("inside infill crit eimtl")
  #   print(openml.did)
  #get meta features
  dataset.metafeatures = getMetaDataForDataset(did=openml.did)
  parset.with.trafo = control$infill.crit.eimtl.parset
  #get performance for the point based on the meta features and hyper parameters  
  perf = getPerformanceFromMetaInfo(points,dataset.metafeatures,parset.with.trafo)
  perf = maximize.mult * perf
  #perf = 1
  #   options(error = browser())
  decay = control$eimtl.decay.param  
  ei = ((1 - (decay ^ iter)) * ei) + (perf * (decay ^ iter))
  
  # FIXME: magic number
  # if se too low set 0 (numerical problems), negate due to minimization
  ifelse(p.se < 1e-6, 0, -ei)
}

