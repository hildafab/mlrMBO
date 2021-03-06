generateTestDesign = function(n = 10L, par.set, ...) {
  for (i in 1:100) {
    design = generateDesign(n = n, par.set = par.set, ...)
    if (checkInitDesign(design, par.set))
      return(design)
  }
  stop("Unable to create a valid design in generateTestDesign!")
}

testf.fsphere.1d = makeSphereFunction(dimensions = 1L)
testp.fsphere.1d = getParamSet(testf.fsphere.1d)
testd.fsphere.1d = generateTestDesign(5L, testp.fsphere.1d)

testf.fsphere.2d = makeSphereFunction(dimensions = 2L)
testp.fsphere.2d = getParamSet(testf.fsphere.2d)
testd.fsphere.2d = generateTestDesign(10L, testp.fsphere.2d)

testf.zdt1.2d = makeZDT1Function(dimensions = 2L)
testp.zdt1.2d = getParamSet(testf.zdt1.2d)
testd.zdt1.2d = generateTestDesign(10L, testp.zdt1.2d)

default.kriging = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

testfmco1 = makeMultiObjectiveFunction(
  fn = function(x) x^2,
  n.objectives = 2L,
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
)
testdesmco1 = generateTestDesign(10L, getParamSet(testfmco1))

testfmco2 = makeMultiObjectiveFunction(
  fn = function(x) c(1, -1) * x^2,
  n.objectives = 2L,
  par.set = makeNumericParamSet(len = 2L, lower = -2, upper = 1)
)
testdesmco2 = generateTestDesign(10L, getParamSet(testfmco2))
