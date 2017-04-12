library(semPlot)

# correlation
model = "
Y ~~ X
"
semPaths(model, what = "path", whatLabels = "name", edge.color = "black", rotation = 3, residuals = F, sizeMan = 10)

model1 = "
X =~ 1*X1 + X2 + X3
Y =~ 1*Y1 + Y2 + Y3
Y ~~ X
"
semPaths(model1, what = "path", whatLabels = "name", edge.color = "black", rotation = 1, residuals = F, layout = "tree2")

# causal
model2 = "
Y ~ a*X
"
semPaths(model2, what = "path", whatLabels = "name", edge.color = "black", rotation = 2, residuals = F, sizeMan = 10)

model3 = "
X =~ 1*X1 + X2 + X3
Y =~ 1*Y1 + Y2 + Y3
Y ~ a*X
"
semPaths(model3, what = "path", whatLabels = "name", edge.color = "black", rotation = 2, residuals = F, layout = "tree2")

# mediation
model4 = "
Z ~ a*X
Y ~ c*X + b*Z
"
semPaths(model4, what = "path", whatLabels = "name", edge.color = "black", residuals = F, layout = "spring", sizeMan = 7)
