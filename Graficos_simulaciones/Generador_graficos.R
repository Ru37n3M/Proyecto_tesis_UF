
test_only_opinion <- map(test, 1)
test_covisualizaciones <- map(test, 2)

graficos <- map(
  test_only_opinion,
  generador_graficos
)

