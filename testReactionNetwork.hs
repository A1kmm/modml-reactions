mycatalyst reactant1 product1 product2 = do
  species0 <- newSpecies
  reaction {- modified by -} [species0] {- from -} [(1, reactant1)] {- to -}
    [(1, species2), (1, species3)]

mynetwork = do
  species1 <- newSpecies
  species2 <- newSpecies
  species3 <- newSpecies
  mycatalyst species1 species2 species3
  mycatalyst species4 species5 species6
