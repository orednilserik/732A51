# 1. Load tree
tr <- read.tree("sylvia_nj_k80.tre")

# 2. Remove species
tr <- drop.tip(tr, "Chamaea_fasciata")
tr <- drop.tip(tr, "Devioeca_papuana")

# 3. Unify names
tr$tip.label <- gsub("Curruca_", "Sylvia_", tr$tip.label)

# 4. Fix spelling errors
tr$tip.label[tr$tip.label == "Sylvia_ruppeli"] <- "Sylvia_rueppelli"

# 5. Extract matched ecological data
DF <- sylvia.eco[tr$tip.label, ]
