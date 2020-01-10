data(rockjock)
data(rockjock_mixtures)

set.seed(400)

f <- list()

for (i in 1:100) {

f[[i]] <- fps(lib = rockjock,
         smpl = rockjock_mixtures[[sample(1:8, size = 1)]],
         harmonise = sample(c(TRUE, FALSE), size = 1),
         solver = sample(c("BFGS", "CG", "Nelder-Mead", "NNLS"),
                    size = 1),
         obj = sample(c("Rwp", "R", "Delta"), size = 1),
         refs = c("ORDERED_MICROCLINE",
                  "LABRADORITE",
                  "KAOLINITE_DRY_BRANCH",
                  "MONTMORILLONITE_WYO",
                  "ILLITE_1M_RM30",
                  "CORUNDUM",
                  "VOLCANIC_GLASS_HEKLA",
                  "OPAL",
                  "Quartz"),
         std = sample(c("ORDERED_MICROCLINE",
                        "LABRADORITE",
                        "KAOLINITE_DRY_BRANCH",
                        "MONTMORILLONITE_WYO",
                        "ILLITE_1M_RM30",
                        "CORUNDUM",
                        "VOLCANIC_GLASS_HEKLA",
                        "OPAL",
                        "QUARTZ"), size = 1),
         align = sample(c(0, 0.1, 0.2, 0.3), size = 1),
         manual_align = sample(c(TRUE, FALSE), size = 1),
         shift = sample(c(0, 0.05, 0.1), size = 1),
         shift_res = sample(c(1:8), size = 1),
         remove_trace = sample(c(0, 0.05, 0.1), size = 1)
    )

}


set.seed(300)

a <- list()

for (i in 1:10) {

  a[[i]] <- afps(lib = rockjock,
                smpl = rockjock_mixtures[[sample(1:8, size = 1)]],
                harmonise = sample(c(TRUE, FALSE), size = 1),
                solver = sample(c("BFGS", "CG", "Nelder-Mead"),
                                size = 1),
                obj = sample(c("Rwp", "R", "Delta"), size = 1),
                force = sample(c("ORDERED_MICROCLINE",
                         "LABRADORITE",
                         "KAOLINITE_DRY_BRANCH",
                         "MONTMORILLONITE_WYO",
                         "ILLITE_1M_RM30",
                         "CORUNDUM",
                         "VOLCANIC_GLASS_HEKLA",
                         "OPAL",
                         "QUARTZ"), size = 1),
                std = "CORUNDUM",
                std_conc = sample(c(NA, 20), size = 1),
                align = sample(c(0, 0.1, 0.2, 0.3), size = 1),
                manual_align = sample(c(TRUE, FALSE), size = 1),
                shift = sample(c(0, 0.05, 0.1), size = 1),
                shift_res = sample(c(1:8), size = 1),
                lod = sample(c(0, 1, 2), size = 1),
                amorphous = sample(c("VOLCANIC_GLASS_HEKLA", "OPAL"),
                                   size = 1),
                amorphous_lod = sample(c(0, 1, 2), size = 1)
  )

}

set.seed(5)

a2 <- list()

for (i in 1:20) {

  a2[[i]] <- afps(lib = rockjock,
                 smpl = rockjock_mixtures[[sample(1:8, size = 1)]],
                 harmonise = sample(c(TRUE, FALSE), size = 1),
                 solver = sample(c("BFGS", "CG", "Nelder-Mead"),
                                 size = 1),
                 obj = sample(c("Rwp", "R", "Delta"), size = 1),
                 force = sample(c("ORDERED_MICROCLINE",
                                  "LABRADORITE",
                                  "KAOLINITE_DRY_BRANCH",
                                  "MONTMORILLONITE_WYO",
                                  "ILLITE_1M_RM30",
                                  "CORUNDUM",
                                  "VOLCANIC_GLASS_HEKLA",
                                  "OPAL",
                                  "QUARTZ"), size = 1),
                 std = "CORUNDUM",
                 std_conc = NA,
                 align = sample(c(0, 0.1, 0.2, 0.3), size = 1),
                 manual_align = sample(c(TRUE, FALSE), size = 1),
                 shift = sample(c(0, 0.05, 0.1), size = 1),
                 shift_res = sample(c(1:8), size = 1),
                 lod = sample(c(0, 1, 2), size = 1),
                 amorphous = sample(c("VOLCANIC_GLASS_HEKLA", "OPAL"),
                                    size = 1),
                 amorphous_lod = sample(c(0, 1, 2), size = 1)
  )

}



