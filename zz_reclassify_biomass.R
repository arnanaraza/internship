if (!require("pacman")) install.packages("pacman")
pacman::p_load(classInt, data.table)

td.14.class <- td.14
class.14 <- classIntervals(td.14$biomass.ton, n=12, style='quantile')
brk <- class.14$brks

td.14.class$bioclass <- td.14.class$biomass.ton



td.14.class$bioclass <- with(td.14.class, ifelse(
  td.14.class$bioclass >= brk[1] & td.14.class$bioclass <= brk[2], 1, ifelse(
    td.14.class$bioclass > brk[2] & td.14.class$bioclass <= brk[3], 2, ifelse(
      td.14.class$bioclass > brk[3] & td.14.class$bioclass <= brk[4], 3, ifelse(
        td.14.class$bioclass > brk[4] & td.14.class$bioclass <= brk[5], 4, ifelse(
          td.14.class$bioclass > brk[5] & td.14.class$bioclass <= brk[6], 5, ifelse(
            td.14.class$bioclass > brk[6] & td.14.class$bioclass <= brk[7], 6, ifelse(
              td.14.class$bioclass > brk[7] & td.14.class$bioclass <= brk[8], 7, ifelse(
                td.14.class$bioclass > brk[8] & td.14.class$bioclass <= brk[9], 8, ifelse(
                  td.14.class$bioclass > brk[9] & td.14.class$bioclass <= brk[10], 9, ifelse(
                    td.14.class$bioclass > brk[10] & td.14.class$bioclass <= brk[11], 10, ifelse(
                      td.14.class$bioclass > brk[11] & td.14.class$bioclass <= brk[12], 11, ifelse(
                        td.14.class$bioclass > brk[12], 12, 'shitty')))))))))))))
td.14.class$bioclass
unique(td.14.class$bioclass)




