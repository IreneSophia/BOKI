if (!file.exists(file.path(dt.pathX[length(dt.pathX)], "cross_turns.rds"))) {
  
  for (i in 1:nrow(df.turns)) { #
    d = df.turns$dyad[i]
    s = df.turns$speaker[i]
    t = df.turns$task[i]
    xmin = df.turns$start_turn[i]
    xmax = df.turns$end_turn[i]
    if (length(df.vid[df.vid$dyad == d & df.vid$task == t & df.vid$speaker == s & df.vid$time <= xmax & df.vid$time >= xmin,]$turn_self) > 0 ) {
      df.vid[df.vid$dyad == d & df.vid$task == t & df.vid$speaker == s & df.vid$time <= xmax & df.vid$time >= xmin,]$turn_self  = 1
      df.vid[df.vid$dyad == d & df.vid$task == t & df.vid$speaker != s & df.vid$time <= xmax & df.vid$time >= xmin,]$turn_other = 1
    } else {
      warning("Nothing found for ", i, " (", d, ", ", t, ", ", s, ")")
    }
    if (i%%1000 == 0) {
      print(sprintf('%s: %d of %d', Sys.time(), i, nrow(df.turns)))
      saveRDS(df.vid, file = file.path(dt.pathX[length(dt.pathX)], 
                                       sprintf("cross_turns_%d.rds", i)))
    }
  }
  1092.48*23 / (60*60)
  
  saveRDS(df.vid, file = file.path(dt.pathX[length(dt.pathX)], "cross_turns.rds"))
  
} else {
  df.vid = readRDS(file.path(dt.pathX[length(dt.pathX)], "cross_turns.rds"))
}