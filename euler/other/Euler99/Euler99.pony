use "files"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      with file = OpenFile(
        FilePath(env.root as AmbientAuth, "p099_base_exp.txt", caps)?) as File
      do
        var maxlog = F64(0)
        var maxlineno = I32(0)
        var lineno = I32(0)
        for line in file.lines() do
          lineno = lineno + 1
          let nums = line.split(",").values()
          let thislog = nums.next()?.f64()?.log() * nums.next()?.f64()?
          if thislog > maxlog then
            maxlog = thislog
            maxlineno = lineno
          end
        end
        env.out.print(maxlineno.string())
      end
    else
      env.out.print("Couldn't open data file")
    end
