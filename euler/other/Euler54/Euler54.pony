use "files"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      with file = OpenFile(
        FilePath(env.root as AmbientAuth, "p054_poker.txt", caps)?) as File
      do
        var c = U32(0)
        for line in file.lines() do
          let cards = line.split().values()
          c = c + 1
        end
        env.out.print(c.string())
      end
    else
      env.out.print("Couldn't open data file")
    end
