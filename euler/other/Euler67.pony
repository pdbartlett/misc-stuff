use "files"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    var data = Array[Array[I32]]
    try
      with file = OpenFile(
        FilePath(env.root as AmbientAuth, "p067_triangle.txt", caps)?) as File
      do
        for line in file.lines() do
          var nums = Array[I32]
          for s in line.split().values() do
            nums.push(s.i32()?)
          end
          data.push(nums)
        end
      end
    else
      env.out.print("Couldn't open data file")
    end
    try
      var i = (data.size() - 2).i32()
      while i >= 0 do
        let i' = i.usize()
        var j = USize(0)
        while j < data(i')?.size() do
          data(i')?(j)? = data(i')?(j)? + data(i'+1)?(j)?.max(data(i'+1)?(j+1)?)
          j = j + 1
        end
        i = i - 1
      end
      env.out.print(data(0)?(0)?.string())
    else
      env.out.print("Huh?")
    end
