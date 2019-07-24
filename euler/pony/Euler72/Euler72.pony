use "collections"

actor Main
  new create(env: Env) =>
    var c : U32 = 0
    for d in Range(2, 1_000_001) do
      for n in Range(1, d) do
        var rpf = true
        for f in Range(2, n.f64().sqrt().ceil().usize()) do
          if ((n % f) == 0) and ((d % f) == 0) then
            rpf = false
            break
          end
        end
        if rpf then c = c + 1 end
      end
    end
    env.out.print(c.string())
