use "collections"

actor Main
  new create(env : Env) =>
    var best_diff: F64 = 1.0
    var best_num: USize = 0
    var best_den: USize = 0
    for d in Range(1, 1_000_001) do
      let n = (d * 3) / 7
      let df = (3.0 / 7.0) - (n.f64() / d.f64())
      if (df > 0) and (df < best_diff) then
        best_diff = df
        best_num = n
        best_den = d
      end
    end
    env.out.print(best_num.string() + " / " + best_den.string())
