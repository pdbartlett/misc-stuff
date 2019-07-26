use "collections"

actor Main
  new create(env: Env) =>
    let counts = Map[String, I32]()
    let firsts = Map[String, U128]()
    var n = U128(0)
    while true do
      let cube = n * n * n
      let a = recover val Sort[Array[U8], U8](cube.string().array().clone()) end
      let key = String.from_array(a)
      let count = counts.get_or_else(key, 0) + 1
      counts.update(key, count)
      if count == 1 then
        firsts.update(key, cube)
      elseif count >= 5 then
        env.out.print(firsts.get_or_else(key, 0).string())
        break
      end
      n = n + 1
    end
