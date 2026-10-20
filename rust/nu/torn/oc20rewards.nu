#!/usr/bin/env nu

let prices = torn api torn items --all-pages | from json | get items | flatten | select id name market_price
let rewards = torn api faction crimes --all-pages | from json | get crimes | where status == "Successful" | select rewards
let money = $rewards.rewards.money | math sum
let items = $rewards.rewards.items | flatten
let priced = $items | join $prices id | insert total {|row| $row.quantity * $row.market_price}
let loot = $priced.total | math sum
let pct = $money / ($money + $loot) * 100

print -n "Total cash earned from crimes: $"
print ($money | into string -gd 0)
print -n "Total loot value from crimes: $"
print ($loot | into string -gd 0)
print -n "Percentage of earnings that are cash: "
print ($pct | into string -d 1)
