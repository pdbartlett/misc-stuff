load "./ports/WhatAGem/Framework/tests/testDefs.rb"

mode = ARGV.length > 0 ? ARGV[0].capitalize : "Console"

require "Test/Unit/UI/#{mode}/TestRunner"
eval "Test::Unit::UI::#{mode}::TestRunner.run(WhatAGemTests)"
