$:.push("./ports") # assumes we're running from the WhaTBI? root

# add new test files here
require "WhatAGem/Framework/tests/list"
require "WhatAGem/Framework/tests/map"

class WhatAGemTests
	def self.suite
		suite = Test::Unit::TestSuite.new("WhatAGem")
		# and also add test classes here
		suite << WhatAGem::Tests::ListTest.suite
		suite << WhatAGem::Tests::MapTest.suite
	end
end # WhatAGemTests

