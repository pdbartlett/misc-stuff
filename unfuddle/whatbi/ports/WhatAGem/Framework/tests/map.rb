require "WhatAGem/Framework/Map"
require "WhatAGem/Framework/Tests/DataSource"

module WhatAGem
	module Tests
		class MapTest < DataSourceTestBase
			def dataSourceClass
				MapDataSource
			end
			def testInvalidGetNonExistent
				assert_raise(WhatAGem::InvalidKeyException) { @ds.get("Dino") }
			end
		end # MapTest
	end # Tests
end # WhatAGem
