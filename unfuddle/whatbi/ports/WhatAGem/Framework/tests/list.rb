require "WhatAGem/Framework/List"
require "WhatAGem/Framework/Tests/DataSource"

module WhatAGem
	module Tests
		class ListTest < DataSourceTestBase
			def dataSourceClass
				ListDataSource
			end
			def testValidGets
				@testData.each_index { |index|	assert_equal(@testData[index], @ds.get(index + 1)) }
			end
			def testInvalidGetNegative
				assert_raise(WhatAGem::InvalidKeyException) { @ds.get(-1) }
			end
			def testInvalidGetZero
				assert_raise(WhatAGem::InvalidKeyException) { @ds.get(0) }
			end
			def testInvalidGetHigh
				assert_raise(WhatAGem::InvalidKeyException) { @ds.get(@testData.length + 1) }
			end
		end # ListTest
	end # Tests
end	 # WhatAGem