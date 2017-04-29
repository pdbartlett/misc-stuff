require "WhatAGem/Framework/Misc"

require "Test/Unit"

module WhatAGem
	module Tests
		class Caveman
			attr_reader :name
			attr_writer :name
			def initialize(name)
				@name = name
			end
			def ==(other)
				self.name == other.name
			end
			def <=>(other)
				self.name <=> other.name
			end
			def key
				@name
			end
		end # Caveman
		class DataSourceTestBase < Test::Unit::TestCase
			def createDataSource(data = nil)
				ds = nil
				if data == nil
					ds = dataSourceClass.send(:new)
				else
					ds = dataSourceClass.send(:new, data)
				end
				ds
			end
			def setup
				@testData = [ Caveman.new("Fred"), Caveman.new("Barney"), Caveman.new("Wilma"), Caveman.new("Betty") ]
				@ds = createDataSource(@testData)
			end
			def testCreateEmpty
				empty_ds = createDataSource()
				assert_not_nil(empty_ds)
			end
			def testCreateWithData
				assert_not_nil(@ds)
			end
			def testCount
				assert_equal(@testData.length, @ds.count)
			end
			def testGetAll
				all = @ds.getAll.sort
				assert_equal(@testData.length, all.length)
				sortedAll = all.sort
				sortedTest = @testData.sort
				(0..all.length-1).each { |index| assert_equal(sortedTest[index], sortedAll[index]) }
			end
			def testValidGets
				@testData.each { |item| assert_equal(item, @ds.get(item.key)) }
			end
			def testInvalidGetNil
				assert_raise(WhatAGem::InvalidKeyException) { @ds.get(nil) }
			end
			def testBlockQuery
				assert_equal( [ Caveman.new("Fred") ], @ds.query { |item| is_fred? item } )
			end
			def testMethodQuery
				assert_equal( [ Caveman.new("Fred") ], @ds.query(self.method(:is_fred?)) )
			end
			def testObjectQuery
				assert_equal( [ Caveman.new("Fred") ], @ds.query(FredPredicate.new) )
			end
			def is_fred?(item)
				item.name == "Fred"
			end
		end # DataSourceTestBase
		class FredPredicate
			def call(item)
				item.name == "Fred"
			end
		end # FredPredicate
	end # Tests
end # WhatAGem
