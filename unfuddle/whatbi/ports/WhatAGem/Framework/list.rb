require "WhatAGem/Framework/DataSource"

module WhatAGem
	class ListDataSource < DataSourceHelper
		def initialize(data = nil)
			@data = data
		end
		def getAll
			@data
		end
		def get(index)
			begin
				item = nil
				if index.integer? && index >= 1 && index <= @data.length
					# index is a one-based integer; list is zero-based
					item = @data[index - 1]
				end
			rescue NoMethodError
				# do nothing - we'll raise an exception in the ensure clause
			ensure
				raise InvalidKeyException.new if item.nil?
			end
		end
	end # ListDataSource
end # WhatAGem