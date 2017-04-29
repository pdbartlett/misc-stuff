require "WhatAGem/Framework/DataSource"

module WhatAGem
	class MapDataSource < DataSourceHelper
		def initialize(data = nil)
			@data = {}
			return if data == nil
			data.each { |item| @data.store(item.key, item) }
		end
		def getAll
			@data.values
		end
		def get(key)
			raise InvalidKeyException.new unless @data.has_key?(key)
			@data[key]
		end
	end # MapDataSource
end # WhatAGem
