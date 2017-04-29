require "WhatAGem/Framework/Misc"

module WhatAGem
	class DataSourceHelper
		def count
			getAll.length
		end
		def query(*pred)
			res = []
			getAll.each do |item|
				matches = false
				if block_given?
					matches = yield item
				elsif pred.length == 1 && pred[0].respond_to?("call")
					matches = pred[0].call(item)
				else
					raise "Expected either a single callable predicate parameter, or an associated block"
				end
				res.push(item) if matches
			end
			res
		end
	end # DataSourceHelper
end # WhatAGem
