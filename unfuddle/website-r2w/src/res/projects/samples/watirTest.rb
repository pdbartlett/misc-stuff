require 'watir'
require 'test/unit'

# skeleton to get you started...

#require 'watir'
#require 'test/unit'
#require 'test/unit/ui/console/testrunner'
#
#require 'WatirTest'
#
#class SkeletonTest < Test::Unit::TestCase
#    include WatirTest
#    def new
#        init_watir
#    end
#    def setup
#        setup_watir
#    end
#    def testDummy
#        # Insert correct test logic here
#        assert(true, "Dummy test")
#    end
#    def teardown
#        teardown_watir
#    end
#end

# end of skeleton code

class IePlus < Watir::IE
    def full_click(elem)
        elem.fire_event("onMouseDown")
        elem.fire_event("onMouseUp")
        elem.click
    end
end

module WatirTest
    attr :ie
    def init_watir
        @first_test = true
    end
    def setup_watir
        if @first_test
            @first_test = false
        else
            sleep(2)
        end
        @ie = IePlus.new
        @ie.add_checker( Proc.new { |inst| assert(!inst.contains_text("HTTP Status"), "HTTP error for " + inst.url + ":\n" + inst.text) } )
    end
    def teardown_watir
        @ie.close
    end
end

