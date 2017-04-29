require 'watir'
require 'test/unit'
require 'test/unit/ui/console/testrunner'

require 'WatirTest'

class GoogleTest < Test::Unit::TestCase
    include WatirTest
    def new
        init_watir
    end
    def setup
        setup_watir
        ie.goto("http://www.google.co.uk/")
    end
    def testWatir
        queryHelper("WATIR");
    end
    def testMe
        queryHelper("Paul Bartlett");
    end
    def queryHelper(query)
        ie.text_field(:name, "q").set(query)
        ie.button(:value, "Google Search").click
        assert(ie.contains_text(query))
    end
    def teardown
        teardown_watir
    end
end

