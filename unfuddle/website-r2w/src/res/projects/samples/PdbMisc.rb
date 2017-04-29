######################################################################
# Static initialization of settings
#   Mixin module to control per class settings
######################################################################

module StaticInit
    @@staticInitDone = {}
    def doStaticInit(list)
        directCaller = caller[0]
        unless @@staticInitDone[directCaller]
            list.each { |item| doSetting(item) }
        end
        @@staticInitDone[directCaller] = true
    end
    def doSetting(item)
        eval "@@#{item.varname} = ($#{item.setting} == nil) ? \"#{item.defval.gsub(/\\/, "\\\\\\\\")}\" : $#{item.setting}"
    end
end

class Setting
    attr_reader :varname, :defval
    def initialize(varname = "", defval = "")
        @varname	= varname
        @defval		= defval
    end
    def setting
        @varname.downcase
    end
    def Setting.processArgs
        arg = nil
        while (arg = ARGV.shift)
            break unless (arg =~ /^--?(.*)/)
            name = $1.downcase
            value = (ARGV[0] =~ /^-/) ? "" : ARGV.shift
            eval "$#{name} = \"#{value}\""
        end
    end
end

######################################################################
# Debug support
#   Helper class to hold debug setting and permit call iff debugging,
#   plus amendment to Kernel#eval to dump command if debugging
######################################################################

class Debug
    @@debug = false
    def Debug.set
        @@debug = true
    end
    def Debug.unset
        @@debug = false
    end
    def Debug.call
        yield if @@debug
    end
end

module Kernel
    alias oldEval eval
    def eval(*all)
        Debug.call { print "EVAL: ", all.join(" "), "\n" }
        oldEval(*all)
    end
end

######################################################################
# Miscellaneous functions
######################################################################

module PdbMisc
    # ttata = "Try, try and try again!"
    # Example usages below
    def ttata(timeout=3, interval=1, logger=nil)
        raise "Block expected" unless block_given?
        while true
            begin
                return yield
            rescue
                logger.call "ttata: failed - #{$!}" if logger
                timeout -= interval
                raise if timeout <= 0
                sleep interval
            end
        end
    end
end

######################################################################
# Example/test code (executed iff script is run directly)
######################################################################

if caller.empty?
    include PdbMisc
    ttata { puts "Hello" }
    puts ( ttata { "Goodbye" } )
    begin
        a = ttata(2, 1, Kernel.method("puts")) do
            x = 1
            y = 2
            z = 3
            w = 21 / (z - y - x)
        end
    rescue; end

    require "Log4r"
    include Log4r
    mylog = Logger.new 'mylog'
    mylog.outputters = Outputter.stdout
    begin
        ttata(3, 1, mylog.method("warn")) { 1 / 0 }
    rescue; end
end
