require "PdbMisc"

class BuildTool
    def initialize(executable, *toolOptions)
        @executable = executable
        @toolOptions = toolOptions
    end
    def command(options)
        allOptions = @toolOptions + options
        allOptions.unshift(@executable).join(" ")
    end
end # BuildTool

class Compiler < BuildTool
    attr_reader :srcExtension
    def initialize(executable, srcExtension)
        super(executable)
        @srcExtension = srcExtension
    end
end # Compiler

class ClrCompiler < Compiler
    include StaticInit
    def initialize(*all)
        @@frameworks = {    "sdk10"     => [ "1.0", "\"C:\\Program Files\\Microsoft.NET\\SDK\\v1.0\\bin\\sdkvars.bat\"" ] ,
                            "sdk11"     => [ "1.1", "\"C:\\Program Files\\Microsoft.NET\\SDK\\v1.1\\bin\\sdkvars.bat\"" ] ,
                            "sdk20"     => [ "2.0", "\"C:\\Program Files\\Microsoft.NET\\SDK\\v2.0\\bin\\sdkvars.bat\"" ] ,
                            "vs2003"    => [ "1.1", "\"C:\\Program Files\\Microsoft Visual Studio .NET 2003\\Common7\\Tools\\vsvars32.bat\"" ] ,
                            "vs2005"    => [ "2.0", "\"C:\\Program Files\\Microsoft Visual Studio 8\\Common7\\Tools\\vsvars32.bat\"" ] }
        super
        doStaticInit( [ Setting.new("clrStuff") ] ) # dummy value - overidden implementation
    end
    def doSetting(item)
        return super unless item.varname == "clrStuff"
        $net ||= "sdk11"
        raise "Invalid value for -net option: #{$net}" unless @@frameworks.include?($net)
        @@clrVersion, @@setupScript = @@frameworks[$net]
    end
    def command(options)
        "ECHO OFF && #{ClrCompiler.SetupScript} && " + super
    end
    def ClrCompiler.ClrVersion
        @@clrVersion
    end
    def ClrCompiler.SetupScript
        @@setupScript
    end
end # ClrCompiler

class Item
    def initialize(name, tool, path)
        @name = name
        @tool = tool
        @path = path
    end
    def execute
        # show title block
        title = "= #{@name} "
        puts "\n" + "=" * 79
        puts title + "=" * (79 - title.length)
        puts "=" * 79
        # get commands to execute
        cmds = commands
        # execute each (stopping if any fail)
        rc = true
        cmds.each do |cmd|
            cmd.strip!
            puts "Runnning '#{cmd}'..."
            puts "-" * 79
            rc = system(cmd)
            puts "-" * 79
            puts rc ? "Completed OK" : "Failed (errno=#{$? >> 8})"
            puts "-" * 79
            break unless rc
        end
        rc
    end
    def commands
        [ @tool.command(options) ]
    end
    def options
        [ @path ] # by default, path contains all the parameters to be passed to the tool
    end
end # Item

class BuildItem < Item
    include StaticInit
    @@itemsByName = {}
    def initialize(name, tool, path, *dependsOn)
        doStaticInit( [ Setting.new("outputDir", "bin") ] )
        # instance init
        super(name, tool, path)
        @dependsOn = dependsOn
        # register instance with class
        @@itemsByName.store(name, self)
    end
    def addToBuildOrder(list)
        return if list.include?(@name)
        @dependsOn.each do |dependName|
            BuildItem.getByName(dependName).addToBuildOrder(list)
        end
        list.push(@name)
    end
    def BuildItem.buildAll
        buildOrder = []
        @@itemsByName.each_value do |item|
            item.addToBuildOrder(buildOrder)
        end
        rc = true
        buildOrder.each do |name|
            break unless (rc = BuildItem.getByName(name).execute)
        end
        puts "\n" + "*" * 79
        puts "BUILD " + (rc ? "OK" : "FAILED")
        puts "*" * 79
    end
    def BuildItem.getByName(name)
        item = @@itemsByName[name]
        raise "Unknown dependency '#{name}'" if item == nil
        return item
    end
    def outputDir
        @@outputDir
    end
end # BuildItem

class ClrBuildItem < BuildItem
    def initialize(namespace, extra_refs, *rest)
        super(*rest)
        @namespace = namespace
        @extra_refs = extra_refs
    end
    def options
        optList = [ "/out:#{target}", "/t:#{targetType}" ]
        optList.push("/debug+") unless $release
        optList.push("/r:#{references.join(",")}") unless references.empty?
        optList.push("#{@path}\\*.#{@tool.srcExtension}")
        optList
    end
    def references
        refs = @dependsOn.collect { |name| BuildItem.getByName(name).target }
        refs += @extra_refs.split(",") unless @extra_refs.empty?
        refs
    end
end # ClrBuildItem

class ClrApplication < ClrBuildItem
    def initialize(refs, name, tool, path, icon, *dependsOn)
        super("", refs, name, tool, path, *dependsOn)
        @icon = icon
    end
    def options
        optList = super
        optList.push("/win32icon:#{@path}\\#{@icon}") unless @icon == ""
        optList
    end
    def target
        "#{@@outputDir}\\#{@name}.exe"
    end
end # ClrApplication

class ClrConsoleApplication < ClrApplication
    def targetType
        "exe"
    end
end # ClrConsoleApplication

class WinFormsApplication < ClrApplication
    def targetType
        "winexe"
    end
end # WinFormsApplication

class ClrClassLibrary < ClrBuildItem
    def target
        "#{@@outputDir}\\#{@namespace}.#{@name}.dll"
    end
    def targetType
        "library"
    end
end # ClrClassLibrary

class ClrTestLibrary < ClrClassLibrary
    include StaticInit
    def initialize(namespace, extra_refs, *rest)
        doStaticInit( [ Setting.new("nUnitDir", "C:\\Program Files\\TestDriven.NET 1.0\\NUnit") ] )
        super(namespace, "\"#{@@nUnitDir}\\NUnit.Framework.dll\",#{extra_refs}", *rest)
    end
    def commands
        if $skiptests
            puts "!!! WARINING: ALL TESTS BEING SKIPPED !!!"
            return []
        end
        cmds = super
        if $guitests
            cmds.push("\"#{@@nUnitDir}\\NUnit-Gui\" /run #{target}")
        else
            cmds.push("\"#{@@nUnitDir}\\NUnit-Console\" /nologo #{target}")
        end
        cmds
    end
end # ClrTestLibrary

class DoxygenDocs < BuildItem
    def initialize(name, path, *depends)
        super(name, $doxy, path, *depends)
    end
end # DoxygenDocs

class NsisInstaller < BuildItem
    def initialize(name, path, *depends)
        super(name, $nsis, path, *depends)
    end
end # NsisInstaller

class MsciPackager < BuildItem
    def initialize(name, path, *depends)
        super(name, $msci, path, *depends)
    end
end # MsciPackager

class BuildScript < BuildItem
    # nothing to override at the moment
end # BuildScript

class TestScript < BuildScript
    def commands
        if $skiptests
            puts "!!! WARINING: ALL TESTS BEING SKIPPED !!!"
            return []
        end
        super
    end
end # BuildScript

class RubyTestScript < TestScript
    def initialize(name, path, *dependsOn)
        testUI = $guitests ? "Tk" : "Console"
        super(name, $ruby, "-I support\\ruby #{path} #{testUI}", *dependsOn)
    end
end # RubyTestScript

class CleanItem < Item
    @@cleanItems = []
    def initialize(name, tool, path)
        super
        @@cleanItems.push(self)
    end
    def CleanItem.cleanAll
        @@cleanItems.each { |item| item.execute }
    end
end # CleanItem

class CleanByWildcard < CleanItem
    def initialize(name, wildcard)
        super(name, $shell, "DEL /S \"#{wildcard}\"")
    end
end # CleanByExtension

class CleanByExtension < CleanByWildcard
    def initialize(name, ext)
        super(name, "*.#{ext}")
    end
end # CleanByExtension

############
# main code
############

# first check our own arguments
if ARGV.length > 0 && ARGV[0] =~ /([-\/])d(ebug)?b(uild)?/i
    Debug.set
    ARGV.shift
end

# then process build script arguments
buildFile = ARGV.pop
Setting.processArgs
if (buildFile == nil || !ARGV.empty?)
    raise "Usage: #{$0} [options] build-def-file"
end

# declare global helpers
$csc    = ClrCompiler.new("csc", "cs")
$doxy   = BuildTool.new("doxygen")
$msci   = BuildTool.new("C:\\Program Files\\Microsoft Component Installer SDK for Windows\\IExpress\\IExpress", "/n", "/q", "/m")
$nsis   = BuildTool.new("makensis");
$ruby   = BuildTool.new("ruby")
$shell  = BuildTool.new("")

# load build definition
load buildFile

# build everything
CleanItem.cleanAll if $clean or $rebuild
BuildItem.buildAll unless $clean
