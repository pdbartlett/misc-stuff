# amend option processing for all CLR build items
# - defines a pre-processor symbol reflecting the selected CLR version, e.g. FX_V10, FX_V11, FX_V20
class ClrBuildItem
    alias oldOptions options
    def options
        oldOptions.unshift("/d:FX_V#{(ClrCompiler.ClrVersion.to_f * 10.0).to_i}")
    end
end # ClrBuildItem

####################
# define clean items
####################

CleanByExtension.new("Delete executables",          "exe"           )
CleanByExtension.new("Delete libraries",            "dll"           )
CleanByExtension.new("Delete debug files",          "pdb"           )
CleanByExtension.new("Delete compiled HTML files",  "chm"           )
CleanByExtension.new("Delete backup files",         "bak"           )
CleanByWildcard .new("Delete test data",            "TestData.xml"  )
CleanByWildcard .new("Delete test results",         "TestResult.xml")

####################
# define build items
####################

$stream = $stream || "trunk"

# WhaTBI? framework

$sqlite = "support\\SQLite.NET.dll"

ClrClassLibrary.new("Pdbartlett",          $sqlite, "Whatbi",           $csc,   "Framework\\#{$stream}"                                                     )
ClrClassLibrary.new("Pdbartlett.Whatbi",   "",      "Examples",         $csc,   "Framework\\#{$stream}\\Examples",          "Whatbi"                        )

ClrConsoleApplication.new(                 "",      "Pretest",          $csc,   "Framework\\#{$stream}\\Pretest", "",       "Whatbi", "Examples"            )

ClrTestLibrary.new("Pdbartlett.Whatbi",    "",      "Tests",            $csc,   "Framework\\#{$stream}\\Tests",             "Whatbi", "Examples", "Pretest" )

unless $skipdbtests
ClrTestLibrary.new("Pdbartlett.Whatbi",    "",      "DbTests",          $csc,   "Framework\\#{$stream}\\Tests\\Db",         "Whatbi", "Examples", "Tests"   )
end

# WhaTBI? apps

WinFormsApplication .new(                  "",      "MembershipLog",    $csc,   "Applications\\MembershipLog", "app.ico",   "Whatbi"                        )

if $release
DoxygenDocs         .new("MemLogDocs",      "Applications\\MembershipLog\\Docs\\Doxyfile",          "MembershipLog"                 )
NsisInstaller       .new("MemLogSetup",     "Applications\\MembershipLog\\Install\\Installer.nsi",  "MembershipLog", "MemLogDocs"   )
MsciPackager        .new("MemLogPackage",   "Applications\\MembershipLog\\Install\\Installer.sed",  "MemLogSetup"                   )
end

# WhatAGem (Ruby port)

RubyTestScript      .new("WhatAGemTests", "ports\\WhatAGem\\Framework\\Tests\\TestRunner.rb")
