///////////////////
// Helper functions
///////////////////

function DoNothing1(varDummy1) {}
function DoNothing2(varDummy1, varDummy2) {}

////////////
// "Classes"
////////////

// Counter

function Count_FilePreamble(strFile, bIsOnlyFile)
{
	this.cFileMatches = 0;
}

function Count_Action(strLine)
{
	++this.cFileMatches;
	++this.cTotalMatches;
	
	return this.bCountReqd;
}

function Count_FilePostamble(strFile, bIsOnlyFile)
{
	if (bIsOnlyFile)
	{
		if (this.bCountReqd)
			WScript.Echo(this.cFileMatches);
		else
			WScript.Echo('Match found');
	}
	else if (this.cFileMatches > 0)
	{
		if (this.bCountReqd)
			WScript.Echo(strFile + ": " + this.cFileMatches);
		else
			WScript.Echo(strFile);
	}
}

function Count_ProgPostamble(bOneFileOnly)
{
	if (this.bCountReqd && !bOneFileOnly)
		WScript.Echo('Total: ' + this.cTotalMatches);
}

function Counter(bCountReqd)
{
	// properties
	this.bCountReqd		= bCountReqd;
	this.cFileMatches	= 0;
	this.cTotalMatches	= 0;

	// methods
	this.FilePreamble	= Count_FilePreamble;
	this.Action			= Count_Action;
	this.FilePostamble	= Count_FilePostamble;
	this.ProgPostamble	= Count_ProgPostamble;
}

// Printer

function Print_Action(strLine)
{
	if (bSingleFile)
		WScript.Echo((this.bNumbered ? nLineNumber : '') + strLine);
	else
		WScript.Echo(strCurrFile + (this.bNumbered ? '(' + nLineNumber + ')' : '') + ': ' + strLine);
	
	return true;
}

function Printer(bNumbered)
{
	// properties
	this.bNumbered		= bNumbered;

	// methods
	this.FilePreamble	= DoNothing2;
	this.Action			= Print_Action;
	this.FilePostamble	= DoNothing2;
	this.ProgPostamble	= DoNothing1;
}

///////////////
// Main program
///////////////

var mapOpts =
{
	'c' : false // (c)ount
,	'i' : false // (i)gnore case
,	'l' : false // (l)ist files
,	'n' : false // show line (n)umbers
,	'r' : false // (r)ecursively match file names
,	'v' : false // in(v)ert match (i.e. show non-matching lines)
};

var i, cArgs = WScript.Arguments.length;
var nFirstNonOptArg = cArgs;

for (i = 0; i < cArgs; ++i)
{
	var strArg = WScript.Arguments(i);
	if (strArg == '--')
	{
		// end of args flag (to allow REs beginning with '-')
		nFirstNonOptArg = ++i;
		break;
	}
	else if (strArg.charAt(0) == '-')
	{
		var j, cChars = strArg.length;
		for (j = 1; j < cChars; ++j)
			mapOpts[strArg.charAt(j)] = true;
	}
	else
	{
		nFirstNonOptArg = i;
		break;
	}
}

if (nFirstNonOptArg == cArgs)
{
	// No RE
	WScript.Echo('usage: jsgrep [-cilnrv] regexp [file ...]');
	WScript.Quit(1);
}

var arrFiles = new Array();
var oShell = null;

for (i = nFirstNonOptArg + 1; i < cArgs; ++i)
{
	var strFileArg = WScript.Arguments(i);
	if (strFileArg.indexOf('*') >= 0 || strFileArg.indexOf('?') >= 0)
	{
		if (oShell == null)
			oShell = new ActiveXObject('WScript.Shell');
		var nLastSep = strFileArg.lastIndexOf('\\');
		var strPath = (nLastSep >= 0) ? strFileArg.slice(0, ++nLastSep) : '';
		var strCmd = 'cmd.exe /c dir /b ';
		if (mapOpts['r'])
			strCmd += '/s ';
		var procGlob = oShell.Exec(strCmd + strFileArg);
		while (!procGlob.StdOut.AtEndOfStream)
		{
			if (mapOpts['r'])
				arrFiles[arrFiles.length] = procGlob.StdOut.ReadLine();
			else
            	arrFiles[arrFiles.length] = strPath + procGlob.StdOut.ReadLine();
		}
	}
	else
		arrFiles[arrFiles.length] = strFileArg;
}

var cFiles = arrFiles.length;

if (cFiles == 0)
{
	if (nFirstNonOptArg < cArgs - 1)
	{
		WScript.Echo('no matching files found');
		WScript.Quit(1)
	}
	else
	{
		arrFiles[0] = '-'; // stdin
		cFiles = 1;
	}
}

var bSingleFile = (cFiles == 1);

var re = new RegExp(WScript.Arguments(nFirstNonOptArg), mapOpts['i'] ? 'i' : '');
var bInvertMatch = mapOpts['v'];

var oAction	= mapOpts['c'] ? new Counter(true) : mapOpts['l'] ? new Counter(false) : new Printer(mapOpts['n']);

var strCurrFile = '', nLineNumber = 1;
var oFSO = new ActiveXObject('Scripting.FileSystemObject');
var ts;

for (i = 0; i < cFiles; ++i)
{
	strCurrFile = arrFiles[i];
	var bNeedClose = false;

	if (strCurrFile == '-')
	{
		ts = WScript.stdin;
	}
	else
	{
		try
		{
			ts = oFSO.OpenTextFile(strCurrFile);
			bNeedClose = true;
		}
		catch(e)
		{
			WScript.Echo('Failed to open "' + strCurrFile + '" - ' + e.description);
			continue;
		}
	}

	nLineNumber = 1;
	oAction.FilePreamble(strCurrFile, bSingleFile);
	
	var bContinue = true;

	while (bContinue && !ts.AtEndOfStream)
	{
		var strLine = ts.ReadLine();

		if ((strLine.match(re) == null) == bInvertMatch)
			bContinue = oAction.Action(strLine);

		++nLineNumber;
	}

	oAction.FilePostamble(strCurrFile, bSingleFile);

	if (bNeedClose) ts.Close();
}

oAction.ProgPostamble(bSingleFile);