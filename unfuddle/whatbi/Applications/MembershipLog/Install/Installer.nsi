;NSIS-script for MembershipLog installer
;Written by Paul D. Bartlett

;--------------------------------
;Includes/macros/defines

  !include "MUI.nsh" ;Modern UI
  !include WordFunc.nsh
  !insertmacro VersionCompare
  !include LogicLib.nsh
  
  !define BUILD_DIR         "..\..\..\bin"
  !define APP_LONG_NAME     "Membership Log"
  !define APP_SHORT_NAME    "MembershipLog"
  !define APP_EXE_NAME      "${APP_SHORT_NAME}.exe"
  !define PDB_REG_KEY       "Software\PDBartlett"
  !define APP_REG_KEY       "${PDB_REG_KEY}\${APP_SHORT_NAME}"
  !define INSTDIR_REG_VAL   "Install Directory"
  !define UNINSTALLER_EXE   "Uninstall.exe"

;--------------------------------
;Variables

  Var MUI_TEMP
  Var STARTMENU_FOLDER

;--------------------------------
;General

  ;Name and file
  Name "${APP_LONG_NAME}"
  OutFile "${BUILD_DIR}\Setup.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES\${APP_SHORT_NAME}"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU ${APP_REG_KEY} ${INSTDIR_REG_VAL)

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE ".\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY

  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "${APP_REG_KEY}" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
  !insertmacro MUI_PAGE_STARTMENU StartMenuPageId $STARTMENU_FOLDER

  !insertmacro MUI_PAGE_INSTFILES

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Required files" SecReqd

  SetOutPath "$INSTDIR"
  SectionIn RO
  
  File "${BUILD_DIR}\${APP_EXE_NAME}"
  File "${BUILD_DIR}\Pdbartlett.Whatbi.dll"

  !insertmacro MUI_STARTMENU_WRITE_BEGIN StartMenuPageId
  CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"
  CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\${APP_LONG_NAME}.lnk" "$INSTDIR\${APP_EXE_NAME}"
  CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Uninstall ${APP_LONG_NAME}.lnk" "$INSTDIR\${UNINSTALLER_EXE}"
  !insertmacro MUI_STARTMENU_WRITE_END

  IfFileExists "$SMPROGRAMS\$STARTMENU_FOLDER" 0 skipSpecialSMItems
  SetOutPath "$SMPROGRAMS\$STARTMENU_FOLDER"
  File ".\Membership Log Writeboard.url"
  SetOutPath "$INSTDIR"
  skipSpecialSMItems:

  ;Store installation folder
  WriteRegStr HKCU "${APP_REG_KEY}" "${INSTDIR_REG_VAL}" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\${UNINSTALLER_EXE}"

SectionEnd

Section "Documentation" SecDoc
  SetOutPath "$INSTDIR"
  File "${BUILD_DIR}\${APP_SHORT_NAME}.chm"
  CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\${APP_LONG_NAME} Manual.lnk" "$INSTDIR\${APP_SHORT_NAME}.chm"
SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  MessageBox MB_YESNO "Delete data and settings?" IDNO skipDelData
  Delete "$LOCALAPPDATA\${APP_SHORT_NAME}\MemberData.xml"
  Delete "$LOCALAPPDATA\${APP_SHORT_NAME}\Settings.xml"
  RMDir "$LOCALAPPDATA\${APP_SHORT_NAME}"
  skipDelData:

  !insertmacro MUI_STARTMENU_GETFOLDER StartMenuPageId $MUI_TEMP
  Delete "$SMPROGRAMS\$MUI_TEMP\${APP_LONG_NAME}.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\${APP_LONG_NAME} Manual.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Uninstall ${APP_LONG_NAME}.lnk"
  Delete "$SMPROGRAMS\$MUI_TEMP\Membership Log Writeboard.url"
  
  ;Delete empty start menu parent diretories
  StrCpy $MUI_TEMP "$SMPROGRAMS\$MUI_TEMP"
  startMenuDeleteLoop:
  ClearErrors
  RMDir $MUI_TEMP
  GetFullPathName $MUI_TEMP "$MUI_TEMP\.."
  IfErrors startMenuDeleteLoopDone
  StrCmp $MUI_TEMP $SMPROGRAMS startMenuDeleteLoopDone startMenuDeleteLoop
  startMenuDeleteLoopDone:

  Delete "$INSTDIR\${APP_EXE_NAME}"
  Delete "$INSTDIR\Pdbartlett.Whatbi.dll"
  Delete "$INSTDIR\${APP_SHORT_NAME}.chm"
  Delete "$INSTDIR\${UNINSTALLER_EXE}"
  RMDir "$INSTDIR"

  DeleteRegValue HKCU ${APP_REG_KEY} "${INSTDIR_REG_VAL}"
  DeleteRegKey /ifempty HKCU ${APP_REG_KEY}
  DeleteRegKey /ifempty HKCU ${PDB_REG_KEY}

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecReqd ${LANG_ENGLISH} "Required components"
  LangString DESC_SecDoc  ${LANG_ENGLISH} "User manual"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecReqd} $(DESC_SecReqd)
    !insertmacro MUI_DESCRIPTION_TEXT ${SecDoc}  $(DESC_SecDoc)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Callback functions

Function .onInit
  Call CheckDotNetVersion
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Launch ${APP_LONG_NAME} application now?" IDNO NoLaunch
    Exec "$INSTDIR\${APP_EXE_NAME}"
  NoLaunch:
FunctionEnd

;--------------------------------
;Other functions

Function CheckDotNETVersion
  Call GetDotNETVersion
  Pop $0
  ${If} $0 == "not found"
    MessageBox MB_OK|MB_ICONSTOP ".NET runtime library is not installed."
    Abort
  ${EndIf}
 
  StrCpy $0 $0 "" 1 # skip "v"
 
  ${VersionCompare} $0 "1.1" $1
  ${If} $1 == 2
    MessageBox MB_OK|MB_ICONSTOP ".NET runtime library v1.1 or newer is required. You have $0."
    Abort
  ${EndIf}
FunctionEnd

Function GetDotNETVersion
  Push $0
  Push $1
 
  System::Call "mscoree::GetCORVersion(w .r0, i ${NSIS_MAX_STRLEN}, *i) i .r1"
  StrCmp $1 "error" 0 +2
    StrCpy $0 "not found"
 
  Pop $1
  Exch $0
FunctionEnd
