unit SyActiveScript;
{
  ActiveScript.pas with small changes by Syhunt
  Search "Syhunt" for the changes.
}

{*******************************************************}
{                ActiveScript library                }
{                     version 1.1                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{*******************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is ActiveScript Library
 *
 * The Initial Developer of the Original Code is
 * Serhiy Perevoznyk
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, SysUtils, ActiveX, ComObj, Contnrs, Classes, Dialogs,
  {$IFNDEF VER130}
  Variants,
  {$ENDIF}
  Forms;

const
  SCATID_ActiveScript =               '{F0B7A1A1-9847-11cf-8F20-00805F2CD064}';
  SCATID_ActiveScriptParse =          '{F0B7A1A2-9847-11cf-8F20-00805F2CD064}';
  SID_IActiveScript =                 '{BB1A2AE1-A4F9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptParse =            '{BB1A2AE2-A4F9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptParseProcedureOld ='{1CFF0050-6FDD-11d0-9328-00A0C90DCAA9}';
  SID_IActiveScriptParseProcedure =   '{AA5B6A80-B834-11d0-932F-00A0C90DCAA9}';
  SID_IActiveScriptSite =             '{DB01A1E3-A42B-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptSiteWindow =       '{D10F6761-83E9-11cf-8F20-00805F2CD064}';
  SID_IActiveScriptSiteInterruptPoll ='{539698A0-CDCA-11CF-A5EB-00AA0047A063}';
  SID_IActiveScriptError =            '{EAE1BA61-A4ED-11cf-8F20-00805F2CD064}';
  SID_IBindEventHandler =             '{63CDBCB0-C1B1-11d0-9336-00A0C90DCAA9}';
  SID_IActiveScriptStats =            '{B8DA6310-E19B-11d0-933C-00A0C90DCAA9}';

  CATID_ActiveScript:                 TGUID = SCATID_ActiveScript;
  CATID_ActiveScriptParse:            TGUID = SCATID_ActiveScriptParse;
  IID_IActiveScript:                  TGUID = SID_IActiveScript;
  IID_IActiveScriptParse:             TGUID = SID_IActiveScriptParse;
  IID_IActiveScriptParseProcedureOld: TGUID = SID_IActiveScriptParseProcedureOld;
  IID_IActiveScriptParseProcedure:    TGUID = SID_IActiveScriptParseProcedure;
  IID_IActiveScriptSite:              TGUID = SID_IActiveScriptSite;
  IID_IActiveScriptSiteWindow:        TGUID = SID_IActiveScriptSiteWindow;
  IID_IActiveScriptSiteInterruptPoll: TGUID = SID_IActiveScriptSiteInterruptPoll;
  IID_IActiveScriptError:             TGUID = SID_IActiveScriptError;
  IID_IBindEventHandler:              TGUID = SID_IBindEventHandler;
  IID_IActiveScriptStats:             TGUID = SID_IActiveScriptStats;

// Constants used by ActiveX Scripting:
//

(* IActiveScript::AddNamedItem() input flags *)

  SCRIPTITEM_ISVISIBLE     = $00000002;
  SCRIPTITEM_ISSOURCE      = $00000004;
  SCRIPTITEM_GLOBALMEMBERS = $00000008;
  SCRIPTITEM_ISPERSISTENT  = $00000040;
  SCRIPTITEM_CODEONLY      = $00000200;
  SCRIPTITEM_NOCODE        = $00000400;
  SCRIPTITEM_ALL_FLAGS     =(SCRIPTITEM_ISSOURCE or
                             SCRIPTITEM_ISVISIBLE or
                             SCRIPTITEM_ISPERSISTENT or
                             SCRIPTITEM_GLOBALMEMBERS or
                             SCRIPTITEM_NOCODE or
                             SCRIPTITEM_CODEONLY);

(* IActiveScript::AddTypeLib() input flags *)

  SCRIPTTYPELIB_ISCONTROL    = $00000010;
  SCRIPTTYPELIB_ISPERSISTENT = $00000040;
  SCRIPTTYPELIB_ALL_FLAGS    = (SCRIPTTYPELIB_ISCONTROL or
                                SCRIPTTYPELIB_ISPERSISTENT);

(* IActiveScriptParse::AddScriptlet() and
   IActiveScriptParse::ParseScriptText() input flags *)

  SCRIPTTEXT_DELAYEXECUTION    = $00000001;
  SCRIPTTEXT_ISVISIBLE         = $00000002;
  SCRIPTTEXT_ISEXPRESSION      = $00000020;
  SCRIPTTEXT_ISPERSISTENT      = $00000040;
  SCRIPTTEXT_HOSTMANAGESSOURCE = $00000080;
  SCRIPTTEXT_ALL_FLAGS         = (SCRIPTTEXT_DELAYEXECUTION or
                                  SCRIPTTEXT_ISVISIBLE or
                                  SCRIPTTEXT_ISEXPRESSION or
                                  SCRIPTTEXT_ISPERSISTENT or
                                  SCRIPTTEXT_HOSTMANAGESSOURCE);

(* IActiveScriptParseProcedure::ParseProcedureText() input flags *)

  SCRIPTPROC_HOSTMANAGESSOURCE = $00000080;
  SCRIPTPROC_IMPLICIT_THIS     = $00000100;
  SCRIPTPROC_IMPLICIT_PARENTS  = $00000200;
  SCRIPTPROC_ALL_FLAGS         = (SCRIPTPROC_HOSTMANAGESSOURCE or
                                  SCRIPTPROC_IMPLICIT_THIS or
                                  SCRIPTPROC_IMPLICIT_PARENTS);

(* IActiveScriptSite::GetItemInfo() input flags *)

  SCRIPTINFO_IUNKNOWN  = $00000001;
  SCRIPTINFO_ITYPEINFO = $00000002;
  SCRIPTINFO_ALL_FLAGS = (SCRIPTINFO_IUNKNOWN or
                          SCRIPTINFO_ITYPEINFO);

(* IActiveScript::Interrupt() Flags *)

  SCRIPTINTERRUPT_DEBUG          = $00000001;
  SCRIPTINTERRUPT_RAISEEXCEPTION = $00000002;
  SCRIPTINTERRUPT_ALL_FLAGS      = (SCRIPTINTERRUPT_DEBUG or
                                    SCRIPTINTERRUPT_RAISEEXCEPTION);

(* IActiveScriptStats::GetStat() values *)

  SCRIPTSTAT_STATEMENT_COUNT   = 1;
  SCRIPTSTAT_INSTRUCTION_COUNT = 2;
  SCRIPTSTAT_INTSTRUCTION_TIME = 3;
  SCRIPTSTAT_TOTAL_TIME        = 4;

(* script state values *)

type
  tagSCRIPTSTATE = integer;
  SCRIPTSTATE = tagSCRIPTSTATE;
const
  SCRIPTSTATE_UNINITIALIZED = $00000000;
  SCRIPTSTATE_INITIALIZED   = $00000005;
  SCRIPTSTATE_STARTED       = $00000001;
  SCRIPTSTATE_CONNECTED     = $00000002;
  SCRIPTSTATE_DISCONNECTED  = $00000003;
  SCRIPTSTATE_CLOSED        = $00000004;

(* script thread state values *)

type
  tagSCRIPTTHREADSTATE = integer;
  SCRIPTTHREADSTATE = tagSCRIPTTHREADSTATE;
const
  SCRIPTTHREADSTATE_NOTINSCRIPT = $00000000;
  SCRIPTTHREADSTATE_RUNNING     = $00000001;

(* Thread IDs *)

type
  SCRIPTTHREADID = DWORD;
const
  SCRIPTTHREADID_CURRENT = SCRIPTTHREADID(-1);
  SCRIPTTHREADID_BASE    = SCRIPTTHREADID(-2);
  SCRIPTTHREADID_ALL     = SCRIPTTHREADID(-3);

type
  IActiveScriptSite =           interface;
  IActiveScriptSiteWindow =     interface;
  IActiveScript =               interface;
  IActiveScriptParse =          interface;
  IActiveScriptParseProcedure = interface;
  IActiveScriptError =          interface;
  LPCOLESTR = PWideChar;

  IActiveScriptSite = interface(IUnknown)
    [SID_IActiveScript]
    function GetLCID(out plcid: LCID): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function OnScriptTerminate(
      var pvarResult: OleVariant;
      var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function OnStateChange(ssScriptState: SCRIPTSTATE): HResult; stdcall;
    function OnScriptError(
      const pscripterror: IActiveScriptError): HResult; stdcall;
    function OnEnterScript: HResult; stdcall;
    function OnLeaveScript: HResult; stdcall;
  end;

  IActiveScriptError = interface(IUnknown)
    [SID_IActiveScriptError]
    function GetExceptionInfo(out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function GetSourcePosition(
      out pdwSourceContext: DWORD;
      out pulLineNumber: ULONG;
      out plCharacterPosition: Integer): HResult; stdcall;
    function GetSourceLineText(out pbstrSourceLine: WideString): HResult; stdcall;
  end;

  IActiveScriptSiteWindow = interface(IUnknown)
    [SID_IActiveScriptSiteWindow]
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

  IActiveScriptSiteInterruptPoll = interface(IUnknown)
    [SID_IActiveScriptSiteInterruptPoll]
    function QueryContinue: HResult; stdcall;
  end;

  IActiveScript = interface(IUnknown)
    [SID_IActiveScript]
    function SetScriptSite(const pass: IActiveScriptSite): HResult; stdcall;
    function GetScriptSite(
      const riid: TGUID;
      out ppvObject: Pointer): HResult; stdcall;
    function SetScriptState(ss: SCRIPTSTATE): HResult; stdcall;
    function GetScriptState(out pssState: SCRIPTSTATE): HResult; stdcall;
    function Close: HResult; stdcall;
    function AddNamedItem(
      pstrName: LPCOLESTR;
      dwFlags: DWORD): HResult; stdcall;
    function AddTypeLib(
      const rguidTypeLib: TGUID;
      dwMajor: DWORD;
      dwMinor: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function GetScriptDispatch(
      pstrItemName: LPCOLESTR;
      out ppdisp: IDispatch): HResult; stdcall;
    function GetCurrentScriptThreadID(
      out pstidThread: SCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadID(dwWin32ThreadId: DWORD;
      out pstidThread: SCRIPTTHREADID): HResult; stdcall;
    function GetScriptThreadState(
      stidThread: SCRIPTTHREADID;
      out pstsState: SCRIPTTHREADSTATE): HResult; stdcall;
    function InterruptScriptThread(
      stidThread: SCRIPTTHREADID;
      var pexcepinfo: EXCEPINFO;
      dwFlags: DWORD): HResult; stdcall;
    function Clone(out ppscript: IActiveScript): HResult; stdcall;
  end;

  IActiveScriptParse = interface(IUnknown)
    [SID_IActiveScriptParse]
    function InitNew: HResult; stdcall;
    function AddScriptlet(
      pstrDefaultName: LPCOLESTR;
      pstrCode: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      pstrSubItemName: LPCOLESTR;
      pstrEventName: LPCOLESTR;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out pbstrName: WideString;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
    function ParseScriptText(
      pstrCode: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out pvarResult: OleVariant;
      out pexcepinfo: EXCEPINFO): HResult; stdcall;
  end;

  IActiveScriptParseProcedureOld = interface(IUnknown)
    [SID_IActiveScriptParseProcedureOld]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  IActiveScriptParseProcedure = interface(IUnknown)
    [SID_IActiveScriptParseProcedure]
    function ParseProcedureText(
      pstrCode: LPCOLESTR;
      pstrFormalParams: LPCOLESTR;
      pstrProcedureName: LPCOLESTR;
      pstrItemName: LPCOLESTR;
      const punkContext: IUnknown;
      pstrDelimiter: LPCOLESTR;
      dwSourceContextCookie: DWORD;
      ulStartingLineNumber: ULONG;
      dwFlags: DWORD;
      out ppdisp: IDispatch): HResult; stdcall;
  end;

  IBindEventHandler = interface(IUnknown)
    [SID_IBindEventHandler]
    function BindHandler(
      pstrEvent: LPCOLESTR;
      const pdisp: IDispatch): HResult; stdcall;
  end;

  IActiveScriptStats = interface(IUnknown)
    [SID_IActiveScriptStats]
    function GetStat(
      stid: DWORD;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function GetStatEx(
      const guid: TGUID;
      out pluHi: ULONG;
      out pluLo: ULONG): HResult; stdcall;
    function ResetStats: HResult; stdcall;
  end;

type
 TOnActiveScriptError = procedure(Sender : TObject; Line, Pos : integer; ASrc : string; ADescription : string) of object;

  TSyScriptGlobalObjects = class(TObject)
  private
    FIntfList: IInterfaceList;
    FNamedList: TStrings;
    FGlobalMembers: TStrings;
  public
    constructor Create;
    function GetNamedItemCount: Integer;
    function GetNamedItemName(I: Integer): string;
    procedure AddNamedIntf(const AName: string; AIntf: IUnknown);
    function FindNamedItemIntf(const AName: string): IUnknown;
    destructor Destroy; override;
    property NamedItemCount: Integer read GetNamedItemCount;
    property NamedItemName[I: Integer]: string read GetNamedItemName;
  end;

  TSyActiveScriptSite = class(TComponent, IActiveScriptSite)
  private
    FUseSafeSubset : boolean;
    FDisp: OleVariant;
    FGlobalObjects : TSyScriptGlobalObjects;
    FOnError : TOnActiveScriptError;
    FParser: IActiveScriptParse;
    FScriptLanguage : string;
    procedure CreateScriptEngine(Language: string);
    procedure CloseScriptEngine;
  protected
    { IActiveScriptSite }
    function  GetLCID(out plcid: LongWord): HResult; stdcall;
    function GetItemInfo(
      pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
    function  GetDocVersionString(out pbstrVersion: WideString): HResult; stdcall;
    function  OnScriptTerminate(var pvarResult: OleVariant; var pexcepinfo: EXCEPINFO): HResult; stdcall;
    function  OnStateChange(ssScriptState: tagSCRIPTSTATE): HResult; stdcall;
    function  OnScriptError(const pscripterror: IActiveScriptError): HResult; stdcall;
    function  OnEnterScript: HResult; stdcall;
    function  OnLeaveScript: HResult; stdcall;
  public
    FEngine: IActiveScript; // Syhunt
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function RunExpression(ACode : Widestring) : string;
    procedure  Execute(ACode : WideString);
    procedure AddNamedItem(AName : string; AIntf : IUnknown);
    procedure AddGlobalMember(AName:string); //SYHUNT
    property ScriptInterface : OleVariant read FDisp;
  published
    property ScriptLanguage : string read FScriptLanguage write FScriptLanguage;
    property OnError : TOnActiveScriptError read FOnError write FOnError;
    property UseSafeSubset : boolean read FUseSafeSubset write FUseSafeSubset default false;
  end;

  TSyActiveScriptWindow = class(TSyActiveScriptSite, IActiveScriptSiteWindow)
  protected
    {IActiveSriptSiteWindow}
    function GetWindow(out phwnd: HWND): HResult; stdcall;
    function EnableModeless(fEnable: BOOL): HResult; stdcall;
  end;

procedure GetActiveScriptParse(List: TStrings);


implementation

const
  INTERFACESAFE_FOR_UNTRUSTED_CALLER = $00000001  // Caller of interface may be untrusted
  ;
  INTERFACESAFE_FOR_UNTRUSTED_DATA = $00000002  // Data passed into interface may be untrusted
  ;
  INTERFACE_USES_DISPEX = $00000004  // Object knows to use IDispatchEx
  ;
  INTERFACE_USES_SECURITY_MANAGER = $00000008  // Object knows to use IInternetHostSecurityManager
  ;


procedure GetActiveScriptParse(List: TStrings);
var
  ProgID: string;

  function ValidProgID: Boolean;
  var
    PID: string;
  begin
     if Length(ProgID) > 7 then
       Result := AnsiCompareStr('.Encode', Copy(ProgID, Length(ProgID)-6, 7)) <> 0
     else
       Result := True;
     // Exclude XML script engine
     if CompareText(Copy(ProgID, 1, 3), 'XML') = 0 then
       Result := False;
     // Exclude "signed" script engines
     PID := UpperCase(ProgID);
     if Pos('SIGNED', PID) <> 0 then
       Result := False;
  end;
var
  EnumGUID: IEnumGUID;
  Fetched: Cardinal;
  Guid: TGUID;
  Rslt: HResult;
  CatInfo: ICatInformation;
  I, BufSize: Integer;
  ClassIDKey: HKey;
  S: string;
  Buffer: array[0..255] of Char;
begin
  List.Clear;
  Rslt := CoCreateInstance(CLSID_StdComponentCategoryMgr, nil,
    CLSCTX_INPROC_SERVER, ICatInformation, CatInfo);
  if Succeeded(Rslt) then
  begin
    OleCheck(CatInfo.EnumClassesOfCategories(1, @CATID_ActiveScriptParse, 0, nil, EnumGUID));
    while EnumGUID.Next(1, Guid, Fetched) = S_OK do
    begin
      try
        ProgID := ClassIDToProgID(Guid);
        if ValidProgID then
          List.Add(ProgID);
      except
        ProgID := ClassIDToProgID(StringToGUID(Buffer));
        List.Add('Invalid Entry In Categories');
      end;
    end;
  end else
  begin
    if RegOpenKey(HKEY_CLASSES_ROOT, 'CLSID', ClassIDKey) <> 0 then
      try
        I := 0;
        while RegEnumKey(ClassIDKey, I, Buffer, SizeOf(Buffer)) = 0 do
        begin
          S := Format('%s\Implemented Categories\%s',[Buffer,  { do not localize }
            GUIDToString(CATID_ActiveScriptParse)]);
          if RegQueryValue(ClassIDKey, PChar(S), nil, BufSize) = 0 then
          begin
            ProgID := ClassIDToProgID(StringToGUID(Buffer));
            if ValidProgID then
              List.Add(ProgID);
          end;
          Inc(I);
        end;
      finally
        RegCloseKey(ClassIDKey);
      end;
  end;
end;

{ TSyActiveScriptSite }


constructor TSyActiveScriptSite.Create(AOwner : TComponent);
begin
  inherited;
  FScriptLanguage := 'VBScript';
  FGlobalObjects := TSyScriptGlobalObjects.Create;
  FUseSafeSubset := false;
end;

destructor TSyActiveScriptSite.Destroy;
begin
  CloseScriptEngine;
  FGlobalObjects.Free;
  inherited;
end;

procedure TSyActiveScriptSite.AddGlobalMember(AName:string); //SYHUNT
begin
 FGlobalObjects.FGlobalMembers.Add(aname)
end;

procedure TSyActiveScriptSite.AddNamedItem(AName: string;
  AIntf: IUnknown);
begin
  FGlobalObjects.AddNamedIntf(AName, AIntf);
end;



procedure TSyActiveScriptSite.CreateScriptEngine(
  Language: string);
const
  NULL_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';
var
  ScriptCLSID : TGUID;
  LanguageW : WideString;
  hr : HRESULT;
  i : integer;
  Disp: IDispatch;
  pOs : IObjectSafety;
  dwSupported : DWORD;
  dwEnabled : DWORD;
begin
  CloseScriptEngine;
  LanguageW := Language;
  if CLSIDFromProgID(PWideChar(LanguageW), ScriptCLSID) <> S_OK
    then ScriptCLSID := NULL_GUID;
  FEngine := CreateComObject(ScriptCLSID) as IActiveScript;
  if FUseSafeSubset then
   begin
     dwSupported := 0;
     dwEnabled := 0;
     FEngine.QueryInterface(IObjectSafety, pOS);
     if Assigned(pOS) then
      begin
        pOS.GetInterfaceSafetyOptions(IDispatch, @dwSupported, @dwEnabled);
          if (INTERFACE_USES_SECURITY_MANAGER and dwSupported) = INTERFACE_USES_SECURITY_MANAGER then
           begin
             dwEnabled := dwEnabled or INTERFACE_USES_SECURITY_MANAGER;
           end;
         pOS.SetInterfaceSafetyOptions(IDispatch, INTERFACE_USES_SECURITY_MANAGER, dwEnabled);
      end;
    end;

  hr := FEngine.QueryInterface(IActiveScriptParse, FParser);
  OLECHECK(hr);

  hr := FEngine.SetScriptSite(Self);
  OLECHECK(hr);

  hr := FParser.InitNew();
  OLECHECK(hr);

  for I := 0 to FGlobalObjects.NamedItemCount - 1 do begin
    if FGlobalObjects.FGlobalMembers.IndexOf(FGlobalObjects.NamedItemName[I]) = -1 then // SYHUNT
    FEngine.AddNamedItem(PWideChar(WideString(FGlobalObjects.NamedItemName[I])), SCRIPTITEM_ISVISIBLE) else
    FEngine.AddNamedItem(PWideChar(WideString(FGlobalObjects.NamedItemName[I])), SCRIPTITEM_ISVISIBLE or SCRIPTITEM_GLOBALMEMBERS);
  end;

  FEngine.GetScriptDispatch(nil, Disp);
  FDisp := Disp;
end;


procedure TSyActiveScriptSite.CloseScriptEngine;
begin
  FParser := nil;
  if FEngine <> nil then FEngine.Close;
  FEngine := nil;
  FDisp := Unassigned;
end;

function TSyActiveScriptSite.RunExpression(ACode: WideString): string;
var
  AResult: OleVariant;
  ExcepInfo: TEXCEPINFO;
begin
  //CreateScriptEngine(FScriptLanguage); //SYHUNT comentei para funcionar com o  SCRIPTTEXT_ISPERSISTENT
  try
  if FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0,
    SCRIPTTEXT_ISEXPRESSION, AResult, ExcepInfo) = S_OK
    then
      Result := AResult
        else
          Result := '';
  except end; // SYHUNT
end;


procedure TSyActiveScriptSite.Execute(ACode: Widestring);
var
  Result: OleVariant;
  ExcepInfo: TEXCEPINFO;
begin
  CreateScriptEngine(FScriptLanguage);
  FParser.ParseScriptText(PWideChar(ACode), nil, nil, nil, 0, 0, SCRIPTTEXT_ISPERSISTENT, Result, ExcepInfo); // syhunt
  FEngine.SetScriptState(SCRIPTSTATE_CONNECTED);
end;

function TSyActiveScriptSite.GetDocVersionString(
  out pbstrVersion: WideString): HResult;
begin
  Result := E_NOTIMPL;
end;

function TSyActiveScriptSite.GetItemInfo(pstrName: LPCOLESTR;
      dwReturnMask: DWORD;
      out ppiunkItem: IUnknown;
      out ppti: ITypeInfo): HResult; stdcall;
begin
  if @ppiunkItem <> nil then Pointer(ppiunkItem) := nil;
  if @ppti <> nil then Pointer(ppti) := nil;
  if (dwReturnMask and SCRIPTINFO_IUNKNOWN) <> 0
    then ppiunkItem := FGlobalObjects.FindNamedItemIntf(pstrName);
  Result := S_OK;
end;

function TSyActiveScriptSite.GetLCID(out plcid: LongWord): HResult;
begin
  plcid := GetSystemDefaultLCID;
  Result := S_OK;
end;

function TSyActiveScriptSite.OnEnterScript: HResult;
begin
  result := S_OK;
end;

function TSyActiveScriptSite.OnLeaveScript: HResult;
begin
  result := S_OK;
end;

function TSyActiveScriptSite.OnScriptError(
  const pscripterror: IActiveScriptError): HResult;
var
  wCookie   : Dword;
  ExcepInfo : TExcepInfo;
  CharNo    : integer;
  LineNo    : DWORD;
  SourceLineW : WideString;
  SourceLine : string;
  Desc : string;
begin
  Result := S_OK;
  wCookie := 0;
  LineNo  := 0;
  CharNo  := 0;
  if Assigned(pscripterror) then
    begin
      pscripterror.GetExceptionInfo(ExcepInfo);
      Desc := ExcepInfo.bstrDescription;
      pscripterror.GetSourcePosition(wCookie, LineNo, CharNo);
      pscripterror.GetSourceLineText(SourceLineW);
      SourceLine := SourceLineW;
      if Assigned(FOnError) then
        FOnError(Self, LineNo, CharNo, SourceLine, Desc);
    end;
end;

function TSyActiveScriptSite.OnScriptTerminate(var pvarResult: OleVariant;
  var pexcepinfo: EXCEPINFO): HResult;
begin
  Result := S_OK;
end;

function TSyActiveScriptSite.OnStateChange(
  ssScriptState: tagSCRIPTSTATE): HResult;
begin
   case ssScriptState of
     SCRIPTSTATE_UNINITIALIZED:;
     SCRIPTSTATE_INITIALIZED:;
     SCRIPTSTATE_STARTED:;
     SCRIPTSTATE_CONNECTED:;
     SCRIPTSTATE_DISCONNECTED:;
     SCRIPTSTATE_CLOSED:;
   end;

   Result := S_OK;

end;


{ TSyActiveScriptWindow }

function TSyActiveScriptWindow.EnableModeless(fEnable: BOOL): HResult;
begin
  Result := S_OK;
end;

function TSyActiveScriptWindow.GetWindow(out phwnd: HWND): HResult;
begin
  if (Owner is TCustomForm) then
   begin
    phwnd := (Owner as TCustomForm).Handle;
    Result := S_OK;
   end
    else
      begin
        phwnd := 0;
        Result := S_FALSE;
      end;
end;


{ TSyScriptGlobalObjects }

procedure TSyScriptGlobalObjects.AddNamedIntf(const AName: string; AIntf: IUnknown);
begin
  FNamedList.Add(AName);
  FIntfList.Add(AIntf);
end;

constructor TSyScriptGlobalObjects.Create;
begin
  inherited Create;
  FNamedList := TStringList.Create;
  FIntfList := TInterfaceList.Create;
  FGlobalMembers:=TStringList.create;// Syhunt
end;

destructor TSyScriptGlobalObjects.Destroy;
begin
  inherited;
  FNamedList.Free;
  FGlobalMembers.free; // Syhunt
end;

function TSyScriptGlobalObjects.FindNamedItemIntf(const AName: string): IUnknown;
var
  I: Integer;
begin
  I := FNamedList.IndexOf(AName);
  if I >= 0 then
    Result := FIntfList[I]
  else
    Result := nil;
end;

function TSyScriptGlobalObjects.GetNamedItemCount: Integer;
begin
  Result := FNamedList.Count;
end;

function TSyScriptGlobalObjects.GetNamedItemName(I: Integer): string;
begin
  Result := FNamedList[I];
end;

end.
