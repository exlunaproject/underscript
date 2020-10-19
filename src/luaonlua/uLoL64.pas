unit uLoL64;

{
  LoL64 constants and functions
  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  SysUtils, Lua, pLua,
  Cromis.Comm.Custom, Cromis.Comm.IPC, Cromis.Threading, Cromis.AnyValue,
  CatUtils, CatStrings, CatTasks;

const
 cLOL_REQUIRE = 1;
 cLOL_CALLFUNCTION = 2;
 cLOL_ACCESSPROP = 3;
 cLOL_SHUTDOWN = 4;
 cLOL_SETSID = 5;

var
 vPID_Host32: Cardinal = 0;
 vSID_Host: string = '';
 vservername: string = '';

type
 TLOLHostSettings = record
   Exefilename: string;
   SearchPath: string;
 end;

const
 cLOLHost32: TLOLHostSettings  = (
   Exefilename: 'lua51host.exe';
   Searchpath: 'Extensions\underscript32\lua51\';
 );

const
 cLOLHost64: TLOLHostSettings  = (
   Exefilename: 'lua51host.exe';
   Searchpath: '';
 );

function SendMessageToHost(id: integer; l: string):string;

implementation

function SendMessageToHost(id: integer; l: string):string;
var
  IPCResult: IIPCData;
  Request: IIPCData;
  IPCClient: TIPCClient;
  ret:string;
begin
  result := emptystr;
  IPCClient := TIPCClient.Create;
  try
    IPCClient.ComputerName := EmptyStr;
    IPCClient.ServerName := vservername;
    IPCClient.ConnectClient(cDefaultTimeout);
    try
      if IPCClient.IsConnected then
      begin
        Request := AcquireIPCData;
        Request.ID := DateTimeToStr(Now);
        Request.Data.WriteInteger('CmdID', id);
        Request.Data.WriteString('Command', l);
        Request.Data.WriteString('SID', vSID_Host);
        IPCResult := IPCClient.ExecuteConnectedRequest(Request);


          if IPCClient.AnswerValid then
          begin
            if ipcresult.ID = 'ret' then begin
              ret := IPCResult.Data.ReadString('json');
              result :=ret;
            end;
          end;


      end;
    finally
      IPCClient.DisconnectClient;
    end;
  finally
    IPCClient.Free;
  end;
end;

initialization

finalization
 if vPID_Host32 <> 0 then begin
   KillTaskbyPID(vPID_Host32);
   SendMessageToHost(cLOL_SHUTDOWN,emptystr);
 end;

end.
