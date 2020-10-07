unit UndHelper_Multi;

{
  UnderScript Helper object
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Variants, UndConst, CatUtils;

type
{$METHODINFO ON}
  TUndHelper = class
  public
    constructor Create;
    procedure Debug(s: String);
    procedure Write(s: String);
    procedure WriteLn(s: String);
  end;
{$METHODINFO OFF}

var
  UndHelper: TUndHelper;

implementation

constructor TUndHelper.Create;
begin
  inherited Create;
end;

procedure TUndHelper.Debug(s: String);
begin
  OutDebug(s);
end;

procedure TUndHelper.Write(s: String);
begin
  System.Write(s);
end;

procedure TUndHelper.WriteLn(s: String);
begin
  System.WriteLn(s);
end;

initialization

UndHelper := TUndHelper.Create;

finalization

UndHelper.free;

end.
