unit OpenXRProgram;

// delphi port of : OpenXR-SDK-VisualStudio\samples\BasicXrApp

interface
uses  System.Classes
      ,OpenXR
      ;

type

TOpenXRProgram = class
   FName:string;
   FLibName:string;
   FLog:TStrings;
  protected
    procedure Log(const aString:string);
    procedure initlibrary;
  public
    constructor Create(const AppName:string;log:TStrings);
end;

implementation

{ TOpenXRProgram }

constructor TOpenXRProgram.Create(const AppName: string;log:TStrings);
begin
  FName := AppName;
  FLog := log;
  initlibrary;

end;

procedure TOpenXRProgram.initlibrary;
var res:boolean;
begin
  if not assigned(LibOpenXR) then begin
    // dll obtained from OpenXR-SDK-VisualStudio\packages\OpenXR.Loader.1.0.10.2\native\Win32\release\bin\
    // and placed in output directory
    FLibName := XR_DEFAULT_LIB_NAME;
    res := LoadOpenXRLibrary(FLibName);
    if not res then begin
      Log('Error loading OpenXR library '+FLibName);
    end else begin
      Log('OpenXR library loaded');
      LoadOpenXRGlobalCommands;
    end;
  end;
end;

procedure TOpenXRProgram.Log(const aString: string);
begin
  if assigned(FLog) then FLog.Add(astring);
end;

end.
