unit OpenXRProgram;

// delphi port of : OpenXR-SDK-VisualStudio\samples\BasicXrApp

interface
uses  System.SysUtils, System.Classes
      , OpenXR
      ;

type

TxrVectorStrings = record
   list:TStringList;
   CharStringsArray:array of TXrCharString;
   RawStringsArray:array of PXrChar;
   procedure BuildArrays;
   class operator Initialize (out Dest: TxrVectorStrings);
   class operator Finalize(var Dest: TxrVectorStrings);
end;

TOptionalExtensions = record
   DepthExtensionSupported:boolean;
   UnboundedRefSpaceSupported:boolean;
   SpatialAnchorSupported:boolean;
   class operator Initialize (out Dest: ToptionalExtensions);
end;

TOpenXRProgram = class
   FName:string;
   FLibName:string;
   FLog:TStrings;
   FOptionalExtensions : TOptionalExtensions;
   FSelectedExtensions: TxrVectorStrings;
  protected
   procedure Log(const aString:string);
   procedure LogResult(const aResult:TXrResult);
   procedure LogCheck(const aCheckValue:boolean;const aMsg:string);

   procedure InitLibrary;
   procedure CreateInstance;
   procedure SelectExtensions;
  public
   constructor Create(const AppName:string;log:TStrings);
end;

implementation

{ TOpenXRProgram }

constructor TOpenXRProgram.Create(const AppName: string;log:TStrings);
begin
  FName := AppName;
  FLog := log;
  InitLibrary;
  CreateInstance;

end;

procedure TOpenXRProgram.CreateInstance;
begin
  // Build out the extensions to enable. Some extensions are required and some are optional.
  SelectExtensions;

end;

procedure TOpenXRProgram.InitLibrary;
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

procedure TOpenXRProgram.LogCheck(const aCheckValue: boolean; const aMsg: string);
begin
  if not aCheckValue then begin
    Log('check failed '+aMsg);
  end;
end;

procedure TOpenXRProgram.LogResult(const aResult: TXrResult);
begin
  if aResult <> XR_SUCCESS then begin
    Log('error '+inttostr(integer(aResult)));
  end;
end;

procedure TOpenXRProgram.SelectExtensions;
var extensionCount:TXrUInt32;
    res : TXrResult;
    extensionProperties:array of TXrExtensionProperties;
    extensionProperties_data:PXrExtensionProperties;
    i:integer;
    checkresult:boolean;


    // Add a specific extension to the list of extensions to be enabled, if it is supported.
    function EnableExtensionIfSupported(const extensionName:TXrCharString):boolean;
    var j:integer;
        aExtension:TXrCharString;
    begin
      result := false;
      for j := 0 to extensionCount-1 do begin
        aExtension := extensionProperties[j].extensionName;
        if aExtension = extensionName then begin
          FSelectedExtensions.List.Add(string(aExtension));
          result := true;
          break;
        end;
      end;
     end;
begin

  // Fetch the list of extensions supported by the runtime.
  extensionCount := 0;
  res := xrEnumerateInstanceExtensionProperties(nil, 0 ,@extensionCount, nil);
  LogResult(res);

  if extensionCount>0 then begin
    setlength( extensionProperties, extensionCount );
    for i := 0 to  extensionCount-1 do begin
      extensionProperties[i].type_ := XR_TYPE_EXTENSION_PROPERTIES;
    end;
    extensionProperties_data := @extensionProperties[0];
    res := xrEnumerateInstanceExtensionProperties(nil, extensionCount ,@extensionCount, extensionProperties_data);
    LogResult(res);
  end;

  // D3D11 extension is required for this sample, so check if it's supported.
  checkresult := EnableExtensionIfSupported(XR_KHR_D3D11_ENABLE_EXTENSION_NAME);
  LogCheck(checkResult,'D3D11 extension');

  // Additional optional extensions for enhanced functionality. Track whether enabled in FoptionalExtensions.
  FoptionalExtensions.DepthExtensionSupported := EnableExtensionIfSupported(XR_KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME);
  FoptionalExtensions.UnboundedRefSpaceSupported := EnableExtensionIfSupported(XR_MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME);
  FoptionalExtensions.SpatialAnchorSupported := EnableExtensionIfSupported(XR_MSFT_SPATIAL_ANCHOR_EXTENSION_NAME);

end;

{ ToptionalExtensions }

class operator ToptionalExtensions.Initialize(out Dest: ToptionalExtensions);
begin
  Dest.DepthExtensionSupported := false;
  Dest.UnboundedRefSpaceSupported := false;
  Dest.SpatialAnchorSupported := false;
end;

{ TxrVectorStrings }

procedure TxrVectorStrings.BuildArrays;
var i:integer;
begin
  SetLength(CharStringsArray,list.Count);
  SetLength(RawStringsArray,list.Count);
  for i:=0 to list.Count-1 do begin
   CharStringsArray[i]:=TXrCharString(list.Strings[i]);
   RawStringsArray[i]:=PXrChar(CharStringsArray[i]);
  end;
end;

class operator TxrVectorStrings.Finalize(var Dest: TxrVectorStrings);
begin
  setlength(Dest.RawStringsArray,0);
  setlength(Dest.CharStringsArray,0);
  Dest.list.free;
end;

class operator TxrVectorStrings.Initialize(out Dest: TxrVectorStrings);
begin
  Dest.list := TStringList.Create;
  setlength(Dest.CharStringsArray,0);
  setlength(Dest.RawStringsArray,0);
end;

end.
