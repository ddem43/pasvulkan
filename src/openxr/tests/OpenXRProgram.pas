﻿unit OpenXRProgram;

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

TNearFar = record
  _Near:single;
  _Far:single;
end;

TOpenXRProgram = class
   FName:string;
   FLibName:string;

   FxrInstanceHandle:TXrInstance;
   FActionSetHandle:TXrActionSet;
   FsubactionPaths: array[0..1] of TXrPath;


   //input action to place a hologram
   FPlaceActionHandle:TXrAction;
   //input action getting the left and right hand poses
   FPoseActionHandle:TXrAction;
   //output action for vibrating the left and right controller
   FVibrateActionHandle:TXrAction;
   //input action to exit session
   FExitActionHandle:TXrAction;

   FsystemId: UInt64;
   FenvironmentBlendMode: TXrEnvironmentBlendMode;
   FnearFar : TNearFar;

   const LeftSide = 0;
         RightSide = 1;
         FformFactor:TXrFormFactor = XR_FORM_FACTOR_HEAD_MOUNTED_DISPLAY;
         FprimaryViewConfigType:TXrViewConfigurationType = XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO;
         FstereoViewCount:UInt32 = 2;// PRIMARY_STEREO view configuration always has 2 views

   var
   FLog:TStrings;
   FOptionalExtensions : TOptionalExtensions;
   FSelectedExtensions: TxrVectorStrings;
   FSelectedLayers: TxrVectorStrings;


   FDoingInit:boolean;
  protected
   procedure Log(const aString:string);
   procedure LogResult(const aTitle:string;const aResult:TXrResult);
   procedure LogCheck(const aCheckValue:boolean;const aMsg:string);
   function GetXrPath(const apString:PXrChar):TXrPath;
   procedure InstanceInfoLog;
   procedure InitLibrary;
   procedure CreateInstance;
   procedure CreateActions;
   procedure SelectExtensions;
   procedure SelectLayers;

   // check headset
   procedure InstanceSystemInfo;

  public
   constructor Create(const AppName:string;log:TStrings);
   procedure InitializeSystem;
   procedure ContinueWithHeadset;

   property DoingInit:boolean read FDoingInit;
   property systemID: UInt64 read FsystemId;

end;

// these consts need to be moved to xr.pas
const XR_NULL_PATH=0;
      XR_NULL_SYSTEM_ID=0;

implementation

{ TOpenXRProgram }

procedure TOpenXRProgram.ContinueWithHeadset;
var res : TXrResult;
    count:UInt32;
    environmentBlendModes: array of TXrEnvironmentBlendMode;
    environmentBlendModes_data : PXrEnvironmentBlendMode;
begin
  // code here is run when FsystemID is ok
  if FsystemId <> XR_NULL_SYSTEM_ID then begin
      // Choose an environment blend mode.

      // Query the list of supported environment blend modes for the current system
      res := xrEnumerateEnvironmentBlendModes(FXrInstanceHandle,FsystemId,FprimaryViewConfigType,0,@count,nil);
      case res of
        XR_SUCCESS:begin
          if count>0 then begin
            setlength( environmentBlendModes, count );
            environmentBlendModes_data := @environmentBlendModes[0];
            res := xrEnumerateEnvironmentBlendModes(FXrInstanceHandle,FsystemId,FprimaryViewConfigType,count,@count,environmentBlendModes_data);
             case res of
               XR_SUCCESS:begin
                  // This sample supports all modes, pick the system's preferred one.
                  FenvironmentBlendMode := environmentBlendModes[0];
               end;
               else begin
                 LogResult('EnumerateEnvironmentBlendModes (get data)',res);
               end;
             end;
          end;
        end;
        else begin
          LogResult('EnumerateEnvironmentBlendModes (query list)', res);
        end;
      end;

      // Choose a reasonable depth range can help improve hologram visual quality.
      // Use reversed Z (near > far) for more uniformed Z resolution.
      FnearFar._Near := 20.0;
      FnearFar._Far  := 0.1;
  end;
end;

constructor TOpenXRProgram.Create(const AppName: string;log:TStrings);
begin
  FName := AppName;
  FxrInstanceHandle := XR_NULL_HANDLE;
  FActionSetHandle := XR_NULL_HANDLE;
  FsubactionPaths[0] := XR_NULL_PATH;
  FsubactionPaths[1] := XR_NULL_PATH;
  FPlaceActionHandle := XR_NULL_HANDLE;
  FPoseActionHandle := XR_NULL_HANDLE;
  FVibrateActionHandle := XR_NULL_HANDLE;
  FExitActionHandle := XR_NULL_HANDLE;

  FSystemId := XR_NULL_SYSTEM_ID;

  FLog := log;
  InitLibrary;

  CreateInstance;
  if (FxrInstanceHandle = XR_NULL_HANDLE) then exit;

  CreateActions;

end;

procedure TOpenXRProgram.CreateActions;
var actionSetInfo : TXrActionSetCreateInfo;
    pactionSetInfo: PXrActionSetCreateInfo;
    actionInfo    : TXrActionCreateInfo;
    pactionInfo   : PXrActionCreateInfo;
    res : TXrResult;

    Bindings : array of TXrActionSuggestedBinding;
    suggestedBindings : TXrInteractionProfileSuggestedBinding;

begin
  // Create an action set.
  actionSetInfo := TXrActionSetCreateInfo.Create('place_hologram_action_set','Placement',0);
  actionSetInfo.type_:= XR_TYPE_ACTION_SET_CREATE_INFO;
  pactionSetInfo := @actionSetInfo;
  res := xrCreateActionSet(FxrInstanceHandle,pactionSetInfo,@FActionSetHandle);
  LogResult('CreateActionSet',res);

  // Create actions.

    // Enable subaction path filtering for left or right hand.
    FsubactionPaths[LeftSide] := GetXrPath('/user/hand/left');
    FsubactionPaths[RightSide] := GetXrPath('/user/hand/right');

    // Create an input action to place a hologram.
    actionInfo := TXrActionCreateInfo.Create('place_hologram',XR_ACTION_TYPE_BOOLEAN_INPUT,2,@FsubactionPaths[0],'Place Hologram');
    actionInfo.type_:= XR_TYPE_ACTION_CREATE_INFO;
    pactionInfo := @actioninfo;
    res := xrCreateAction(FActionSetHandle,pactionInfo,@FPlaceActionHandle);
    LogResult('CreateAction (place hologram)',res);

    // Create an input action getting the left and right hand poses.
    actionInfo := TXrActionCreateInfo.Create('hand_pose',XR_ACTION_TYPE_POSE_INPUT,2,@FsubactionPaths[0],'Hand Pose');
    actionInfo.type_:= XR_TYPE_ACTION_CREATE_INFO;
    pactionInfo := @actioninfo;
    res := xrCreateAction(FActionSetHandle,pactionInfo,@FPoseActionHandle);
    LogResult('CreateAction (get hand poses)',res);

    // Create an output action for vibrating the left and right controller.
    actionInfo := TXrActionCreateInfo.Create('vibrate',XR_ACTION_TYPE_VIBRATION_OUTPUT,2,@FsubactionPaths[0],'Vibrate');
    actionInfo.type_:= XR_TYPE_ACTION_CREATE_INFO;
    pactionInfo := @actioninfo;
    res := xrCreateAction(FActionSetHandle,pactionInfo,@FVibrateActionHandle);
    LogResult('CreateAction (vibration)',res);

    // Create an input action to exit session
    actionInfo := TXrActionCreateInfo.Create('exit_session',XR_ACTION_TYPE_BOOLEAN_INPUT,2,@FsubactionPaths[0],'Exit Session');
    actionInfo.type_:= XR_TYPE_ACTION_CREATE_INFO;
    pactionInfo := @actioninfo;
    res := xrCreateAction(FActionSetHandle,pactionInfo,@FExitActionHandle);
    LogResult('CreateAction (exit session)',res);

  // Setup suggest bindings for simple controller.
  setLength(Bindings,0);
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FPlaceActionHandle , GetXrPath('/user/hand/right/input/select/click')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FPlaceActionHandle , GetXrPath('/user/hand/left/input/select/click')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FPoseActionHandle , GetXrPath('/user/hand/right/input/grip/pose')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FPoseActionHandle , GetXrPath('/user/hand/left/input/grip/pose')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FVibrateActionHandle , GetXrPath('/user/hand/right/output/haptic')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FVibrateActionHandle , GetXrPath('/user/hand/left/output/haptic')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FExitActionHandle , GetXrPath('/user/hand/right/input/menu/click')) ];
  Bindings := Bindings+ [ TXrActionSuggestedBinding.Create(FExitActionHandle , GetXrPath('/user/hand/left/input/menu/click')) ];

  suggestedBindings :=  TXrInteractionProfileSuggestedBinding.Create( GetXrPath('/interaction_profiles/khr/simple_controller'), 8, @Bindings[0] );
  suggestedBindings.type_ :=  XR_TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING;
  res := xrSuggestInteractionProfileBindings(FXrInstanceHandle,@suggestedBindings);
  LogResult('SuggestInteractionProfileBindings',res);

end;

procedure TOpenXRProgram.CreateInstance;
var createInfo : TXrInstanceCreateInfo;
    res : TXrResult;
begin
  // Build out the extensions to enable. Some extensions are required and some are optional.
  SelectExtensions;
  FSelectedExtensions.BuildArrays;

  // not required for instance creation
  // may change if a headset is connected ?
  SelectLayers;
  //FSelectedLayers.BuildArrays;

  // Create the instance with desired extensions.
  createInfo := Default(TXrInstanceCreateInfo);
  createInfo.type_:= XR_TYPE_INSTANCE_CREATE_INFO;

  createInfo.enabledApiLayerCount := FSelectedLayers.list.Count;
  if createInfo.enabledApiLayerCount > 0 then begin
   createInfo.enabledApiLayerNames := @FSelectedLayers.RawStringsArray[0];
  end;

  createInfo.enabledExtensionCount := FSelectedExtensions.list.count;
  if createInfo.enabledExtensionCount > 0 then begin
   createInfo.enabledExtensionNames :=  @FSelectedExtensions.RawStringsArray[0];
  end;


  // XR_MAKE_VERSION(1,0,2) = $ 0001 0000 0000 0002 = 281474976710658
  var api_version:TXrVersion := XR_MAKE_VERSION(1,0,17);
  createInfo.applicationInfo.create(TXrcharString(FName),1,TXrcharString('OpenXR sample'),1,api_version);

  res := xrCreateInstance(@createInfo,@FxrInstanceHandle);
  if res = XR_SUCCESS then begin
    InstanceInfoLog;
  end else begin
    LogResult('CreateInstance',res);
  end;


end;

function TOpenXRProgram.GetXrPath(const apString: PXrChar): TXrPath;
var res : TXrResult;
begin
  result := XR_NULL_PATH;
  if FXrInstanceHandle <> XR_NULL_HANDLE then begin
    res := xr.StringToPath(FXrInstanceHandle,apString,@result);
    LogResult('StringToPath',res);
  end;
end;

procedure TOpenXRProgram.InitializeSystem;
begin
  if (FxrInstanceHandle <> XR_NULL_HANDLE) then begin
    if (FsystemId=XR_NULL_SYSTEM_ID) then begin

      Log('try system initialisation...');
      FDoingInit := true;
      try
        InstanceSystemInfo;
      finally
        FDoingInit := false;
      end;

    end;
  end;
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

procedure TOpenXRProgram.InstanceInfoLog;
var res : TXrResult;
    instProps:TXrInstanceProperties;
    pinstProps:PXrInstanceProperties;
begin
  if FXrInstanceHandle <> XR_NULL_HANDLE then begin
   instProps := Default(TXrInstanceProperties);
   instProps.type_ :=  XR_TYPE_INSTANCE_PROPERTIES;
   instProps.next := nil;
   pinstProps := @instProps;
   res := xrGetInstanceProperties(FXrInstanceHandle,pinstProps);
   if res = XR_SUCCESS then begin
    Log('Active Runtime '+instProps.runtimeName);
   end else begin
     LogResult('GetInstanceProperties',res);
   end;
  end else begin
   Log('OpenXr instance Creation failed');
  end;
end;

procedure TOpenXRProgram.InstanceSystemInfo;
var systemInfo:TXrSystemGetInfo;
    res : TXrResult;
begin
  systemInfo.Type_ := XR_TYPE_SYSTEM_GET_INFO;
  systemInfo.next := nil;
  systemInfo.formFactor := FformFactor; //XR_FORM_FACTOR_HEAD_MOUNTED_DISPLAY

  res := xrGetSystem(FXrInstanceHandle,@systemInfo,@FsystemID);
  case res of
    XR_ERROR_FORM_FACTOR_UNAVAILABLE : begin
       Log('HeadSet not connected, trying again in 2s');
    end;
    XR_SUCCESS : begin
     log('HeadSet connected, ID = '+inttostr(FsystemID));
     //break;
    end;
    else begin
      LogResult('GetSystem',res);
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

procedure TOpenXRProgram.LogResult(const aTitle:string; const aResult: TXrResult);
begin
  if aResult <> XR_SUCCESS then begin
    Log(aTitle+' : error '+inttostr(integer(aResult)));
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
  LogResult('EnumerateInstanceExtensionProperties (count only) ',res);

  if extensionCount>0 then begin
    setlength( extensionProperties, extensionCount );
    for i := 0 to  extensionCount-1 do begin
      extensionProperties[i].type_ := XR_TYPE_EXTENSION_PROPERTIES;
    end;
    extensionProperties_data := @extensionProperties[0];
    res := xrEnumerateInstanceExtensionProperties(nil, extensionCount ,@extensionCount, extensionProperties_data);
    LogResult('EnumerateInstanceExtensionProperties',res);
  end;

  if res=XR_SUCCESS then Log(inttostr(extensionCount)+' InstanceExtensionProperties');


  // D3D11 extension is required for this sample, so check if it's supported.
  checkresult := EnableExtensionIfSupported(XR_KHR_D3D11_ENABLE_EXTENSION_NAME);
  LogCheck(checkResult,'D3D11 extension');

  // Additional optional extensions for enhanced functionality. Track whether enabled in FoptionalExtensions.
  FoptionalExtensions.DepthExtensionSupported := EnableExtensionIfSupported(XR_KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME);
  FoptionalExtensions.UnboundedRefSpaceSupported := EnableExtensionIfSupported(XR_MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME);
  FoptionalExtensions.SpatialAnchorSupported := EnableExtensionIfSupported(XR_MSFT_SPATIAL_ANCHOR_EXTENSION_NAME);

end;

procedure TOpenXRProgram.SelectLayers;
var layerCount:TXrUInt32;
    res : TXrResult;
begin
  // layers supported by the runtime
  layerCount := 0;
  res := xrEnumerateApiLayerProperties( 0 ,@layerCount, nil);
  LogResult('EnumerateApiLayerProperties (count only)',res);

  if res=XR_SUCCESS then Log(inttostr(layerCount)+' ApiLayerProperties');

  // no api layers found
  if (layerCount = 0) then exit;

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
