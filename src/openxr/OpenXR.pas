(*
** Copyright (c) 2015-2016 The Khronos Group Inc.
** Copyright (c) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de, the pascal headers)
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
**
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
*)
(*
** This header is generated from the Khronos OpenXR XML API Registry.
**
*)
unit OpenXR;
{$ifdef fpc}
 {$mode delphi}
 {$z4}
 {$packrecords c}
 {$define CAN_INLINE}
 {$define HAS_ADVANCED_RECORDS}
 {$notes off}
{$else}
 {$z4}
 {$undef CAN_INLINE}
 {$undef HAS_ADVANCED_RECORDS}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$define CAN_INLINE}
   {$define HAS_ADVANCED_RECORDS}
  {$ifend}
 {$endif}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$if defined(Android)}
 {$define XR_USE_PLATFORM_ANDROID_KHR}
{$elseif defined(Windows)}
 {$define XR_USE_PLATFORM_WIN32_KHR}
{$elseif defined(Unix) or defined(Linux)}
 {$ifdef WAYLAND}
  {$define XR_USE_PLATFORM_WAYLAND_KHR}
 {$endif}
 {$ifdef XCB}
  {$define XR_USE_PLATFORM_XCB_KHR}
 {$endif}
 {$ifdef XLIB}
  {$define XR_USE_PLATFORM_XLIB_KHR}
 {$endif}
{$ifend}

interface

uses {$if defined(Windows)}
      Windows,
      Winapi.D3DCommon,
      Winapi.D3D11,
      Winapi.D3D12,
      Vulkan,
     {$elseif defined(Unix)}
      BaseUnix,UnixType,dl,
     {$ifend}
     {$if defined(XLIB) and defined(OpenXRUseXLIBUnits)}x,xlib,{$ifend}
     {$if defined(XCB) and defined(OpenXRUseXCBUnits)}xcb,{$ifend}
     {$if defined(Wayland) and defined(OpenXRUseWaylandUnits)}Wayland,{$ifend}
     {$if defined(Android) and defined(OpenXRUseAndroidUnits)}Android,{$ifend}
     {$if defined(Fuchsia) and defined(OpenXRUseFuchsiaUnits)}Fuchsia,{$ifend}
     {$if defined(DirectFB) and defined(OpenXRUseDirectFBUnits)}DirectFB,{$ifend}
     SysUtils;

const XR_DEFAULT_LIB_NAME={$ifdef Windows}'openxr-1.dll'{$else}{$ifdef Android}'libopenxr.so'{$else}{$ifdef Unix}'libopenxr.so.1'{$else}'libopenxr'{$endif}{$endif}{$endif};

type PPXrInt8=^PXrInt8;
     PXrInt8=^TXrInt8;
     TXrInt8={$ifdef FPC}Int8{$else}ShortInt{$endif};

     PPXrUInt8=^PXrUInt8;
     PXrUInt8=^TXrUInt8;
     TXrUInt8={$ifdef FPC}UInt8{$else}Byte{$endif};

     PPXrInt16=^PXrInt16;
     PXrInt16=^TXrInt16;
     TXrInt16={$ifdef FPC}Int16{$else}SmallInt{$endif};

     PPXrUInt16=^PXrUInt16;
     PXrUInt16=^TXrUInt16;
     TXrUInt16={$ifdef FPC}UInt16{$else}Word{$endif};

     PPXrInt32=^PXrInt32;
     PXrInt32=^TXrInt32;
     TXrInt32={$ifdef FPC}Int32{$else}LongInt{$endif};

     PPXrUInt32=^PXrUInt32;
     PXrUInt32=^TXrUInt32;
     TXrUInt32={$ifdef FPC}UInt32{$else}LongWord{$endif};

     PPXrInt64=^PXrInt64;
     PXrInt64=^TXrInt64;
     TXrInt64=Int64;

     PPXrUInt64=^PXrUInt64;
     PXrUInt64=^TXrUInt64;
     TXrUInt64=UInt64;

     PPXrChar=^PXrChar;
     PXrChar=PAnsiChar;
     TXrChar=AnsiChar;

     PPXrPointer=^PXrPointer;
     PXrPointer=^TXrPointer;
     TXrPointer=Pointer;

     PPXrVoid=^PXrVoid;
     PXrVoid=Pointer;

     PPXrHalfFloat=^PXrHalfFloat;
     PXrHalfFloat=^TXrHalfFloat;
     TXrHalfFloat=TXrUInt16;

     PPXrFloat=^PXrFloat;
     PXrFloat=^TXrFloat;
     TXrFloat=Single;

     PPXrDouble=^PXrDouble;
     PXrDouble=^TXrDouble;
     TXrDouble=Double;

     PPXrPtrUInt=^PXrPtrUInt;
     PPXrPtrInt=^PXrPtrInt;
     PXrPtrUInt=^TXrPtrUInt;
     PXrPtrInt=^TXrPtrInt;
{$ifdef fpc}
     TXrPtrUInt=PtrUInt;
     TXrPtrInt=PtrInt;
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TXrPtrUInt=NativeUInt;
     TXrPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
{$ifdef cpu64}
     TXrPtrUInt=TXrUInt64;
     TXrPtrInt=TXrInt64;
{$else}
     TXrPtrUInt=TXrUInt32;
     TXrPtrInt=TXrInt32;
{$endif}
{$endif}

     PPXrSizeUInt=^PXrSizeUInt;
     PXrSizeUInt=^TXrSizeUInt;
     TXrSizeUInt=TXrPtrUInt;

     PPXrSizeInt=^PXrSizeInt;
     PXrSizeInt=^TXrSizeInt;
     TXrSizeInt=TXrPtrInt;

     PPXrSize=^PXrSizeUInt;
     PXrSize=^TXrSizeUInt;
     TXrSize=TXrPtrUInt;

     PPXrPtrDiff=^PXrPtrDiff;
     PXrPtrDiff=^TXrPtrDiff;
     TXrPtrDiff=TXrPtrInt;

     PPXrCharString=^PXrCharString;
     PXrCharString=^TXrCharString;
     TXrCharString=AnsiString;

{$ifdef Android}
     PPXrAndroidANativeWindow=^PXrAndroidANativeWindow;
     PXrAndroidANativeWindow={$ifdef OpenXRUseAndroidUnits}PANativeWindow{$else}TXrPointer{$endif};

     PPXrAndroidAHardwareBuffer=^PXrAndroidAHardwareBuffer;
     PXrAndroidAHardwareBuffer={$ifdef OpenXRUseAndroidUnits}PAHardwareBuffer{$else}TXrPointer{$endif};
{$endif}

{$ifdef Fuchsia}
     PPXrFuchsiaZXHandle=^PXrFuchsiaZXHandle;
     PXrFuchsiaZXHandle=^TXrFuchsiaZXHandle;
     TXrFuchsiaZXHandle={$ifdef OpenXRUseFuchsiaUnits}Tzx_handle_t{$else}TXrSizeUInt{$endif};
{$endif}

{$ifdef DirectFB}
     PPXrDirectFBIDirectFB=^PXrDirectFBIDirectFB;
     PXrDirectFBIDirectFB=^TXrDirectFBIDirectFB;
     TXrDirectFBIDirectFB={$ifdef OpenXRUseDirectFBUnits}IDirectFB{$else}TXrSizeUInt{$endif};

     PPXrDirectFBIDirectFBSurface=^PXrDirectFBIDirectFBSurface;
     PXrDirectFBIDirectFBSurface=^TXrDirectFBIDirectFBSurface;
     TXrDirectFBIDirectFBSurface={$ifdef OpenXRUseDirectFBUnits}IDirectFBSurface{$else}TXrSizeUInt{$endif};
{$endif}

{$ifdef Wayland}
     PPXrWaylandDisplay=^PXrWaylandDisplay;
     PXrWaylandDisplay={$ifdef OpenXRUseWaylandUnits}Pwl_display{$else}TXrPointer{$endif};

     PPXrWaylandSurface=^PXrWaylandSurface;
     PXrWaylandSurface={$ifdef OpenXRUseWaylandUnits}Pwl_surface{$else}TXrPointer{$endif};
{$endif}

{$ifdef XCB}
     PPXrXCBConnection=^PXrXCBConnection;
     PXrXCBConnection={$ifdef OpenXRUseXCBUnits}Pxcb_connection_t{$else}TXrPointer{$endif};

     PPXrXCBVisualID=^PXrXCBVisualID;
     PXrXCBVisualID={$ifdef OpenXRUseXCBUnits}Pxcb_visualid_t{$else}^TXrXCBVisualID{$endif};
     TXrXCBVisualID={$if defined(OpenXRUseXCBUnits)}Pxcb_visualid_t{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};

     PPXrXCBWindow=^PXrXCBWindow;
     PXrXCBWindow={$ifdef OpenXRUseXCBUnits}Pxcb_window_t{$else}^TXrXCBWindow{$endif};
     TXrXCBWindow={$if defined(OpenXRUseXCBUnits)}Txcb_window_t{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};
{$endif}

{$ifdef XLIB}
     PPXrXLIBDisplay=^PXrXLIBDisplay;
     PXrXLIBDisplay={$ifdef OpenXRUseXLIBUnits}PDisplay{$else}TXrPointer{$endif};
     {$ifdef OpenXRUseXLIBUnits}TXrXLIBDisplay=TDisplay;{$endif}

     PPXrXLIBVisualID=^PXrXLIBVisualID;
     PXrXLIBVisualID={$ifdef OpenXRUseXLIBUnits}PVisualID{$else}^TXrXLIBVisualID{$endif};
     TXrXLIBVisualID={$if defined(OpenXRUseXLIBUnits)}TVisualID{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};

     PPXrXLIBWindow=^PXrXLIBWindow;
     PXrXLIBWindow={$ifdef OpenXRUseXLIBUnits}PWindow{$else}^TXrXLIBWindow{$endif};
     TXrXLIBWindow={$if defined(OpenXRUseXLIBUnits)}TWindow{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};
{$endif}

     TXrNonDefinedType=pointer;

     PPXrGgpStreamDescriptor=^PXrGgpStreamDescriptor;
     PXrGgpStreamDescriptor=^TXrGgpStreamDescriptor;
     TXrGgpStreamDescriptor=TXrNonDefinedType;

     PPXrGgpFrameToken=^PXrGgpFrameToken;
     PXrGgpFrameToken=^TXrGgpFrameToken;
     TXrGgpFrameToken=TXrNonDefinedType;

     PPXrCAMetalLayer=^PXrCAMetalLayer;
     PXrCAMetalLayer=^TXrCAMetalLayer;
     TXrCAMetalLayer=TXrNonDefinedType;

const XR_NULL_HANDLE=0;

      XR_NULL_INSTANCE=0;

      XR_TRUE=1;
      XR_FALSE=0;
      XR_MAX_EXTENSION_NAME_SIZE=128;
      XR_MAX_API_LAYER_NAME_SIZE=256;
      XR_MAX_API_LAYER_DESCRIPTION_SIZE=256;
      XR_MAX_SYSTEM_NAME_SIZE=256;
      XR_MAX_APPLICATION_NAME_SIZE=128;
      XR_MAX_ENGINE_NAME_SIZE=128;
      XR_MAX_RUNTIME_NAME_SIZE=128;
      XR_MAX_PATH_LENGTH=256;
      XR_MAX_STRUCTURE_NAME_SIZE=64;
      XR_MAX_RESULT_STRING_SIZE=64;
      XR_MAX_GRAPHICS_APIS_SUPPORTED=32;
      XR_MAX_ACTION_SET_NAME_SIZE=64;
      XR_MAX_ACTION_NAME_SIZE=64;
      XR_MAX_LOCALIZED_ACTION_SET_NAME_SIZE=128;
      XR_MAX_LOCALIZED_ACTION_NAME_SIZE=128;
      XR_MIN_COMPOSITION_LAYERS_SUPPORTED=16;
      XR_KHR_android_thread_settings_SPEC_VERSION=5;
      XR_KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME='XR_KHR_android_thread_settings';
      XR_KHR_android_surface_swapchain_SPEC_VERSION=4;
      XR_KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME='XR_KHR_android_surface_swapchain';
      XR_KHR_composition_layer_cube_SPEC_VERSION=8;
      XR_KHR_COMPOSITION_LAYER_CUBE_EXTENSION_NAME='XR_KHR_composition_layer_cube';
      XR_KHR_android_create_instance_SPEC_VERSION=3;
      XR_KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME='XR_KHR_android_create_instance';
      XR_KHR_composition_layer_depth_SPEC_VERSION=5;
      XR_KHR_COMPOSITION_LAYER_DEPTH_EXTENSION_NAME='XR_KHR_composition_layer_depth';
      XR_KHR_headless_SPEC_VERSION=4;
      XR_KHR_HEADLESS_EXTENSION_NAME='XR_KHR_headless';
      XR_KHR_vulkan_swapchain_format_list_SPEC_VERSION=4;
      XR_KHR_VULKAN_SWAPCHAIN_FORMAT_LIST_EXTENSION_NAME='XR_KHR_vulkan_swapchain_format_list';
      XR_EXT_performance_settings_SPEC_VERSION=3;
      XR_EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME='XR_EXT_performance_settings';
      XR_EXT_thermal_query_SPEC_VERSION=2;
      XR_EXT_THERMAL_QUERY_EXTENSION_NAME='XR_EXT_thermal_query';
      XR_KHR_composition_layer_cylinder_SPEC_VERSION=4;
      XR_KHR_COMPOSITION_LAYER_CYLINDER_EXTENSION_NAME='XR_KHR_composition_layer_cylinder';
      XR_KHR_composition_layer_equirect_SPEC_VERSION=3;
      XR_KHR_COMPOSITION_LAYER_EQUIRECT_EXTENSION_NAME='XR_KHR_composition_layer_equirect';
      XR_EXT_debug_utils_SPEC_VERSION=4;
      XR_EXT_DEBUG_UTILS_EXTENSION_NAME='XR_EXT_debug_utils';
      XR_KHR_variable_rate_rendering_SPEC_VERSION=1;
      XR_KHR_VARIABLE_RATE_RENDERING_EXTENSION_NAME='XR_KHR_variable_rate_rendering';
      XR_KHR_overlays_SPEC_VERSION=0;
      XR_KHR_OVERLAYS_EXTENSION_NAME='XR_KHR_overlays';
      XR_KHR_opengl_enable_SPEC_VERSION=9;
      XR_KHR_OPENGL_ENABLE_EXTENSION_NAME='XR_KHR_opengl_enable';
      XR_KHR_opengl_es_enable_SPEC_VERSION=7;
      XR_KHR_OPENGL_ES_ENABLE_EXTENSION_NAME='XR_KHR_opengl_es_enable';
      XR_KHR_vulkan_enable_SPEC_VERSION=8;
      XR_KHR_VULKAN_ENABLE_EXTENSION_NAME='XR_KHR_vulkan_enable';
      XR_KHR_D3D10_enable_obsolete_SPEC_VERSION=4;
      XR_KHR_D3D10_ENABLE_OBSOLETE_EXTENSION_NAME='XR_KHR_D3D10_enable_obsolete';
      XR_KHR_D3D11_enable_SPEC_VERSION=5;
      XR_KHR_D3D11_ENABLE_EXTENSION_NAME='XR_KHR_D3D11_enable';
      XR_KHR_D3D12_enable_SPEC_VERSION=7;
      XR_KHR_D3D12_ENABLE_EXTENSION_NAME='XR_KHR_D3D12_enable';
      XR_KHR_metal_enable_SPEC_VERSION=1;
      XR_KHR_METAL_ENABLE_EXTENSION_NAME='XR_KHR_metal_enable';
      XR_EXT_eye_gaze_interaction_SPEC_VERSION=1;
      XR_EXT_EYE_GAZE_INTERACTION_EXTENSION_NAME='XR_EXT_eye_gaze_interaction';
      XR_KHR_visibility_mask_SPEC_VERSION=2;
      XR_KHR_VISIBILITY_MASK_EXTENSION_NAME='XR_KHR_visibility_mask';
      XR_EXT_permissions_support_SPEC_VERSION=1;
      XR_EXT_PERMISSIONS_SUPPORT_EXTENSION_NAME='XR_EXT_permissions_support';
      XR_EXTX_overlay_SPEC_VERSION=5;
      XR_EXTX_OVERLAY_EXTENSION_NAME='XR_EXTX_overlay';
      XR_KHR_composition_layer_color_scale_bias_SPEC_VERSION=5;
      XR_KHR_COMPOSITION_LAYER_COLOR_SCALE_BIAS_EXTENSION_NAME='XR_KHR_composition_layer_color_scale_bias';
      XR_KHR_win32_convert_performance_counter_time_SPEC_VERSION=1;
      XR_KHR_WIN32_CONVERT_PERFORMANCE_COUNTER_TIME_EXTENSION_NAME='XR_KHR_win32_convert_performance_counter_time';
      XR_KHR_convert_timespec_time_SPEC_VERSION=1;
      XR_KHR_CONVERT_TIMESPEC_TIME_EXTENSION_NAME='XR_KHR_convert_timespec_time';
      XR_VARJO_quad_views_SPEC_VERSION=1;
      XR_VARJO_QUAD_VIEWS_EXTENSION_NAME='XR_VARJO_quad_views';
      XR_MSFT_unbounded_reference_space_SPEC_VERSION=1;
      XR_MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME='XR_MSFT_unbounded_reference_space';
      XR_MSFT_spatial_anchor_SPEC_VERSION=1;
      XR_MSFT_SPATIAL_ANCHOR_EXTENSION_NAME='XR_MSFT_spatial_anchor';
      XR_OCULUS_extension_41_SPEC_VERSION=1;
      XR_OCULUS_EXTENSION_41_EXTENSION_NAME='XR_OCULUS_extension_41';
      XR_OCULUS_extension_42_SPEC_VERSION=1;
      XR_OCULUS_EXTENSION_42_EXTENSION_NAME='XR_OCULUS_extension_42';
      XR_MND_headless_SPEC_VERSION=2;
      XR_MND_HEADLESS_EXTENSION_NAME='XR_MND_headless';
      XR_OCULUS_extension_44_SPEC_VERSION=1;
      XR_OCULUS_EXTENSION_44_EXTENSION_NAME='XR_OCULUS_extension_44';
      XR_OCULUS_android_session_state_enable_SPEC_VERSION=1;
      XR_OCULUS_ANDROID_SESSION_STATE_ENABLE_EXTENSION_NAME='XR_OCULUS_android_session_state_enable';
      XR_MND_extension_46_SPEC_VERSION=1;
      XR_MND_EXTENSION_46_EXTENSION_NAME='XR_MND_extension_46';
      XR_EXT_view_configuration_depth_range_SPEC_VERSION=1;
      XR_EXT_VIEW_CONFIGURATION_DEPTH_RANGE_EXTENSION_NAME='XR_EXT_view_configuration_depth_range';
      XR_EXT_conformance_automation_SPEC_VERSION=3;
      XR_EXT_CONFORMANCE_AUTOMATION_EXTENSION_NAME='XR_EXT_conformance_automation';
      XR_MNDX_egl_enable_SPEC_VERSION=1;
      XR_MNDX_EGL_ENABLE_EXTENSION_NAME='XR_MNDX_egl_enable';
      XR_MSFT_spatial_graph_bridge_SPEC_VERSION=1;
      XR_MSFT_SPATIAL_GRAPH_BRIDGE_EXTENSION_NAME='XR_MSFT_spatial_graph_bridge';
      XR_MSFT_hand_interaction_SPEC_VERSION=1;
      XR_MSFT_HAND_INTERACTION_EXTENSION_NAME='XR_MSFT_hand_interaction';
      XR_EXT_hand_tracking_SPEC_VERSION=4;
      XR_EXT_HAND_TRACKING_EXTENSION_NAME='XR_EXT_hand_tracking';
      XR_MSFT_hand_tracking_mesh_SPEC_VERSION=3;
      XR_MSFT_HAND_TRACKING_MESH_EXTENSION_NAME='XR_MSFT_hand_tracking_mesh';
      XR_MSFT_secondary_view_configuration_SPEC_VERSION=1;
      XR_MSFT_SECONDARY_VIEW_CONFIGURATION_EXTENSION_NAME='XR_MSFT_secondary_view_configuration';
      XR_MSFT_first_person_observer_SPEC_VERSION=1;
      XR_MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME='XR_MSFT_first_person_observer';
      XR_MSFT_controller_model_SPEC_VERSION=2;
      XR_MSFT_CONTROLLER_MODEL_EXTENSION_NAME='XR_MSFT_controller_model';
      XR_MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT=64;
      XR_MSFT_perception_anchor_interop_SPEC_VERSION=1;
      XR_MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME='XR_MSFT_perception_anchor_interop';
      XR_EXT_win32_appcontainer_compatible_SPEC_VERSION=1;
      XR_EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME='XR_EXT_win32_appcontainer_compatible';
      XR_ML_extension_59_SPEC_VERSION=1;
      XR_ML_extension_59_EXTENSION_NAME='XR_ML_extension_59';
      XR_EPIC_view_configuration_fov_SPEC_VERSION=2;
      XR_EPIC_VIEW_CONFIGURATION_FOV_EXTENSION_NAME='XR_EPIC_view_configuration_fov';
      XR_MSFT_extension_63_SPEC_VERSION=1;
      XR_MSFT_extension_63_EXTENSION_NAME='XR_MSFT_extension_63';
      XR_MSFT_holographic_window_attachment_SPEC_VERSION=1;
      XR_MSFT_HOLOGRAPHIC_WINDOW_ATTACHMENT_EXTENSION_NAME='XR_MSFT_holographic_window_attachment';
      XR_MSFT_extension_65_SPEC_VERSION=1;
      XR_MSFT_extension_65_EXTENSION_NAME='XR_MSFT_extension_65';
      XR_MSFT_extension_66_SPEC_VERSION=1;
      XR_MSFT_extension_66_EXTENSION_NAME='XR_MSFT_extension_66';
      XR_MSFT_composition_layer_reprojection_SPEC_VERSION=1;
      XR_MSFT_COMPOSITION_LAYER_REPROJECTION_EXTENSION_NAME='XR_MSFT_composition_layer_reprojection';
      XR_LUNARG_extension_68_SPEC_VERSION=1;
      XR_LUNARG_extension_68_EXTENSION_NAME='XR_LUNARG_extension_68';
      XR_LUNARG_extension_69_SPEC_VERSION=1;
      XR_LUNARG_extension_69_EXTENSION_NAME='XR_LUNARG_extension_69';
      XR_HUAWEI_controller_interaction_SPEC_VERSION=1;
      XR_HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME='XR_HUAWEI_controller_interaction';
      XR_FB_android_surface_swapchain_create_SPEC_VERSION=1;
      XR_FB_ANDROID_SURFACE_SWAPCHAIN_CREATE_EXTENSION_NAME='XR_FB_android_surface_swapchain_create';
      XR_FB_swapchain_update_state_SPEC_VERSION=3;
      XR_FB_SWAPCHAIN_UPDATE_STATE_EXTENSION_NAME='XR_FB_swapchain_update_state';
      XR_FB_extension_73_SPEC_VERSION=1;
      XR_FB_extension_73_EXTENSION_NAME='XR_FB_extension_73';
      XR_FB_extension_74_SPEC_VERSION=1;
      XR_FB_extension_74_EXTENSION_NAME='XR_FB_extension_74';
      XR_FB_extension_75_SPEC_VERSION=1;
      XR_FB_extension_75_EXTENSION_NAME='XR_FB_extension_75';
      XR_FB_extension_76_SPEC_VERSION=1;
      XR_FB_extension_76_EXTENSION_NAME='XR_FB_extension_76';
      XR_FB_extension_77_SPEC_VERSION=1;
      XR_FB_extension_77_EXTENSION_NAME='XR_FB_extension_77';
      XR_FB_extension_78_SPEC_VERSION=1;
      XR_FB_extension_78_EXTENSION_NAME='XR_FB_extension_78';
      XR_VALVE_extension_79_SPEC_VERSION=1;
      XR_VALVE_extension_79_EXTENSION_NAME='XR_VALVE_extension_79';
      XR_VALVE_analog_threshold_SPEC_VERSION=1;
      XR_VALVE_ANALOG_THRESHOLD_EXTENSION_NAME='XR_VALVE_analog_threshold';
      XR_EXT_hand_joints_motion_range_SPEC_VERSION=1;
      XR_EXT_HAND_JOINTS_MOTION_RANGE_EXTENSION_NAME='XR_EXT_hand_joints_motion_range';
      XR_VALVE_extension_82_SPEC_VERSION=1;
      XR_VALVE_extension_82_EXTENSION_NAME='XR_VALVE_extension_82';
      XR_VALVE_extension_83_SPEC_VERSION=1;
      XR_VALVE_extension_83_EXTENSION_NAME='XR_VALVE_extension_83';
      XR_VALVE_extension_84_SPEC_VERSION=1;
      XR_VALVE_extension_84_EXTENSION_NAME='XR_VALVE_extension_84';
      XR_VALVE_extension_85_SPEC_VERSION=1;
      XR_VALVE_extension_85_EXTENSION_NAME='XR_VALVE_extension_85';
      XR_VALVE_extension_86_SPEC_VERSION=1;
      XR_VALVE_extension_86_EXTENSION_NAME='XR_VALVE_extension_86';
      XR_VALVE_extension_87_SPEC_VERSION=1;
      XR_VALVE_extension_87_EXTENSION_NAME='XR_VALVE_extension_87';
      XR_VALVE_extension_88_SPEC_VERSION=1;
      XR_VALVE_extension_88_EXTENSION_NAME='XR_VALVE_extension_88';
      XR_KHR_loader_init_SPEC_VERSION=1;
      XR_KHR_LOADER_INIT_EXTENSION_NAME='XR_KHR_loader_init';
      XR_KHR_loader_init_android_SPEC_VERSION=1;
      XR_KHR_LOADER_INIT_ANDROID_EXTENSION_NAME='XR_KHR_loader_init_android';
      XR_KHR_vulkan_enable2_SPEC_VERSION=2;
      XR_KHR_VULKAN_ENABLE2_EXTENSION_NAME='XR_KHR_vulkan_enable2';
      XR_KHR_composition_layer_equirect2_SPEC_VERSION=1;
      XR_KHR_COMPOSITION_LAYER_EQUIRECT2_EXTENSION_NAME='XR_KHR_composition_layer_equirect2';
      XR_EXT_extension_93_SPEC_VERSION=1;
      XR_EXT_extension_93_EXTENSION_NAME='XR_EXT_extension_93';
      XR_EXT_extension_94_SPEC_VERSION=1;
      XR_EXT_extension_94_EXTENSION_NAME='XR_EXT_extension_94';
      XR_EXT_samsung_odyssey_controller_SPEC_VERSION=1;
      XR_EXT_SAMSUNG_ODYSSEY_CONTROLLER_EXTENSION_NAME='XR_EXT_samsung_odyssey_controller';
      XR_EXT_hp_mixed_reality_controller_SPEC_VERSION=1;
      XR_EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME='XR_EXT_hp_mixed_reality_controller';
      XR_MND_swapchain_usage_input_attachment_bit_SPEC_VERSION=2;
      XR_MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME='XR_MND_swapchain_usage_input_attachment_bit';
      XR_MSFT_scene_understanding_SPEC_VERSION=1;
      XR_MSFT_SCENE_UNDERSTANDING_EXTENSION_NAME='XR_MSFT_scene_understanding';
      XR_MSFT_scene_understanding_serialization_SPEC_VERSION=1;
      XR_MSFT_SCENE_UNDERSTANDING_SERIALIZATION_EXTENSION_NAME='XR_MSFT_scene_understanding_serialization';
      XR_MSFT_extension_100_SPEC_VERSION=1;
      XR_MSFT_extension_100_EXTENSION_NAME='XR_MSFT_extension_100';
      XR_MSFT_extension_101_SPEC_VERSION=1;
      XR_MSFT_extension_101_EXTENSION_NAME='XR_MSFT_extension_101';
      XR_FB_display_refresh_rate_SPEC_VERSION=1;
      XR_FB_DISPLAY_REFRESH_RATE_EXTENSION_NAME='XR_FB_display_refresh_rate';
      XR_HTC_vive_cosmos_controller_interaction_SPEC_VERSION=1;
      XR_HTC_VIVE_COSMOS_CONTROLLER_INTERACTION_EXTENSION_NAME='XR_HTC_vive_cosmos_controller_interaction';
      XR_HTC_extension_104_SPEC_VERSION=1;
      XR_HTC_extension_104_EXTENSION_NAME='XR_HTC_extension_104';
      XR_HTC_extension_105_SPEC_VERSION=1;
      XR_HTC_extension_105_EXTENSION_NAME='XR_HTC_extension_105';
      XR_HTC_extension_106_SPEC_VERSION=1;
      XR_HTC_extension_106_EXTENSION_NAME='XR_HTC_extension_106';
      XR_HTC_extension_107_SPEC_VERSION=1;
      XR_HTC_extension_107_EXTENSION_NAME='XR_HTC_extension_107';
      XR_HTC_extension_108_SPEC_VERSION=1;
      XR_HTC_extension_108_EXTENSION_NAME='XR_HTC_extension_108';
      XR_FB_color_space_SPEC_VERSION=1;
      XR_FB_COLOR_SPACE_EXTENSION_NAME='XR_FB_color_space';
      XR_FB_extension_110_SPEC_VERSION=1;
      XR_FB_extension_110_EXTENSION_NAME='XR_FB_extension_110';
      XR_FB_extension_111_SPEC_VERSION=1;
      XR_FB_extension_111_EXTENSION_NAME='XR_FB_extension_111';
      XR_FB_extension_112_SPEC_VERSION=1;
      XR_FB_extension_112_EXTENSION_NAME='XR_FB_extension_112';
      XR_FB_extension_113_SPEC_VERSION=1;
      XR_FB_extension_113_EXTENSION_NAME='XR_FB_extension_113';
      XR_FB_extension_114_SPEC_VERSION=1;
      XR_FB_extension_114_EXTENSION_NAME='XR_FB_extension_114';
      XR_FB_extension_115_SPEC_VERSION=1;
      XR_FB_extension_115_EXTENSION_NAME='XR_FB_extension_115';
      XR_FB_extension_116_SPEC_VERSION=1;
      XR_FB_extension_116_EXTENSION_NAME='XR_FB_extension_116';
      XR_FB_extension_117_SPEC_VERSION=1;
      XR_FB_extension_117_EXTENSION_NAME='XR_FB_extension_117';
      XR_FB_extension_118_SPEC_VERSION=1;
      XR_FB_extension_118_EXTENSION_NAME='XR_FB_extension_118';
      XR_FB_extension_119_SPEC_VERSION=1;
      XR_FB_extension_119_EXTENSION_NAME='XR_FB_extension_119';
      XR_FB_extension_120_SPEC_VERSION=1;
      XR_FB_extension_120_EXTENSION_NAME='XR_FB_extension_120';
      XR_KHR_binding_modification_SPEC_VERSION=1;
      XR_KHR_BINDING_MODIFICATION_EXTENSION_NAME='XR_KHR_binding_modification';
      XR_VARJO_foveated_rendering_SPEC_VERSION=1;
      XR_VARJO_FOVEATED_RENDERING_EXTENSION_NAME='XR_VARJO_foveated_rendering';
      XR_VARJO_composition_layer_depth_test_SPEC_VERSION=1;
      XR_VARJO_COMPOSITION_LAYER_DEPTH_TEST_EXTENSION_NAME='XR_VARJO_composition_layer_depth_test';
      XR_VARJO_environment_depth_estimation_SPEC_VERSION=1;
      XR_VARJO_ENVIRONMENT_DEPTH_ESTIMATION_EXTENSION_NAME='XR_VARJO_environment_depth_estimation';
      XR_VARJO_extension_125_SPEC_VERSION=1;
      XR_VARJO_extension_125_EXTENSION_NAME='XR_VARJO_extension_125';
      XR_VARJO_extension_126_SPEC_VERSION=1;
      XR_VARJO_extension_126_EXTENSION_NAME='XR_VARJO_extension_126';
      XR_VARJO_extension_127_SPEC_VERSION=1;
      XR_VARJO_extension_127_EXTENSION_NAME='XR_VARJO_extension_127';
      XR_VARJO_extension_128_SPEC_VERSION=1;
      XR_VARJO_extension_128_EXTENSION_NAME='XR_VARJO_extension_128';
      XR_VARJO_extension_129_SPEC_VERSION=1;
      XR_VARJO_extension_129_EXTENSION_NAME='XR_VARJO_extension_129';
      XR_VARJO_extension_130_SPEC_VERSION=1;
      XR_VARJO_extension_130_EXTENSION_NAME='XR_VARJO_extension_130';
      XR_VARJO_extension_131_SPEC_VERSION=1;
      XR_VARJO_extension_131_EXTENSION_NAME='XR_VARJO_extension_131';
      XR_VARJO_extension_132_SPEC_VERSION=1;
      XR_VARJO_extension_132_EXTENSION_NAME='XR_VARJO_extension_132';
      XR_ML_extension_133_SPEC_VERSION=1;
      XR_ML_extension_133_EXTENSION_NAME='XR_ML_extension_133';
      XR_ML_extension_134_SPEC_VERSION=1;
      XR_ML_extension_134_EXTENSION_NAME='XR_ML_extension_134';
      XR_ML_extension_135_SPEC_VERSION=1;
      XR_ML_extension_135_EXTENSION_NAME='XR_ML_extension_135';
      XR_ML_extension_136_SPEC_VERSION=1;
      XR_ML_extension_136_EXTENSION_NAME='XR_ML_extension_136';
      XR_ML_extension_137_SPEC_VERSION=1;
      XR_ML_extension_137_EXTENSION_NAME='XR_ML_extension_137';
      XR_ML_extension_138_SPEC_VERSION=1;
      XR_ML_extension_138_EXTENSION_NAME='XR_ML_extension_138';
      XR_ML_extension_139_SPEC_VERSION=1;
      XR_ML_extension_139_EXTENSION_NAME='XR_ML_extension_139';
      XR_ML_extension_140_SPEC_VERSION=1;
      XR_ML_extension_140_EXTENSION_NAME='XR_ML_extension_140';
      XR_ML_extension_141_SPEC_VERSION=1;
      XR_ML_extension_141_EXTENSION_NAME='XR_ML_extension_141';
      XR_ML_extension_142_SPEC_VERSION=1;
      XR_ML_extension_142_EXTENSION_NAME='XR_ML_extension_142';
      XR_MSFT_extension_143_SPEC_VERSION=1;
      XR_MSFT_extension_143_EXTENSION_NAME='XR_MSFT_extension_143';
      XR_MSFT_extension_144_SPEC_VERSION=1;
      XR_MSFT_extension_144_EXTENSION_NAME='XR_MSFT_extension_144';
      XR_MSFT_extension_145_SPEC_VERSION=1;
      XR_MSFT_extension_145_EXTENSION_NAME='XR_MSFT_extension_145';
      XR_MSFT_extension_146_SPEC_VERSION=1;
      XR_MSFT_extension_146_EXTENSION_NAME='XR_MSFT_extension_146';
      XR_MSFT_extension_147_SPEC_VERSION=1;
      XR_MSFT_extension_147_EXTENSION_NAME='XR_MSFT_extension_147';
      XR_MSFT_extension_148_SPEC_VERSION=1;
      XR_MSFT_extension_148_EXTENSION_NAME='XR_MSFT_extension_148';
      XR_KHR_extension_149_SPEC_VERSION=1;
      XR_KHR_extension_149_EXTENSION_NAME='XR_KHR_extension_149';
      XR_ULTRALEAP_extension_150_SPEC_VERSION=1;
      XR_ULTRALEAP_extension_150_EXTENSION_NAME='XR_ULTRALEAP_extension_150';
      XR_ULTRALEAP_extension_151_SPEC_VERSION=1;
      XR_ULTRALEAP_extension_151_EXTENSION_NAME='XR_ULTRALEAP_extension_151';
      XR_ULTRALEAP_extension_152_SPEC_VERSION=1;
      XR_ULTRALEAP_extension_152_EXTENSION_NAME='XR_ULTRALEAP_extension_152';
      XR_ULTRALEAP_extension_153_SPEC_VERSION=1;
      XR_ULTRALEAP_extension_153_EXTENSION_NAME='XR_ULTRALEAP_extension_153';
      XR_ULTRALEAP_extension_154_SPEC_VERSION=1;
      XR_ULTRALEAP_extension_154_EXTENSION_NAME='XR_ULTRALEAP_extension_154';
      XR_ULTRALEAP_extension_155_SPEC_VERSION=1;
      XR_ULTRALEAP_extension_155_EXTENSION_NAME='XR_ULTRALEAP_extension_155';
      XR_FB_extension_156_SPEC_VERSION=1;
      XR_FB_extension_156_EXTENSION_NAME='XR_FB_extension_156';
      XR_FB_extension_157_SPEC_VERSION=1;
      XR_FB_extension_157_EXTENSION_NAME='XR_FB_extension_157';
      XR_FB_extension_158_SPEC_VERSION=1;
      XR_FB_extension_158_EXTENSION_NAME='XR_FB_extension_158';
      XR_FB_extension_159_SPEC_VERSION=1;
      XR_FB_extension_159_EXTENSION_NAME='XR_FB_extension_159';
      XR_OCULUS_audio_device_guid_SPEC_VERSION=1;
      XR_OCULUS_AUDIO_DEVICE_GUID_EXTENSION_NAME='XR_OCULUS_audio_device_guid';
      XR_MAX_AUDIO_DEVICE_STR_SIZE_OCULUS=128;
      XR_FB_extension_161_SPEC_VERSION=1;
      XR_FB_extension_161_EXTENSION_NAME='XR_FB_extension_161';
      XR_FB_swapchain_update_state_android_surface_SPEC_VERSION=1;
      XR_FB_SWAPCHAIN_UPDATE_STATE_ANDROID_SURFACE_EXTENSION_NAME='XR_FB_swapchain_update_state_android_surface';
      XR_FB_swapchain_update_state_opengl_es_SPEC_VERSION=1;
      XR_FB_SWAPCHAIN_UPDATE_STATE_OPENGL_ES_EXTENSION_NAME='XR_FB_swapchain_update_state_opengl_es';
      XR_FB_swapchain_update_state_vulkan_SPEC_VERSION=1;
      XR_FB_SWAPCHAIN_UPDATE_STATE_VULKAN_EXTENSION_NAME='XR_FB_swapchain_update_state_vulkan';
      XR_FB_extension_165_SPEC_VERSION=1;
      XR_FB_extension_165_EXTENSION_NAME='XR_FB_extension_165';
      XR_KHR_extension_166_SPEC_VERSION=1;
      XR_KHR_extension_166_EXTENSION_NAME='XR_KHR_extension_166';
      XR_FB_extension_167_SPEC_VERSION=1;
      XR_FB_extension_167_EXTENSION_NAME='XR_FB_extension_167';
      XR_FB_extension_168_SPEC_VERSION=1;
      XR_FB_extension_168_EXTENSION_NAME='XR_FB_extension_168';
      XR_FB_extension_169_SPEC_VERSION=1;
      XR_FB_extension_169_EXTENSION_NAME='XR_FB_extension_169';
      XR_FB_extension_170_SPEC_VERSION=1;
      XR_FB_extension_170_EXTENSION_NAME='XR_FB_extension_170';
      XR_FB_extension_171_SPEC_VERSION=1;
      XR_FB_extension_171_EXTENSION_NAME='XR_FB_extension_171';
      XR_FB_extension_172_SPEC_VERSION=1;
      XR_FB_extension_172_EXTENSION_NAME='XR_FB_extension_172';
      XR_FB_extension_173_SPEC_VERSION=1;
      XR_FB_extension_173_EXTENSION_NAME='XR_FB_extension_173';
      XR_FB_extension_174_SPEC_VERSION=1;
      XR_FB_extension_174_EXTENSION_NAME='XR_FB_extension_174';
      XR_FB_extension_175_SPEC_VERSION=1;
      XR_FB_extension_175_EXTENSION_NAME='XR_FB_extension_175';
      XR_FB_extension_176_SPEC_VERSION=1;
      XR_FB_extension_176_EXTENSION_NAME='XR_FB_extension_176';

type PPXrDispatchableHandle=^PXrDispatchableHandle;
     PXrDispatchableHandle=^TXrDispatchableHandle;
     TXrDispatchableHandle=TXrPtrInt;

     PPXrNonDispatchableHandle=^PXrNonDispatchableHandle;
     PXrNonDispatchableHandle=^TXrNonDispatchableHandle;
     TXrNonDispatchableHandle=TXrUInt64;

     PPXrAtom=^PXrAtom;
     PXrAtom=^TXrAtom;
     TXrAtom=TXrUInt64;

     PPXrHDC=^PXrHDC;
     PXrHDC=^TXrHDC;
     TXrHDC=HDC;

     PPXrHGLRC=^PXrHGLRC;
     PXrHGLRC=^TXrHGLRC;
     TXrHGLRC=HGLRC;

     PPXrEnum=^PXrEnum;
     PXrEnum=^TXrEnum;
     TXrEnum=TXrInt32;

     TPFN_vkGetInstanceProcAddr=TvkGetInstanceProcAddr;

{$ifdef Windows}
     PPXrHINSTANCE=^PXrHINSTANCE;
     PXrHINSTANCE=^TXrHINSTANCE;
     TXrHINSTANCE=TXrPtrUInt;

     PPXrHWND=^PXrHWND;
     PXrHWND=^TXrHWND;
     TXrHWND=HWND;

     PPXrHMONITOR=^PXrHMONITOR;
     PXrHMONITOR=^TXrHMONITOR;
     TXrHMONITOR=HMONITOR;
{$endif}

     PPXrBool32=^PXrBool32;
     PXrBool32=^TXrBool32;
     TXrBool32=TXrUInt32;

     PPXrFlags64=^PXrFlags64;
     PXrFlags64=^TXrFlags64;
     TXrFlags64=TXrUInt64;

     PPXrTime=^PXrTime;
     PXrTime=^TXrTime;
     TXrTime=TXrInt64;

     PPXrDuration=^PXrDuration;
     PXrDuration=^TXrDuration;
     TXrDuration=TXrInt64;

     PPXrVersion=^PXrVersion;
     PXrVersion=^TXrVersion;
     TXrVersion=TXrUInt64;

     PPXrPath=^PXrPath;
     PXrPath=^TXrPath;
     TXrPath=TXrAtom;

     PPXrSystemId=^PXrSystemId;
     PXrSystemId=^TXrSystemId;
     TXrSystemId=TXrAtom;

     PPXrControllerModelKeyMSFT=^PXrControllerModelKeyMSFT;
     PXrControllerModelKeyMSFT=^TXrControllerModelKeyMSFT;
     TXrControllerModelKeyMSFT=TXrAtom;

     PPXrInstanceCreateFlags=^PXrInstanceCreateFlags;
     PXrInstanceCreateFlags=^TXrInstanceCreateFlags;
     TXrInstanceCreateFlags=TXrFlags64;

     PPXrSessionCreateFlags=^PXrSessionCreateFlags;
     PXrSessionCreateFlags=^TXrSessionCreateFlags;
     TXrSessionCreateFlags=TXrFlags64;

     PPXrSwapchainCreateFlags=^PXrSwapchainCreateFlags;
     PXrSwapchainCreateFlags=^TXrSwapchainCreateFlags;
     TXrSwapchainCreateFlags=TXrFlags64;

     PPXrSwapchainUsageFlags=^PXrSwapchainUsageFlags;
     PXrSwapchainUsageFlags=^TXrSwapchainUsageFlags;
     TXrSwapchainUsageFlags=TXrFlags64;

     PPXrViewStateFlags=^PXrViewStateFlags;
     PXrViewStateFlags=^TXrViewStateFlags;
     TXrViewStateFlags=TXrFlags64;

     PPXrCompositionLayerFlags=^PXrCompositionLayerFlags;
     PXrCompositionLayerFlags=^TXrCompositionLayerFlags;
     TXrCompositionLayerFlags=TXrFlags64;

     PPXrSpaceLocationFlags=^PXrSpaceLocationFlags;
     PXrSpaceLocationFlags=^TXrSpaceLocationFlags;
     TXrSpaceLocationFlags=TXrFlags64;

     PPXrSpaceVelocityFlags=^PXrSpaceVelocityFlags;
     PXrSpaceVelocityFlags=^TXrSpaceVelocityFlags;
     TXrSpaceVelocityFlags=TXrFlags64;

     PPXrInputSourceLocalizedNameFlags=^PXrInputSourceLocalizedNameFlags;
     PXrInputSourceLocalizedNameFlags=^TXrInputSourceLocalizedNameFlags;
     TXrInputSourceLocalizedNameFlags=TXrFlags64;

     PPXrVulkanInstanceCreateFlagsKHR=^PXrVulkanInstanceCreateFlagsKHR;
     PXrVulkanInstanceCreateFlagsKHR=^TXrVulkanInstanceCreateFlagsKHR;
     TXrVulkanInstanceCreateFlagsKHR=TXrFlags64;

     PPXrVulkanDeviceCreateFlagsKHR=^PXrVulkanDeviceCreateFlagsKHR;
     PXrVulkanDeviceCreateFlagsKHR=^TXrVulkanDeviceCreateFlagsKHR;
     TXrVulkanDeviceCreateFlagsKHR=TXrFlags64;

     PPXrDebugUtilsMessageSeverityFlagsEXT=^PXrDebugUtilsMessageSeverityFlagsEXT;
     PXrDebugUtilsMessageSeverityFlagsEXT=^TXrDebugUtilsMessageSeverityFlagsEXT;
     TXrDebugUtilsMessageSeverityFlagsEXT=TXrFlags64;

     PPXrDebugUtilsMessageTypeFlagsEXT=^PXrDebugUtilsMessageTypeFlagsEXT;
     PXrDebugUtilsMessageTypeFlagsEXT=^TXrDebugUtilsMessageTypeFlagsEXT;
     TXrDebugUtilsMessageTypeFlagsEXT=TXrFlags64;

     PPXrOverlayMainSessionFlagsEXTX=^PXrOverlayMainSessionFlagsEXTX;
     PXrOverlayMainSessionFlagsEXTX=^TXrOverlayMainSessionFlagsEXTX;
     TXrOverlayMainSessionFlagsEXTX=TXrFlags64;

     PPXrOverlaySessionCreateFlagsEXTX=^PXrOverlaySessionCreateFlagsEXTX;
     PXrOverlaySessionCreateFlagsEXTX=^TXrOverlaySessionCreateFlagsEXTX;
     TXrOverlaySessionCreateFlagsEXTX=TXrFlags64;

     PPXrAndroidSurfaceSwapchainFlagsFB=^PXrAndroidSurfaceSwapchainFlagsFB;
     PXrAndroidSurfaceSwapchainFlagsFB=^TXrAndroidSurfaceSwapchainFlagsFB;
     TXrAndroidSurfaceSwapchainFlagsFB=TXrFlags64;

     PPXrInstance=^PXrInstance;
     PXrInstance=^TXrInstance;
     TXrInstance=TXrDispatchableHandle;

     PPXrSession=^PXrSession;
     PXrSession=^TXrSession;
     TXrSession=TXrDispatchableHandle;

     PPXrActionSet=^PXrActionSet;
     PXrActionSet=^TXrActionSet;
     TXrActionSet=TXrDispatchableHandle;

     PPXrAction=^PXrAction;
     PXrAction=^TXrAction;
     TXrAction=TXrDispatchableHandle;

     PPXrSwapchain=^PXrSwapchain;
     PXrSwapchain=^TXrSwapchain;
     TXrSwapchain=TXrDispatchableHandle;

     PPXrSpace=^PXrSpace;
     PXrSpace=^TXrSpace;
     TXrSpace=TXrDispatchableHandle;

     PPXrDebugUtilsMessengerEXT=^PXrDebugUtilsMessengerEXT;
     PXrDebugUtilsMessengerEXT=^TXrDebugUtilsMessengerEXT;
     TXrDebugUtilsMessengerEXT=TXrDispatchableHandle;

     PPXrSpatialAnchorMSFT=^PXrSpatialAnchorMSFT;
     PXrSpatialAnchorMSFT=^TXrSpatialAnchorMSFT;
     TXrSpatialAnchorMSFT=TXrDispatchableHandle;

     PPXrHandTrackerEXT=^PXrHandTrackerEXT;
     PXrHandTrackerEXT=^TXrHandTrackerEXT;
     TXrHandTrackerEXT=TXrDispatchableHandle;

     PPXrSceneObserverMSFT=^PXrSceneObserverMSFT;
     PXrSceneObserverMSFT=^TXrSceneObserverMSFT;
     TXrSceneObserverMSFT=TXrDispatchableHandle;

     PPXrSceneMSFT=^PXrSceneMSFT;
     PXrSceneMSFT=^TXrSceneMSFT;
     TXrSceneMSFT=TXrDispatchableHandle;

     PPXrStructureType=^PXrStructureType;
     PXrStructureType=^TXrStructureType;
     TXrStructureType=
      (
       XR_TYPE_UNKNOWN=0,
       XR_TYPE_API_LAYER_PROPERTIES=1,
       XR_TYPE_EXTENSION_PROPERTIES=2,
       XR_TYPE_INSTANCE_CREATE_INFO=3,
       XR_TYPE_SYSTEM_GET_INFO=4,
       XR_TYPE_SYSTEM_PROPERTIES=5,
       XR_TYPE_VIEW_LOCATE_INFO=6,
       XR_TYPE_VIEW=7,
       XR_TYPE_SESSION_CREATE_INFO=8,
       XR_TYPE_SWAPCHAIN_CREATE_INFO=9,
       XR_TYPE_SESSION_BEGIN_INFO=10,
       XR_TYPE_VIEW_STATE=11,
       XR_TYPE_FRAME_END_INFO=12,
       XR_TYPE_HAPTIC_VIBRATION=13,
       XR_TYPE_EVENT_DATA_BUFFER=16,
       XR_TYPE_EVENT_DATA_INSTANCE_LOSS_PENDING=17,
       XR_TYPE_EVENT_DATA_SESSION_STATE_CHANGED=18,
       XR_TYPE_ACTION_STATE_BOOLEAN=23,
       XR_TYPE_ACTION_STATE_FLOAT=24,
       XR_TYPE_ACTION_STATE_VECTOR2F=25,
       XR_TYPE_ACTION_STATE_POSE=27,
       XR_TYPE_ACTION_SET_CREATE_INFO=28,
       XR_TYPE_ACTION_CREATE_INFO=29,
       XR_TYPE_INSTANCE_PROPERTIES=32,
       XR_TYPE_FRAME_WAIT_INFO=33,
       XR_TYPE_COMPOSITION_LAYER_PROJECTION=35,
       XR_TYPE_COMPOSITION_LAYER_QUAD=36,
       XR_TYPE_REFERENCE_SPACE_CREATE_INFO=37,
       XR_TYPE_ACTION_SPACE_CREATE_INFO=38,
       XR_TYPE_EVENT_DATA_REFERENCE_SPACE_CHANGE_PENDING=40,
       XR_TYPE_VIEW_CONFIGURATION_VIEW=41,
       XR_TYPE_SPACE_LOCATION=42,
       XR_TYPE_SPACE_VELOCITY=43,
       XR_TYPE_FRAME_STATE=44,
       XR_TYPE_VIEW_CONFIGURATION_PROPERTIES=45,
       XR_TYPE_FRAME_BEGIN_INFO=46,
       XR_TYPE_COMPOSITION_LAYER_PROJECTION_VIEW=48,
       XR_TYPE_EVENT_DATA_EVENTS_LOST=49,
       XR_TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING=51,
       XR_TYPE_EVENT_DATA_INTERACTION_PROFILE_CHANGED=52,
       XR_TYPE_INTERACTION_PROFILE_STATE=53,
       XR_TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO=55,
       XR_TYPE_SWAPCHAIN_IMAGE_WAIT_INFO=56,
       XR_TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO=57,
       XR_TYPE_ACTION_STATE_GET_INFO=58,
       XR_TYPE_HAPTIC_ACTION_INFO=59,
       XR_TYPE_SESSION_ACTION_SETS_ATTACH_INFO=60,
       XR_TYPE_ACTIONS_SYNC_INFO=61,
       XR_TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO=62,
       XR_TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO=63,
       XR_TYPE_COMPOSITION_LAYER_CUBE_KHR=1000006000,
       XR_TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR=1000008000,
       XR_TYPE_COMPOSITION_LAYER_DEPTH_INFO_KHR=1000010000,
       XR_TYPE_VULKAN_SWAPCHAIN_FORMAT_LIST_CREATE_INFO_KHR=1000014000,
       XR_TYPE_EVENT_DATA_PERF_SETTINGS_EXT=1000015000,
       XR_TYPE_COMPOSITION_LAYER_CYLINDER_KHR=1000017000,
       XR_TYPE_COMPOSITION_LAYER_EQUIRECT_KHR=1000018000,
       XR_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT=1000019000,
       XR_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT=1000019001,
       XR_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT=1000019002,
       XR_TYPE_DEBUG_UTILS_LABEL_EXT=1000019003,
       XR_TYPE_GRAPHICS_BINDING_OPENGL_WIN32_KHR=1000023000,
       XR_TYPE_GRAPHICS_BINDING_OPENGL_XLIB_KHR=1000023001,
       XR_TYPE_GRAPHICS_BINDING_OPENGL_XCB_KHR=1000023002,
       XR_TYPE_GRAPHICS_BINDING_OPENGL_WAYLAND_KHR=1000023003,
       XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_KHR=1000023004,
       XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_KHR=1000023005,
       XR_TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR=1000024001,
       XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR=1000024002,
       XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR=1000024003,
       XR_TYPE_GRAPHICS_BINDING_VULKAN_KHR=1000025000,
       XR_TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR=1000025001,
       XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR=1000025002,
       XR_TYPE_GRAPHICS_BINDING_D3D11_KHR=1000027000,
       XR_TYPE_SWAPCHAIN_IMAGE_D3D11_KHR=1000027001,
       XR_TYPE_GRAPHICS_REQUIREMENTS_D3D11_KHR=1000027002,
       XR_TYPE_GRAPHICS_BINDING_D3D12_KHR=1000028000,
       XR_TYPE_SWAPCHAIN_IMAGE_D3D12_KHR=1000028001,
       XR_TYPE_GRAPHICS_REQUIREMENTS_D3D12_KHR=1000028002,
       XR_TYPE_SYSTEM_EYE_GAZE_INTERACTION_PROPERTIES_EXT=1000030000,
       XR_TYPE_EYE_GAZE_SAMPLE_TIME_EXT=1000030001,
       XR_TYPE_VISIBILITY_MASK_KHR=1000031000,
       XR_TYPE_EVENT_DATA_VISIBILITY_MASK_CHANGED_KHR=1000031001,
       XR_TYPE_SESSION_CREATE_INFO_OVERLAY_EXTX=1000033000,
       XR_TYPE_EVENT_DATA_MAIN_SESSION_VISIBILITY_CHANGED_EXTX=1000033003,
       XR_TYPE_COMPOSITION_LAYER_COLOR_SCALE_BIAS_KHR=1000034000,
       XR_TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT=1000039000,
       XR_TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT=1000039001,
       XR_TYPE_VIEW_CONFIGURATION_DEPTH_RANGE_EXT=1000046000,
       XR_TYPE_GRAPHICS_BINDING_EGL_MNDX=1000048004,
       XR_TYPE_SPATIAL_GRAPH_NODE_SPACE_CREATE_INFO_MSFT=1000049000,
       XR_TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT=1000051000,
       XR_TYPE_HAND_TRACKER_CREATE_INFO_EXT=1000051001,
       XR_TYPE_HAND_JOINTS_LOCATE_INFO_EXT=1000051002,
       XR_TYPE_HAND_JOINT_LOCATIONS_EXT=1000051003,
       XR_TYPE_HAND_JOINT_VELOCITIES_EXT=1000051004,
       XR_TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT=1000052000,
       XR_TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT=1000052001,
       XR_TYPE_HAND_MESH_UPDATE_INFO_MSFT=1000052002,
       XR_TYPE_HAND_MESH_MSFT=1000052003,
       XR_TYPE_HAND_POSE_TYPE_INFO_MSFT=1000052004,
       XR_TYPE_SECONDARY_VIEW_CONFIGURATION_SESSION_BEGIN_INFO_MSFT=1000053000,
       XR_TYPE_SECONDARY_VIEW_CONFIGURATION_STATE_MSFT=1000053001,
       XR_TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_STATE_MSFT=1000053002,
       XR_TYPE_SECONDARY_VIEW_CONFIGURATION_FRAME_END_INFO_MSFT=1000053003,
       XR_TYPE_SECONDARY_VIEW_CONFIGURATION_LAYER_INFO_MSFT=1000053004,
       XR_TYPE_SECONDARY_VIEW_CONFIGURATION_SWAPCHAIN_CREATE_INFO_MSFT=1000053005,
       XR_TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT=1000055000,
       XR_TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT=1000055001,
       XR_TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT=1000055002,
       XR_TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT=1000055003,
       XR_TYPE_CONTROLLER_MODEL_STATE_MSFT=1000055004,
       XR_TYPE_VIEW_CONFIGURATION_VIEW_FOV_EPIC=1000059000,
       XR_TYPE_HOLOGRAPHIC_WINDOW_ATTACHMENT_MSFT=1000063000,
       XR_TYPE_COMPOSITION_LAYER_REPROJECTION_INFO_MSFT=1000066000,
       XR_TYPE_COMPOSITION_LAYER_REPROJECTION_PLANE_OVERRIDE_MSFT=1000066001,
       XR_TYPE_ANDROID_SURFACE_SWAPCHAIN_CREATE_INFO_FB=1000070000,
       XR_TYPE_INTERACTION_PROFILE_ANALOG_THRESHOLD_VALVE=1000079000,
       XR_TYPE_HAND_JOINTS_MOTION_RANGE_INFO_EXT=1000080000,
       XR_TYPE_LOADER_INIT_INFO_ANDROID_KHR=1000089000,
       XR_TYPE_VULKAN_INSTANCE_CREATE_INFO_KHR=1000090000,
       XR_TYPE_VULKAN_DEVICE_CREATE_INFO_KHR=1000090001,
       XR_TYPE_VULKAN_GRAPHICS_DEVICE_GET_INFO_KHR=1000090003,
       XR_TYPE_COMPOSITION_LAYER_EQUIRECT2_KHR=1000091000,
       XR_TYPE_SCENE_OBSERVER_CREATE_INFO_MSFT=1000097000,
       XR_TYPE_SCENE_CREATE_INFO_MSFT=1000097001,
       XR_TYPE_NEW_SCENE_COMPUTE_INFO_MSFT=1000097002,
       XR_TYPE_VISUAL_MESH_COMPUTE_LOD_INFO_MSFT=1000097003,
       XR_TYPE_SCENE_COMPONENTS_MSFT=1000097004,
       XR_TYPE_SCENE_COMPONENTS_GET_INFO_MSFT=1000097005,
       XR_TYPE_SCENE_COMPONENT_LOCATIONS_MSFT=1000097006,
       XR_TYPE_SCENE_COMPONENTS_LOCATE_INFO_MSFT=1000097007,
       XR_TYPE_SCENE_OBJECTS_MSFT=1000097008,
       XR_TYPE_SCENE_COMPONENT_PARENT_FILTER_INFO_MSFT=1000097009,
       XR_TYPE_SCENE_OBJECT_TYPES_FILTER_INFO_MSFT=1000097010,
       XR_TYPE_SCENE_PLANES_MSFT=1000097011,
       XR_TYPE_SCENE_PLANE_ALIGNMENT_FILTER_INFO_MSFT=1000097012,
       XR_TYPE_SCENE_MESHES_MSFT=1000097013,
       XR_TYPE_SCENE_MESH_BUFFERS_GET_INFO_MSFT=1000097014,
       XR_TYPE_SCENE_MESH_BUFFERS_MSFT=1000097015,
       XR_TYPE_SCENE_MESH_VERTEX_BUFFER_MSFT=1000097016,
       XR_TYPE_SCENE_MESH_INDICES_UINT32_MSFT=1000097017,
       XR_TYPE_SCENE_MESH_INDICES_UINT16_MSFT=1000097018,
       XR_TYPE_SERIALIZED_SCENE_FRAGMENT_DATA_GET_INFO_MSFT=1000098000,
       XR_TYPE_SCENE_DESERIALIZE_INFO_MSFT=1000098001,
       XR_TYPE_EVENT_DATA_DISPLAY_REFRESH_RATE_CHANGED_FB=1000101000,
       XR_TYPE_SYSTEM_COLOR_SPACE_PROPERTIES_FB=1000108000,
       XR_TYPE_BINDING_MODIFICATIONS_KHR=1000120000,
       XR_TYPE_VIEW_LOCATE_FOVEATED_RENDERING_VARJO=1000121000,
       XR_TYPE_FOVEATED_VIEW_CONFIGURATION_VIEW_VARJO=1000121001,
       XR_TYPE_SYSTEM_FOVEATED_RENDERING_PROPERTIES_VARJO=1000121002,
       XR_TYPE_COMPOSITION_LAYER_DEPTH_TEST_VARJO=1000122000,
       XR_TYPE_SWAPCHAIN_STATE_ANDROID_SURFACE_DIMENSIONS_FB=1000161000,
       XR_TYPE_SWAPCHAIN_STATE_SAMPLER_OPENGL_ES_FB=1000162000,
       XR_TYPE_SWAPCHAIN_STATE_SAMPLER_VULKAN_FB=1000163000,
       XR_TYPE_GRAPHICS_BINDING_VULKAN2_KHR=XR_TYPE_GRAPHICS_BINDING_VULKAN_KHR,
       XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN2_KHR=XR_TYPE_GRAPHICS_REQUIREMENTS_VULKAN_KHR,
       XR_TYPE_SWAPCHAIN_IMAGE_VULKAN2_KHR=XR_TYPE_SWAPCHAIN_IMAGE_VULKAN_KHR
      );

     PPXrResult=^PXrResult;
     PXrResult=^TXrResult;
     TXrResult=
      (
       XR_ERROR_COLOR_SPACE_UNSUPPORTED_FB=-1000108000,
       XR_ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB=-1000101000,
       XR_ERROR_SCENE_COMPUTE_CONSISTENCY_MISMATCH_MSFT=-1000097005,
       XR_ERROR_SCENE_COMPUTE_FEATURE_INCOMPATIBLE_MSFT=-1000097004,
       XR_ERROR_SCENE_MESH_BUFFER_ID_INVALID_MSFT=-1000097003,
       XR_ERROR_SCENE_COMPONENT_TYPE_MISMATCH_MSFT=-1000097002,
       XR_ERROR_SCENE_COMPONENT_ID_INVALID_MSFT=-1000097001,
       XR_ERROR_COMPUTE_NEW_SCENE_NOT_COMPLETED_MSFT=-1000097000,
       XR_ERROR_REPROJECTION_MODE_UNSUPPORTED_MSFT=-1000066000,
       XR_ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT=-1000055000,
       XR_ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT=-1000053000,
       XR_ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT=-1000039001,
       XR_ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR=-1000003001,
       XR_ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR=-1000003000,
       _UNUSED_START=-100,
       XR_ERROR_RUNTIME_UNAVAILABLE=-51,                                         //< The loader was unable to find or load a runtime.
       XR_ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING=-50,                          //< The fname:xrGetGraphicsRequirements* call was not made before calling fname:xrCreateSession.
       XR_ERROR_LOCALIZED_NAME_INVALID=-49,                                      //< The localized name provided was invalid.
       XR_ERROR_LOCALIZED_NAME_DUPLICATED=-48,                                   //< The localized name provided was a duplicate of an already-existing resource.
       XR_ERROR_ACTIONSETS_ALREADY_ATTACHED=-47,                                 //< The session already has attached action sets.
       XR_ERROR_ACTIONSET_NOT_ATTACHED=-46,                                      //< A referenced action set is not attached to the session.
       XR_ERROR_NAME_INVALID=-45,                                                //< The name provided was invalid.
       XR_ERROR_NAME_DUPLICATED=-44,                                             //< The name provided was a duplicate of an already-existing resource.
       XR_ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED=-42,                          //< The specified environment blend mode is not supported by the runtime or platform.
       XR_ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED=-41,                         //< The specified view configuration type is not supported by the runtime or platform.
       XR_ERROR_INDEX_OUT_OF_RANGE=-40,                                          //< The supplied index was outside the range of valid indices.
       XR_ERROR_POSE_INVALID=-39,                                                //< The supplied pose was invalid with respect to the requirements.
       XR_ERROR_GRAPHICS_DEVICE_INVALID=-38,                                     //< The given graphics device is not in a valid state. The graphics device could be lost or initialized without meeting graphics requirements.
       XR_ERROR_CALL_ORDER_INVALID=-37,                                          //< The call was made without having made a previously required call.
       XR_ERROR_API_LAYER_NOT_PRESENT=-36,                                       //< A requested API layer is not present or could not be loaded.
       XR_ERROR_FORM_FACTOR_UNAVAILABLE=-35,                                     //< The specified form factor is supported, but the device is currently not available, e.g. not plugged in or powered off.
       XR_ERROR_FORM_FACTOR_UNSUPPORTED=-34,                                     //< The specified form factor is not supported by the current runtime or platform.
       XR_ERROR_FILE_CONTENTS_INVALID=-33,                                       //< The file's contents were invalid.
       XR_ERROR_FILE_ACCESS_ERROR=-32,                                           //< The file could not be accessed.
       XR_ERROR_REFERENCE_SPACE_UNSUPPORTED=-31,                                 //< The specified reference space is not supported by the runtime or system.
       XR_ERROR_TIME_INVALID=-30,                                                //< The provided basetype:XrTime was zero, negative, or out of range.
       XR_ERROR_SESSION_NOT_STOPPING=-29,                                        //< The session is not in the stopping state.
       XR_ERROR_SESSION_NOT_READY=-28,                                           //< The session is not in the ready state.
       XR_ERROR_ACTION_TYPE_MISMATCH=-27,                                        //< The API used to retrieve an action's state does not match the action's type.
       XR_ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED=-26,                                //< The image format is not supported by the runtime or platform.
       XR_ERROR_SWAPCHAIN_RECT_INVALID=-25,                                      //< The image rect was negatively sized or otherwise invalid.
       XR_ERROR_LAYER_LIMIT_EXCEEDED=-24,                                        //< The number of specified layers is greater than the supported number.
       XR_ERROR_LAYER_INVALID=-23,                                               //< The layer was NULL or otherwise invalid.
       XR_ERROR_PATH_UNSUPPORTED=-22,                                            //< The semantic path is unsupported.
       XR_ERROR_PATH_FORMAT_INVALID=-21,                                         //< The semantic path character format is invalid.
       XR_ERROR_PATH_COUNT_EXCEEDED=-20,                                         //< The maximum number of supported semantic paths has been reached.
       XR_ERROR_PATH_INVALID=-19,                                                //< The provided basetype:XrPath was not valid.
       XR_ERROR_SYSTEM_INVALID=-18,                                              //< The provided basetype:XrSystemId was invalid.
       XR_ERROR_SESSION_LOST=-17,                                                //< The slink:XrSession was lost. It will need to be destroyed and optionally recreated.
       XR_ERROR_SESSION_NOT_RUNNING=-16,                                         //< The session <<session_not_running, is not yet running>>.
       XR_ERROR_SESSION_RUNNING=-14,                                             //< The session <<session_running, is already running>>.
       XR_ERROR_INSTANCE_LOST=-13,                                               //< The slink:XrInstance was lost or could not be found. It will need to be destroyed and optionally recreated.
       XR_ERROR_HANDLE_INVALID=-12,                                              //< A supplied object handle was invalid.
       XR_ERROR_SIZE_INSUFFICIENT=-11,                                           //< The supplied size was smaller than required.
       XR_ERROR_LIMIT_REACHED=-10,                                               //< The runtime supports no more of the requested resource.
       XR_ERROR_EXTENSION_NOT_PRESENT=-9,                                        //< A requested extension is not supported.
       XR_ERROR_FEATURE_UNSUPPORTED=-8,                                          //< The requested feature is not supported.
       XR_ERROR_FUNCTION_UNSUPPORTED=-7,                                         //< The requested function was not found or is otherwise unsupported.
       XR_ERROR_INITIALIZATION_FAILED=-6,                                        //< Initialization of object could not be completed.
       XR_ERROR_API_VERSION_UNSUPPORTED=-4,                                      //< The runtime does not support the requested API version.
       XR_ERROR_OUT_OF_MEMORY=-3,                                                //< A memory allocation has failed.
       XR_ERROR_RUNTIME_FAILURE=-2,                                              //< The runtime failed to handle the function in an unexpected way that is not covered by another error result. 
       XR_ERROR_VALIDATION_FAILURE=-1,                                           //< The function usage was invalid in some way.
       XR_SUCCESS=0,                                                             //< Function successfully completed.
       XR_TIMEOUT_EXPIRED=1,                                                     //< The specified timeout time occurred before the operation could complete.
       XR_SESSION_LOSS_PENDING=3,                                                //< The session will be lost soon.
       XR_EVENT_UNAVAILABLE=4,                                                   //< No event was available.
       XR_SPACE_BOUNDS_UNAVAILABLE=7,                                            //< The space's bounds are not known at the moment.
       XR_SESSION_NOT_FOCUSED=8,                                                 //< The session is not in the focused state.
       XR_FRAME_DISCARDED=9                                                      //< A frame has been discarded from composition.
      );

     PPXrObjectType=^PXrObjectType;
     PXrObjectType=^TXrObjectType;
     TXrObjectType=
      (
       XR_OBJECT_TYPE_UNKNOWN=0,
       XR_OBJECT_TYPE_INSTANCE=1,                                                //< XrInstance
       XR_OBJECT_TYPE_SESSION=2,                                                 //< XrSession
       XR_OBJECT_TYPE_SWAPCHAIN=3,                                               //< XrSwapchain
       XR_OBJECT_TYPE_SPACE=4,                                                   //< XrSpace
       XR_OBJECT_TYPE_ACTION_SET=5,                                              //< XrActionSet
       XR_OBJECT_TYPE_ACTION=6,                                                  //< XrAction
       XR_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT=1000019000,
       XR_OBJECT_TYPE_SPATIAL_ANCHOR_MSFT=1000039000,
       XR_OBJECT_TYPE_HAND_TRACKER_EXT=1000051000,
       XR_OBJECT_TYPE_SCENE_OBSERVER_MSFT=1000097000,
       XR_OBJECT_TYPE_SCENE_MSFT=1000097001
      );

     PPXrAndroidThreadTypeKHR=^PXrAndroidThreadTypeKHR;
     PXrAndroidThreadTypeKHR=^TXrAndroidThreadTypeKHR;
     TXrAndroidThreadTypeKHR=
      (
       XR_ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR=1,
       XR_ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR=2,
       XR_ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR=3,
       XR_ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR=4
      );

     PPXrEyeVisibility=^PXrEyeVisibility;
     PXrEyeVisibility=^TXrEyeVisibility;
     TXrEyeVisibility=
      (
       XR_EYE_VISIBILITY_BOTH=0,                                                 //< Display in both eyes.
       XR_EYE_VISIBILITY_LEFT=1,                                                 //< Display in the left eye only.
       XR_EYE_VISIBILITY_RIGHT=2                                                 //< Display in the right eye only.
      );

     PPXrActionType=^PXrActionType;
     PXrActionType=^TXrActionType;
     TXrActionType=
      (
       XR_ACTION_TYPE_BOOLEAN_INPUT=1,
       XR_ACTION_TYPE_FLOAT_INPUT=2,
       XR_ACTION_TYPE_VECTOR2F_INPUT=3,
       XR_ACTION_TYPE_POSE_INPUT=4,
       XR_ACTION_TYPE_VIBRATION_OUTPUT=100
      );

     PPXrReferenceSpaceType=^PXrReferenceSpaceType;
     PXrReferenceSpaceType=^TXrReferenceSpaceType;
     TXrReferenceSpaceType=
      (
       XR_REFERENCE_SPACE_TYPE_VIEW=1,
       XR_REFERENCE_SPACE_TYPE_LOCAL=2,
       XR_REFERENCE_SPACE_TYPE_STAGE=3,
       XR_REFERENCE_SPACE_TYPE_UNBOUNDED_MSFT=1000038000,
       XR_REFERENCE_SPACE_TYPE_COMBINED_EYE_VARJO=1000121000
      );

     PPXrFormFactor=^PXrFormFactor;
     PXrFormFactor=^TXrFormFactor;
     TXrFormFactor=
      (
       XR_FORM_FACTOR_HEAD_MOUNTED_DISPLAY=1,
       XR_FORM_FACTOR_HANDHELD_DISPLAY=2
      );

     PPXrViewConfigurationType=^PXrViewConfigurationType;
     PXrViewConfigurationType=^TXrViewConfigurationType;
     TXrViewConfigurationType=
      (
       XR_VIEW_CONFIGURATION_TYPE_PRIMARY_MONO=1,
       XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO=2,
       XR_VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO=1000037000,
       XR_VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT=1000054000
      );

     PPXrEnvironmentBlendMode=^PXrEnvironmentBlendMode;
     PXrEnvironmentBlendMode=^TXrEnvironmentBlendMode;
     TXrEnvironmentBlendMode=
      (
       XR_ENVIRONMENT_BLEND_MODE_OPAQUE=1,
       XR_ENVIRONMENT_BLEND_MODE_ADDITIVE=2,
       XR_ENVIRONMENT_BLEND_MODE_ALPHA_BLEND=3
      );

     PPXrSessionState=^PXrSessionState;
     PXrSessionState=^TXrSessionState;
     TXrSessionState=
      (
       XR_SESSION_STATE_UNKNOWN=0,
       XR_SESSION_STATE_IDLE=1,
       XR_SESSION_STATE_READY=2,
       XR_SESSION_STATE_SYNCHRONIZED=3,
       XR_SESSION_STATE_VISIBLE=4,
       XR_SESSION_STATE_FOCUSED=5,
       XR_SESSION_STATE_STOPPING=6,
       XR_SESSION_STATE_LOSS_PENDING=7,
       XR_SESSION_STATE_EXITING=8
      );

     PPXrPerfSettingsLevelEXT=^PXrPerfSettingsLevelEXT;
     PXrPerfSettingsLevelEXT=^TXrPerfSettingsLevelEXT;
     TXrPerfSettingsLevelEXT=
      (
       XR_PERF_SETTINGS_LEVEL_POWER_SAVINGS_EXT=0,                               //< Performance settings hint used by the application to indicate that it enters a non-XR section (head-locked / static screen), during which power savings are to be prioritized
       XR_PERF_SETTINGS_LEVEL_SUSTAINED_LOW_EXT=25,                              //< Performance settings hint used by the application to indicate that it enters a low and stable complexity section, during which reducing power is more important than occasional late rendering frames
       XR_PERF_SETTINGS_LEVEL_SUSTAINED_HIGH_EXT=50,                             //< Performance settings hint used by the application to indicate that it enters a high or dynamic complexity section, during which the XR Runtime strives for consistent XR compositing and frame rendering within a thermally sustainable range
       XR_PERF_SETTINGS_LEVEL_BOOST_EXT=75                                       //< Performance settings hint used by the application to indicate that the application enters a section with very high complexity, during which the XR Runtime is allowed to step up beyond the thermally sustainable range
      );

     PPXrPerfSettingsDomainEXT=^PXrPerfSettingsDomainEXT;
     PXrPerfSettingsDomainEXT=^TXrPerfSettingsDomainEXT;
     TXrPerfSettingsDomainEXT=
      (
       XR_PERF_SETTINGS_DOMAIN_CPU_EXT=1,                                        //< Indicates that the performance settings or notification applies to CPU domain
       XR_PERF_SETTINGS_DOMAIN_GPU_EXT=2                                         //< Indicates that the performance settings or notification applies to GPU domain
      );

     PPXrPerfSettingsSubDomainEXT=^PXrPerfSettingsSubDomainEXT;
     PXrPerfSettingsSubDomainEXT=^TXrPerfSettingsSubDomainEXT;
     TXrPerfSettingsSubDomainEXT=
      (
       XR_PERF_SETTINGS_SUB_DOMAIN_COMPOSITING_EXT=1,                            //< Indicates that the performance notification originates from the COMPOSITING sub-domain
       XR_PERF_SETTINGS_SUB_DOMAIN_RENDERING_EXT=2,                              //< Indicates that the performance notification originates from the RENDERING sub-domain
       XR_PERF_SETTINGS_SUB_DOMAIN_THERMAL_EXT=3                                 //< Indicates that the performance notification originates from the THERMAL sub-domain
      );

     PPXrPerfSettingsNotificationLevelEXT=^PXrPerfSettingsNotificationLevelEXT;
     PXrPerfSettingsNotificationLevelEXT=^TXrPerfSettingsNotificationLevelEXT;
     TXrPerfSettingsNotificationLevelEXT=
      (
       XR_PERF_SETTINGS_NOTIF_LEVEL_NORMAL_EXT=0,                                //< Notifies that the sub-domain has reached a level where no further actions other than currently applied are necessary
       XR_PERF_SETTINGS_NOTIF_LEVEL_WARNING_EXT=25,                              //< Notifies that the sub-domain has reached an early warning level where the application should start proactive mitigation actions with the goal to return to the ename:XR_PERF_NOTIF_LEVEL_NORMAL level
       XR_PERF_SETTINGS_NOTIF_LEVEL_IMPAIRED_EXT=75                              //< Notifies that the sub-domain has reached a critical level with significant performance degradation. The application should take drastic mitigation action
      );

     PPXrVisibilityMaskTypeKHR=^PXrVisibilityMaskTypeKHR;
     PXrVisibilityMaskTypeKHR=^TXrVisibilityMaskTypeKHR;
     TXrVisibilityMaskTypeKHR=
      (
       XR_VISIBILITY_MASK_TYPE_HIDDEN_TRIANGLE_MESH_KHR=1,                       //< exclusive mesh; indicates that which the viewer cannot see.
       XR_VISIBILITY_MASK_TYPE_VISIBLE_TRIANGLE_MESH_KHR=2,                      //< inclusive mesh; indicates strictly that which the viewer can see.
       XR_VISIBILITY_MASK_TYPE_LINE_LOOP_KHR=3                                   //< line loop; traces the outline of the area the viewer can see.
      );

     PPXrHandEXT=^PXrHandEXT;
     PXrHandEXT=^TXrHandEXT;
     TXrHandEXT=
      (
       XR_HAND_LEFT_EXT=1,
       XR_HAND_RIGHT_EXT=2
      );

     PPXrHandJointEXT=^PXrHandJointEXT;
     PXrHandJointEXT=^TXrHandJointEXT;
     TXrHandJointEXT=
      (
       XR_HAND_JOINT_PALM_EXT=0,
       XR_HAND_JOINT_WRIST_EXT=1,
       XR_HAND_JOINT_THUMB_METACARPAL_EXT=2,
       XR_HAND_JOINT_THUMB_PROXIMAL_EXT=3,
       XR_HAND_JOINT_THUMB_DISTAL_EXT=4,
       XR_HAND_JOINT_THUMB_TIP_EXT=5,
       XR_HAND_JOINT_INDEX_METACARPAL_EXT=6,
       XR_HAND_JOINT_INDEX_PROXIMAL_EXT=7,
       XR_HAND_JOINT_INDEX_INTERMEDIATE_EXT=8,
       XR_HAND_JOINT_INDEX_DISTAL_EXT=9,
       XR_HAND_JOINT_INDEX_TIP_EXT=10,
       XR_HAND_JOINT_MIDDLE_METACARPAL_EXT=11,
       XR_HAND_JOINT_MIDDLE_PROXIMAL_EXT=12,
       XR_HAND_JOINT_MIDDLE_INTERMEDIATE_EXT=13,
       XR_HAND_JOINT_MIDDLE_DISTAL_EXT=14,
       XR_HAND_JOINT_MIDDLE_TIP_EXT=15,
       XR_HAND_JOINT_RING_METACARPAL_EXT=16,
       XR_HAND_JOINT_RING_PROXIMAL_EXT=17,
       XR_HAND_JOINT_RING_INTERMEDIATE_EXT=18,
       XR_HAND_JOINT_RING_DISTAL_EXT=19,
       XR_HAND_JOINT_RING_TIP_EXT=20,
       XR_HAND_JOINT_LITTLE_METACARPAL_EXT=21,
       XR_HAND_JOINT_LITTLE_PROXIMAL_EXT=22,
       XR_HAND_JOINT_LITTLE_INTERMEDIATE_EXT=23,
       XR_HAND_JOINT_LITTLE_DISTAL_EXT=24,
       XR_HAND_JOINT_LITTLE_TIP_EXT=25
      );

     PPXrHandJointSetEXT=^PXrHandJointSetEXT;
     PXrHandJointSetEXT=^TXrHandJointSetEXT;
     TXrHandJointSetEXT=
      (
       XR_HAND_JOINT_SET_DEFAULT_EXT=0
      );

     PPXrHandJointsMotionRangeEXT=^PXrHandJointsMotionRangeEXT;
     PXrHandJointsMotionRangeEXT=^TXrHandJointsMotionRangeEXT;
     TXrHandJointsMotionRangeEXT=
      (
       XR_HAND_JOINTS_MOTION_RANGE_UNOBSTRUCTED_EXT=1,
       XR_HAND_JOINTS_MOTION_RANGE_CONFORMING_TO_CONTROLLER_EXT=2
      );

     PPXrHandPoseTypeMSFT=^PXrHandPoseTypeMSFT;
     PXrHandPoseTypeMSFT=^TXrHandPoseTypeMSFT;
     TXrHandPoseTypeMSFT=
      (
       XR_HAND_POSE_TYPE_TRACKED_MSFT=0,
       XR_HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT=1
      );

     PPXrColorSpaceFB=^PXrColorSpaceFB;
     PXrColorSpaceFB=^TXrColorSpaceFB;
     TXrColorSpaceFB=
      (
       XR_COLOR_SPACE_UNMANAGED_FB=0,
       XR_COLOR_SPACE_REC2020_FB=1,
       XR_COLOR_SPACE_REC709_FB=2,
       XR_COLOR_SPACE_RIFT_CV1_FB=3,
       XR_COLOR_SPACE_RIFT_S_FB=4,
       XR_COLOR_SPACE_QUEST_FB=5,
       XR_COLOR_SPACE_P3_FB=6,
       XR_COLOR_SPACE_ADOBE_RGB_FB=7
      );

     PPXrReprojectionModeMSFT=^PXrReprojectionModeMSFT;
     PXrReprojectionModeMSFT=^TXrReprojectionModeMSFT;
     TXrReprojectionModeMSFT=
      (
       XR_REPROJECTION_MODE_DEPTH_MSFT=1,
       XR_REPROJECTION_MODE_PLANAR_FROM_DEPTH_MSFT=2,
       XR_REPROJECTION_MODE_PLANAR_MANUAL_MSFT=3,
       XR_REPROJECTION_MODE_ORIENTATION_ONLY_MSFT=4
      );

     PPXrInstanceCreateFlagBits=^PXrInstanceCreateFlagBits;
     PXrInstanceCreateFlagBits=^TXrInstanceCreateFlagBits;
     TXrInstanceCreateFlagBits=
      (
       TXrInstanceCreateFlagBitsDummyValue=0
      );

     PPXrSessionCreateFlagBits=^PXrSessionCreateFlagBits;
     PXrSessionCreateFlagBits=^TXrSessionCreateFlagBits;
     TXrSessionCreateFlagBits=
      (
       TXrSessionCreateFlagBitsDummyValue=0
      );

     PPXrSwapchainCreateFlagBits=^PXrSwapchainCreateFlagBits;
     PXrSwapchainCreateFlagBits=^TXrSwapchainCreateFlagBits;
     TXrSwapchainCreateFlagBits=
      (
       XR_SWAPCHAIN_CREATE_PROTECTED_CONTENT_BIT=$00000001,                      //< Content will be protected from CPU access
       XR_SWAPCHAIN_CREATE_STATIC_IMAGE_BIT=$00000002                            //< Only one image will be acquired from this swapchain over its lifetime
      );

     PPXrSwapchainUsageFlagBits=^PXrSwapchainUsageFlagBits;
     PXrSwapchainUsageFlagBits=^TXrSwapchainUsageFlagBits;
     TXrSwapchainUsageFlagBits=
      (
       XR_SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT=$00000001,                        //< Specifies that the image may: be a color rendering target.
       XR_SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT=$00000002,                //< Specifies that the image may: be a depth/stencil rendering target.
       XR_SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT=$00000004,                        //< Specifies that the image may: be accessed out of order and that access may: be via atomic operations.
       XR_SWAPCHAIN_USAGE_TRANSFER_SRC_BIT=$00000008,                            //< Specifies that the image may: be used as the source of a transfer operation.
       XR_SWAPCHAIN_USAGE_TRANSFER_DST_BIT=$00000010,                            //< Specifies that the image may: be used as the destination of a transfer operation.
       XR_SWAPCHAIN_USAGE_SAMPLED_BIT=$00000020,                                 //< Specifies that the image may: be sampled by a shader.
       XR_SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT=$00000040,                          //< Specifies that the image may: be reinterpreted as another image format.
       XR_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND=$00000080
      );

     PPXrViewStateFlagBits=^PXrViewStateFlagBits;
     PXrViewStateFlagBits=^TXrViewStateFlagBits;
     TXrViewStateFlagBits=
      (
       XR_VIEW_STATE_ORIENTATION_VALID_BIT=$00000001,                            //< Indicates validity of all slink:XrView orientations
       XR_VIEW_STATE_POSITION_VALID_BIT=$00000002,                               //< Indicates validity of all slink:XrView positions
       XR_VIEW_STATE_ORIENTATION_TRACKED_BIT=$00000004,                          //< Indicates whether all slink:XrView orientations are actively tracked
       XR_VIEW_STATE_POSITION_TRACKED_BIT=$00000008                              //< Indicates whether all slink:XrView positions are actively tracked
      );

     PPXrCompositionLayerFlagBits=^PXrCompositionLayerFlagBits;
     PXrCompositionLayerFlagBits=^TXrCompositionLayerFlagBits;
     TXrCompositionLayerFlagBits=
      (
       XR_COMPOSITION_LAYER_CORRECT_CHROMATIC_ABERRATION_BIT=$00000001,          //< Enables chromatic aberration correction when not done by default.
       XR_COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT=$00000002,            //< Enables the layer texture alpha channel.
       XR_COMPOSITION_LAYER_UNPREMULTIPLIED_ALPHA_BIT=$00000004                  //< Indicates the texture color channels have not been premultiplied by the texture alpha channel.
      );

     PPXrSpaceLocationFlagBits=^PXrSpaceLocationFlagBits;
     PXrSpaceLocationFlagBits=^TXrSpaceLocationFlagBits;
     TXrSpaceLocationFlagBits=
      (
       XR_SPACE_LOCATION_ORIENTATION_VALID_BIT=$00000001,                        //< Indicates that the pname:orientation member contains valid data
       XR_SPACE_LOCATION_POSITION_VALID_BIT=$00000002,                           //< Indicates that the pname:position member contains valid data
       XR_SPACE_LOCATION_ORIENTATION_TRACKED_BIT=$00000004,                      //< Indicates whether pname:pose member contains an actively tracked pname:orientation
       XR_SPACE_LOCATION_POSITION_TRACKED_BIT=$00000008                          //< Indicates whether pname:pose member contains an actively tracked pname:position
      );

     PPXrSpaceVelocityFlagBits=^PXrSpaceVelocityFlagBits;
     PXrSpaceVelocityFlagBits=^TXrSpaceVelocityFlagBits;
     TXrSpaceVelocityFlagBits=
      (
       XR_SPACE_VELOCITY_LINEAR_VALID_BIT=$00000001,                             //< Indicates that the pname:linearVelocity member contains valid data
       XR_SPACE_VELOCITY_ANGULAR_VALID_BIT=$00000002                             //< Indicates that the pname:angularVelocity member contains valid data
      );

     PPXrInputSourceLocalizedNameFlagBits=^PXrInputSourceLocalizedNameFlagBits;
     PXrInputSourceLocalizedNameFlagBits=^TXrInputSourceLocalizedNameFlagBits;
     TXrInputSourceLocalizedNameFlagBits=
      (
       XR_INPUT_SOURCE_LOCALIZED_NAME_USER_PATH_BIT=$00000001,                   //< Asks for the part of the string which indicates the top level user path the source represents
       XR_INPUT_SOURCE_LOCALIZED_NAME_INTERACTION_PROFILE_BIT=$00000002,         //< Asks for the part of the string which represents the interaction profile of the source
       XR_INPUT_SOURCE_LOCALIZED_NAME_COMPONENT_BIT=$00000004                    //< Asks for the part of the string which represents the component on the device which needs to be interacted with
      );

     PPXrVulkanInstanceCreateFlagBitsKHR=^PXrVulkanInstanceCreateFlagBitsKHR;
     PXrVulkanInstanceCreateFlagBitsKHR=^TXrVulkanInstanceCreateFlagBitsKHR;
     TXrVulkanInstanceCreateFlagBitsKHR=
      (
       TXrVulkanInstanceCreateFlagBitsKHRDummyValue=0
      );

     PPXrVulkanDeviceCreateFlagBitsKHR=^PXrVulkanDeviceCreateFlagBitsKHR;
     PXrVulkanDeviceCreateFlagBitsKHR=^TXrVulkanDeviceCreateFlagBitsKHR;
     TXrVulkanDeviceCreateFlagBitsKHR=
      (
       TXrVulkanDeviceCreateFlagBitsKHRDummyValue=0
      );

     PPXrDebugUtilsMessageSeverityFlagBitsEXT=^PXrDebugUtilsMessageSeverityFlagBitsEXT;
     PXrDebugUtilsMessageSeverityFlagBitsEXT=^TXrDebugUtilsMessageSeverityFlagBitsEXT;
     TXrDebugUtilsMessageSeverityFlagBitsEXT=
      (
       XR_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT=$00000001,                //< Most verbose output severity, typically used for debugging.
       XR_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT=$00000010,                   //< General info message
       XR_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT=$00000100,                //< Indicates the item may be the cause of issues.
       XR_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT=$00001000                   //< Indicates that the item is definitely related to erroneous behavior.
      );

     PPXrDebugUtilsMessageTypeFlagBitsEXT=^PXrDebugUtilsMessageTypeFlagBitsEXT;
     PXrDebugUtilsMessageTypeFlagBitsEXT=^TXrDebugUtilsMessageTypeFlagBitsEXT;
     TXrDebugUtilsMessageTypeFlagBitsEXT=
      (
       XR_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT=$00000001,                    //< Indicates this is a general message
       XR_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT=$00000002,                 //< Indicates the message is related to a validation message
       XR_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT=$00000004,                //< Indicates the message is related to a potential performance situation
       XR_DEBUG_UTILS_MESSAGE_TYPE_CONFORMANCE_BIT_EXT=$00000008                 //< Indicates the message is related to a non-conformant runtime result
      );

     PPXrOverlayMainSessionFlagBitsEXTX=^PXrOverlayMainSessionFlagBitsEXTX;
     PXrOverlayMainSessionFlagBitsEXTX=^TXrOverlayMainSessionFlagBitsEXTX;
     TXrOverlayMainSessionFlagBitsEXTX=
      (
       XR_OVERLAY_MAIN_SESSION_ENABLED_COMPOSITION_LAYER_INFO_DEPTH_BIT_EXTX=$00000001 //< Indicates the main session enabled `XR_KHR_composition_layer_depth`
      );

     PPXrOverlaySessionCreateFlagBitsEXTX=^PXrOverlaySessionCreateFlagBitsEXTX;
     PXrOverlaySessionCreateFlagBitsEXTX=^TXrOverlaySessionCreateFlagBitsEXTX;
     TXrOverlaySessionCreateFlagBitsEXTX=
      (
       TXrOverlaySessionCreateFlagBitsEXTXDummyValue=0
      );

     PPXrSpatialGraphNodeTypeMSFT=^PXrSpatialGraphNodeTypeMSFT;
     PXrSpatialGraphNodeTypeMSFT=^TXrSpatialGraphNodeTypeMSFT;
     TXrSpatialGraphNodeTypeMSFT=
      (
       XR_SPATIAL_GRAPH_NODE_TYPE_STATIC_MSFT=1,
       XR_SPATIAL_GRAPH_NODE_TYPE_DYNAMIC_MSFT=2
      );

     PPXrSceneObjectTypeMSFT=^PXrSceneObjectTypeMSFT;
     PXrSceneObjectTypeMSFT=^TXrSceneObjectTypeMSFT;
     TXrSceneObjectTypeMSFT=
      (
       XR_SCENE_OBJECT_TYPE_UNCATEGORIZED_MSFT=-1,
       XR_SCENE_OBJECT_TYPE_BACKGROUND_MSFT=1,
       XR_SCENE_OBJECT_TYPE_WALL_MSFT=2,
       XR_SCENE_OBJECT_TYPE_FLOOR_MSFT=3,
       XR_SCENE_OBJECT_TYPE_CEILING_MSFT=4,
       XR_SCENE_OBJECT_TYPE_PLATFORM_MSFT=5,
       XR_SCENE_OBJECT_TYPE_INFERRED_MSFT=6
      );

     PPXrScenePlaneAlignmentTypeMSFT=^PXrScenePlaneAlignmentTypeMSFT;
     PXrScenePlaneAlignmentTypeMSFT=^TXrScenePlaneAlignmentTypeMSFT;
     TXrScenePlaneAlignmentTypeMSFT=
      (
       XR_SCENE_PLANE_ALIGNMENT_TYPE_NON_ORTHOGONAL_MSFT=0,
       XR_SCENE_PLANE_ALIGNMENT_TYPE_HORIZONTAL_MSFT=1,
       XR_SCENE_PLANE_ALIGNMENT_TYPE_VERTICAL_MSFT=2
      );

     PPXrSceneComputeStateMSFT=^PXrSceneComputeStateMSFT;
     PXrSceneComputeStateMSFT=^TXrSceneComputeStateMSFT;
     TXrSceneComputeStateMSFT=
      (
       XR_SCENE_COMPUTE_STATE_NONE_MSFT=0,
       XR_SCENE_COMPUTE_STATE_UPDATING_MSFT=1,
       XR_SCENE_COMPUTE_STATE_COMPLETED_MSFT=2,
       XR_SCENE_COMPUTE_STATE_COMPLETED_WITH_ERROR_MSFT=3
      );

     PPXrSceneComponentTypeMSFT=^PXrSceneComponentTypeMSFT;
     PXrSceneComponentTypeMSFT=^TXrSceneComponentTypeMSFT;
     TXrSceneComponentTypeMSFT=
      (
       XR_SCENE_COMPONENT_TYPE_INVALID_MSFT=-1,
       XR_SCENE_COMPONENT_TYPE_OBJECT_MSFT=1,
       XR_SCENE_COMPONENT_TYPE_PLANE_MSFT=2,
       XR_SCENE_COMPONENT_TYPE_VISUAL_MESH_MSFT=3,
       XR_SCENE_COMPONENT_TYPE_COLLIDER_MESH_MSFT=4,
       XR_SCENE_COMPONENT_TYPE_SERIALIZED_SCENE_FRAGMENT_MSFT=1000098000
      );

     PPXrSceneComputeFeatureMSFT=^PXrSceneComputeFeatureMSFT;
     PXrSceneComputeFeatureMSFT=^TXrSceneComputeFeatureMSFT;
     TXrSceneComputeFeatureMSFT=
      (
       XR_SCENE_COMPUTE_FEATURE_PLANE_MSFT=1,
       XR_SCENE_COMPUTE_FEATURE_PLANE_MESH_MSFT=2,
       XR_SCENE_COMPUTE_FEATURE_VISUAL_MESH_MSFT=3,
       XR_SCENE_COMPUTE_FEATURE_COLLIDER_MESH_MSFT=4,
       XR_SCENE_COMPUTE_FEATURE_SERIALIZE_SCENE_MSFT=1000098000
      );

     PPXrSceneComputeConsistencyMSFT=^PXrSceneComputeConsistencyMSFT;
     PXrSceneComputeConsistencyMSFT=^TXrSceneComputeConsistencyMSFT;
     TXrSceneComputeConsistencyMSFT=
      (
       XR_SCENE_COMPUTE_CONSISTENCY_SNAPSHOT_COMPLETE_MSFT=1,
       XR_SCENE_COMPUTE_CONSISTENCY_SNAPSHOT_INCOMPLETE_FAST_MSFT=2,
       XR_SCENE_COMPUTE_CONSISTENCY_OCCLUSION_OPTIMIZED_MSFT=3
      );

     PPXrMeshComputeLodMSFT=^PXrMeshComputeLodMSFT;
     PXrMeshComputeLodMSFT=^TXrMeshComputeLodMSFT;
     TXrMeshComputeLodMSFT=
      (
       XR_MESH_COMPUTE_LOD_COARSE_MSFT=1,
       XR_MESH_COMPUTE_LOD_MEDIUM_MSFT=2,
       XR_MESH_COMPUTE_LOD_FINE_MSFT=3,
       XR_MESH_COMPUTE_LOD_UNLIMITED_MSFT=4
      );

     PPXrAndroidSurfaceSwapchainFlagBitsFB=^PXrAndroidSurfaceSwapchainFlagBitsFB;
     PXrAndroidSurfaceSwapchainFlagBitsFB=^TXrAndroidSurfaceSwapchainFlagBitsFB;
     TXrAndroidSurfaceSwapchainFlagBitsFB=
      (
       XR_ANDROID_SURFACE_SWAPCHAIN_SYNCHRONOUS_BIT_FB=$00000001,                //< Create the underlying BufferQueue in synchronous mode
       XR_ANDROID_SURFACE_SWAPCHAIN_USE_TIMESTAMPS_BIT_FB=$00000002              //< Acquire most recent buffer whose presentation timestamp is not greater than display time of final composited frame
      );

     PPXrVector2f=^PXrVector2f;
     PXrVector2f=^TXrVector2f;
     TXrVector2f=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TXrFloat;
       y:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aX:TXrFloat;
                          const aY:TXrFloat);
{$endif}
     end;

     PPXrVector3f=^PXrVector3f;
     PXrVector3f=^TXrVector3f;
     TXrVector3f=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TXrFloat;
       y:TXrFloat;
       z:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aX:TXrFloat;
                          const aY:TXrFloat;
                          const aZ:TXrFloat);
{$endif}
     end;

     PPXrVector4f=^PXrVector4f;
     PXrVector4f=^TXrVector4f;
     TXrVector4f=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TXrFloat;
       y:TXrFloat;
       z:TXrFloat;
       w:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aX:TXrFloat;
                          const aY:TXrFloat;
                          const aZ:TXrFloat;
                          const aW:TXrFloat);
{$endif}
     end;

     PPXrColor4f=^PXrColor4f;
     PXrColor4f=^TXrColor4f;
     TXrColor4f=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       r:TXrFloat;
       g:TXrFloat;
       b:TXrFloat;
       a:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aR:TXrFloat;
                          const aG:TXrFloat;
                          const aB:TXrFloat;
                          const aA:TXrFloat);
{$endif}
     end;

     PPXrQuaternionf=^PXrQuaternionf;
     PXrQuaternionf=^TXrQuaternionf;
     TXrQuaternionf=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TXrFloat;
       y:TXrFloat;
       z:TXrFloat;
       w:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aX:TXrFloat;
                          const aY:TXrFloat;
                          const aZ:TXrFloat;
                          const aW:TXrFloat);
{$endif}
     end;

     PPXrPosef=^PXrPosef;
     PXrPosef=^TXrPosef;
     TXrPosef=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       orientation:TXrQuaternionf;
       position:TXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aOrientation:TXrQuaternionf;
                          const aPosition:TXrVector3f);
{$endif}
     end;

     PPXrOffset2Df=^PXrOffset2Df;
     PXrOffset2Df=^TXrOffset2Df;
     TXrOffset2Df=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TXrFloat;
       y:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aX:TXrFloat;
                          const aY:TXrFloat);
{$endif}
     end;

     PPXrExtent2Df=^PXrExtent2Df;
     PXrExtent2Df=^TXrExtent2Df;
     TXrExtent2Df=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       width:TXrFloat;
       height:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aWidth:TXrFloat;
                          const aHeight:TXrFloat);
{$endif}
     end;

     PPXrRect2Df=^PXrRect2Df;
     PXrRect2Df=^TXrRect2Df;
     TXrRect2Df=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       offset:TXrOffset2Df;
       extent:TXrExtent2Df;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aOffset:TXrOffset2Df;
                          const aExtent:TXrExtent2Df);
{$endif}
     end;

     PPXrOffset2Di=^PXrOffset2Di;
     PXrOffset2Di=^TXrOffset2Di;
     TXrOffset2Di=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       x:TXrInt32;
       y:TXrInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aX:TXrInt32;
                          const aY:TXrInt32);
{$endif}
     end;

     PPXrExtent2Di=^PXrExtent2Di;
     PXrExtent2Di=^TXrExtent2Di;
     TXrExtent2Di=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       width:TXrInt32;
       height:TXrInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aWidth:TXrInt32;
                          const aHeight:TXrInt32);
{$endif}
     end;

     PPXrRect2Di=^PXrRect2Di;
     PXrRect2Di=^TXrRect2Di;
     TXrRect2Di=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       offset:TXrOffset2Di;
       extent:TXrExtent2Di;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aOffset:TXrOffset2Di;
                          const aExtent:TXrExtent2Di);
{$endif}
     end;

     PPXrBaseInStructure=^PXrBaseInStructure;
     PXrBaseInStructure=^TXrBaseInStructure;
     TXrBaseInStructure=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrBaseInStructure;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrBaseOutStructure=^PXrBaseOutStructure;
     PXrBaseOutStructure=^TXrBaseOutStructure;
     TXrBaseOutStructure=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrBaseOutStructure;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrApiLayerProperties=^PXrApiLayerProperties;
     PXrApiLayerProperties=^TXrApiLayerProperties;
     TXrApiLayerProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerName:array[0..XR_MAX_API_LAYER_NAME_SIZE-1] of TXrChar;
       specVersion:TXrVersion;
       layerVersion:TXrUInt32;
       description:array[0..XR_MAX_API_LAYER_DESCRIPTION_SIZE-1] of TXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerName:TXrCharString;
                          const aSpecVersion:TXrVersion;
                          const aLayerVersion:TXrUInt32;
                          const aDescription:TXrCharString);
{$endif}
     end;

     PPXrExtensionProperties=^PXrExtensionProperties;
     PXrExtensionProperties=^TXrExtensionProperties;
     TXrExtensionProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       extensionName:array[0..XR_MAX_EXTENSION_NAME_SIZE-1] of TXrChar;
       extensionVersion:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aExtensionName:TXrCharString;
                          const aExtensionVersion:TXrUInt32);
{$endif}
     end;

     PPXrApplicationInfo=^PXrApplicationInfo;
     PXrApplicationInfo=^TXrApplicationInfo;
     TXrApplicationInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       applicationName:array[0..XR_MAX_APPLICATION_NAME_SIZE-1] of TXrChar;
       applicationVersion:TXrUInt32;
       engineName:array[0..XR_MAX_ENGINE_NAME_SIZE-1] of TXrChar;
       engineVersion:TXrUInt32;
       apiVersion:TXrVersion;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aApplicationName:TXrCharString;
                          const aApplicationVersion:TXrUInt32;
                          const aEngineName:TXrCharString;
                          const aEngineVersion:TXrUInt32;
                          const aApiVersion:TXrVersion);
{$endif}
     end;

     PPXrInstanceCreateInfo=^PXrInstanceCreateInfo;
     PXrInstanceCreateInfo=^TXrInstanceCreateInfo;
     TXrInstanceCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       createFlags:TXrInstanceCreateFlags;
       applicationInfo:TXrApplicationInfo;
       enabledApiLayerCount:TXrUInt32;
       enabledApiLayerNames:PPXrChar;
       enabledExtensionCount:TXrUInt32;
       enabledExtensionNames:PPXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCreateFlags:TXrInstanceCreateFlags;
                          const aApplicationInfo:TXrApplicationInfo;
                          const aEnabledApiLayerCount:TXrUInt32;
                          const aEnabledApiLayerNames:PPXrChar;
                          const aEnabledExtensionCount:TXrUInt32;
                          const aEnabledExtensionNames:PPXrChar);
{$endif}
     end;

     PPXrInstanceProperties=^PXrInstanceProperties;
     PXrInstanceProperties=^TXrInstanceProperties;
     TXrInstanceProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       runtimeVersion:TXrVersion;
       runtimeName:array[0..XR_MAX_RUNTIME_NAME_SIZE-1] of TXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aRuntimeVersion:TXrVersion;
                          const aRuntimeName:TXrCharString);
{$endif}
     end;

     PPXrSystemGetInfo=^PXrSystemGetInfo;
     PXrSystemGetInfo=^TXrSystemGetInfo;
     TXrSystemGetInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       formFactor:TXrFormFactor;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aFormFactor:TXrFormFactor);
{$endif}
     end;

     PPXrSystemGraphicsProperties=^PXrSystemGraphicsProperties;
     PXrSystemGraphicsProperties=^TXrSystemGraphicsProperties;
     TXrSystemGraphicsProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       maxSwapchainImageHeight:TXrUInt32;
       maxSwapchainImageWidth:TXrUInt32;
       maxLayerCount:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMaxSwapchainImageHeight:TXrUInt32;
                          const aMaxSwapchainImageWidth:TXrUInt32;
                          const aMaxLayerCount:TXrUInt32);
{$endif}
     end;

     PPXrSystemTrackingProperties=^PXrSystemTrackingProperties;
     PXrSystemTrackingProperties=^TXrSystemTrackingProperties;
     TXrSystemTrackingProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       orientationTracking:TXrBool32;
       positionTracking:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aOrientationTracking:TXrBool32;
                          const aPositionTracking:TXrBool32);
{$endif}
     end;

     PPXrSystemProperties=^PXrSystemProperties;
     PXrSystemProperties=^TXrSystemProperties;
     TXrSystemProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       systemId:TXrSystemId;
       vendorId:TXrUInt32;
       systemName:array[0..XR_MAX_SYSTEM_NAME_SIZE-1] of TXrChar;
       graphicsProperties:TXrSystemGraphicsProperties;
       trackingProperties:TXrSystemTrackingProperties;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSystemId:TXrSystemId;
                          const aVendorId:TXrUInt32;
                          const aSystemName:TXrCharString;
                          const aGraphicsProperties:TXrSystemGraphicsProperties;
                          const aTrackingProperties:TXrSystemTrackingProperties);
{$endif}
     end;

     PPXrGraphicsBindingOpenGLWin32KHR=^PXrGraphicsBindingOpenGLWin32KHR;
     PXrGraphicsBindingOpenGLWin32KHR=^TXrGraphicsBindingOpenGLWin32KHR;
     TXrGraphicsBindingOpenGLWin32KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       hDC:TXrHDC;
       hGLRC:TXrHGLRC;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aHDC:TXrHDC;
                          const aHGLRC:TXrHGLRC);
{$endif}
     end;

{$ifdef XLIB}
     PPXrGraphicsBindingOpenGLXlibKHR=^PXrGraphicsBindingOpenGLXlibKHR;
     PXrGraphicsBindingOpenGLXlibKHR=^TXrGraphicsBindingOpenGLXlibKHR;
     TXrGraphicsBindingOpenGLXlibKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       xDisplay:PXrXLIBDisplay;
       visualid:TXrUInt32;
       glxFBConfig:TGLXFBConfig;
       glxDrawable:TGLXDrawable;
       glxContext:TGLXContext;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aXDisplay:PXrXLIBDisplay;
                          const aVisualid:TXrUInt32;
                          const aGlxFBConfig:TGLXFBConfig;
                          const aGlxDrawable:TGLXDrawable;
                          const aGlxContext:TGLXContext);
{$endif}
     end;
{$endif}

{$ifdef XCB}
     PPXrGraphicsBindingOpenGLXcbKHR=^PXrGraphicsBindingOpenGLXcbKHR;
     PXrGraphicsBindingOpenGLXcbKHR=^TXrGraphicsBindingOpenGLXcbKHR;
     TXrGraphicsBindingOpenGLXcbKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       connection:PXrXCBConnection;
       screenNumber:TXrUInt32;
       fbconfigid:Txcb_glx_fbconfig_t;
       visualid:TXrXCBVisualID;
       glxDrawable:Txcb_glx_drawable_t;
       glxContext:Txcb_glx_context_t;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aConnection:PXrXCBConnection;
                          const aScreenNumber:TXrUInt32;
                          const aFbconfigid:Txcb_glx_fbconfig_t;
                          const aVisualid:TXrXCBVisualID;
                          const aGlxDrawable:Txcb_glx_drawable_t;
                          const aGlxContext:Txcb_glx_context_t);
{$endif}
     end;
{$endif}

{$ifdef Wayland}
     PPXrGraphicsBindingOpenGLWaylandKHR=^PXrGraphicsBindingOpenGLWaylandKHR;
     PXrGraphicsBindingOpenGLWaylandKHR=^TXrGraphicsBindingOpenGLWaylandKHR;
     TXrGraphicsBindingOpenGLWaylandKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       display:PXrWaylandDisplay;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDisplay:PXrWaylandDisplay);
{$endif}
     end;
{$endif}

     PPXrGraphicsBindingD3D11KHR=^PXrGraphicsBindingD3D11KHR;
     PXrGraphicsBindingD3D11KHR=^TXrGraphicsBindingD3D11KHR;
     TXrGraphicsBindingD3D11KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       device:ID3D11Device;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDevice:ID3D11Device);
{$endif}
     end;

     PPXrGraphicsBindingD3D12KHR=^PXrGraphicsBindingD3D12KHR;
     PXrGraphicsBindingD3D12KHR=^TXrGraphicsBindingD3D12KHR;
     TXrGraphicsBindingD3D12KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       device:ID3D12Device;
       queue:ID3D12CommandQueue;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDevice:ID3D12Device;
                          const aQueue:ID3D12CommandQueue);
{$endif}
     end;

{$ifdef Android}
     PPXrGraphicsBindingOpenGLESAndroidKHR=^PXrGraphicsBindingOpenGLESAndroidKHR;
     PXrGraphicsBindingOpenGLESAndroidKHR=^TXrGraphicsBindingOpenGLESAndroidKHR;
     TXrGraphicsBindingOpenGLESAndroidKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       display:TEGLDisplay;
       config:TEGLConfig;
       context:TEGLContext;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDisplay:TEGLDisplay;
                          const aConfig:TEGLConfig;
                          const aContext:TEGLContext);
{$endif}
     end;
{$endif}

     PPXrGraphicsBindingVulkanKHR=^PXrGraphicsBindingVulkanKHR;
     PXrGraphicsBindingVulkanKHR=^TXrGraphicsBindingVulkanKHR;
     TXrGraphicsBindingVulkanKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       instance:TVkInstance;
       physicalDevice:TVkPhysicalDevice;
       device:TVkDevice;
       queueFamilyIndex:TXrUInt32;
       queueIndex:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aInstance:TVkInstance;
                          const aPhysicalDevice:TVkPhysicalDevice;
                          const aDevice:TVkDevice;
                          const aQueueFamilyIndex:TXrUInt32;
                          const aQueueIndex:TXrUInt32);
{$endif}
     end;

     PPXrSessionCreateInfo=^PXrSessionCreateInfo;
     PXrSessionCreateInfo=^TXrSessionCreateInfo;
     TXrSessionCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       createFlags:TXrSessionCreateFlags;
       systemId:TXrSystemId;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCreateFlags:TXrSessionCreateFlags;
                          const aSystemId:TXrSystemId);
{$endif}
     end;

     PPXrSessionBeginInfo=^PXrSessionBeginInfo;
     PXrSessionBeginInfo=^TXrSessionBeginInfo;
     TXrSessionBeginInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       primaryViewConfigurationType:TXrViewConfigurationType;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPrimaryViewConfigurationType:TXrViewConfigurationType);
{$endif}
     end;

     PPXrSwapchainCreateInfo=^PXrSwapchainCreateInfo;
     PXrSwapchainCreateInfo=^TXrSwapchainCreateInfo;
     TXrSwapchainCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       createFlags:TXrSwapchainCreateFlags;
       usageFlags:TXrSwapchainUsageFlags;
       format:TXrInt64;
       sampleCount:TXrUInt32;
       width:TXrUInt32;
       height:TXrUInt32;
       faceCount:TXrUInt32;
       arraySize:TXrUInt32;
       mipCount:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCreateFlags:TXrSwapchainCreateFlags;
                          const aUsageFlags:TXrSwapchainUsageFlags;
                          const aFormat:TXrInt64;
                          const aSampleCount:TXrUInt32;
                          const aWidth:TXrUInt32;
                          const aHeight:TXrUInt32;
                          const aFaceCount:TXrUInt32;
                          const aArraySize:TXrUInt32;
                          const aMipCount:TXrUInt32);
{$endif}
     end;

     PPXrSwapchainImageBaseHeader=^PXrSwapchainImageBaseHeader;
     PXrSwapchainImageBaseHeader=^TXrSwapchainImageBaseHeader;
     TXrSwapchainImageBaseHeader=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrSwapchainImageOpenGLKHR=^PXrSwapchainImageOpenGLKHR;
     PXrSwapchainImageOpenGLKHR=^TXrSwapchainImageOpenGLKHR;
     TXrSwapchainImageOpenGLKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       image:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aImage:TXrUInt32);
{$endif}
     end;

     PPXrSwapchainImageOpenGLESKHR=^PXrSwapchainImageOpenGLESKHR;
     PXrSwapchainImageOpenGLESKHR=^TXrSwapchainImageOpenGLESKHR;
     TXrSwapchainImageOpenGLESKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       image:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aImage:TXrUInt32);
{$endif}
     end;

     PPXrSwapchainImageVulkanKHR=^PXrSwapchainImageVulkanKHR;
     PXrSwapchainImageVulkanKHR=^TXrSwapchainImageVulkanKHR;
     TXrSwapchainImageVulkanKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       image:TVkImage;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aImage:TVkImage);
{$endif}
     end;

     PPXrSwapchainImageD3D11KHR=^PXrSwapchainImageD3D11KHR;
     PXrSwapchainImageD3D11KHR=^TXrSwapchainImageD3D11KHR;
     TXrSwapchainImageD3D11KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       texture:ID3D11Texture2D;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aTexture:ID3D11Texture2D);
{$endif}
     end;

     PPXrSwapchainImageD3D12KHR=^PXrSwapchainImageD3D12KHR;
     PXrSwapchainImageD3D12KHR=^TXrSwapchainImageD3D12KHR;
     TXrSwapchainImageD3D12KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       texture:ID3D12Resource;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aTexture:ID3D12Resource);
{$endif}
     end;

     PPXrSwapchainImageAcquireInfo=^PXrSwapchainImageAcquireInfo;
     PXrSwapchainImageAcquireInfo=^TXrSwapchainImageAcquireInfo;
     TXrSwapchainImageAcquireInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid; //< Pointer to next structure
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrSwapchainImageWaitInfo=^PXrSwapchainImageWaitInfo;
     PXrSwapchainImageWaitInfo=^TXrSwapchainImageWaitInfo;
     TXrSwapchainImageWaitInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid; //< Pointer to next structure
       timeout:TXrDuration;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aTimeout:TXrDuration);
{$endif}
     end;

     PPXrSwapchainImageReleaseInfo=^PXrSwapchainImageReleaseInfo;
     PXrSwapchainImageReleaseInfo=^TXrSwapchainImageReleaseInfo;
     TXrSwapchainImageReleaseInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid; //< Pointer to next structure
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrReferenceSpaceCreateInfo=^PXrReferenceSpaceCreateInfo;
     PXrReferenceSpaceCreateInfo=^TXrReferenceSpaceCreateInfo;
     TXrReferenceSpaceCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       referenceSpaceType:TXrReferenceSpaceType;
       poseInReferenceSpace:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aReferenceSpaceType:TXrReferenceSpaceType;
                          const aPoseInReferenceSpace:TXrPosef);
{$endif}
     end;

     PPXrActionSpaceCreateInfo=^PXrActionSpaceCreateInfo;
     PXrActionSpaceCreateInfo=^TXrActionSpaceCreateInfo;
     TXrActionSpaceCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       action:TXrAction;
       subactionPath:TXrPath;
       poseInActionSpace:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAction:TXrAction;
                          const aSubactionPath:TXrPath;
                          const aPoseInActionSpace:TXrPosef);
{$endif}
     end;

     PPXrSpaceLocation=^PXrSpaceLocation;
     PXrSpaceLocation=^TXrSpaceLocation;
     TXrSpaceLocation=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       locationFlags:TXrSpaceLocationFlags;
       pose:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLocationFlags:TXrSpaceLocationFlags;
                          const aPose:TXrPosef);
{$endif}
     end;

     PPXrSpaceVelocity=^PXrSpaceVelocity;
     PXrSpaceVelocity=^TXrSpaceVelocity;
     TXrSpaceVelocity=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       velocityFlags:TXrSpaceVelocityFlags;
       linearVelocity:TXrVector3f;
       angularVelocity:TXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVelocityFlags:TXrSpaceVelocityFlags;
                          const aLinearVelocity:TXrVector3f;
                          const aAngularVelocity:TXrVector3f);
{$endif}
     end;

     PPXrFovf=^PXrFovf;
     PXrFovf=^TXrFovf;
     TXrFovf=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       angleLeft:TXrFloat;
       angleRight:TXrFloat;
       angleUp:TXrFloat;
       angleDown:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAngleLeft:TXrFloat;
                          const aAngleRight:TXrFloat;
                          const aAngleUp:TXrFloat;
                          const aAngleDown:TXrFloat);
{$endif}
     end;

     PPXrView=^PXrView;
     PXrView=^TXrView;
     TXrView=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       pose:TXrPosef;
       fov:TXrFovf;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPose:TXrPosef;
                          const aFov:TXrFovf);
{$endif}
     end;

     PPXrViewLocateInfo=^PXrViewLocateInfo;
     PXrViewLocateInfo=^TXrViewLocateInfo;
     TXrViewLocateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationType:TXrViewConfigurationType;
       displayTime:TXrTime;
       space:TXrSpace;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationType:TXrViewConfigurationType;
                          const aDisplayTime:TXrTime;
                          const aSpace:TXrSpace);
{$endif}
     end;

     PPXrViewState=^PXrViewState;
     PXrViewState=^TXrViewState;
     TXrViewState=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewStateFlags:TXrViewStateFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewStateFlags:TXrViewStateFlags);
{$endif}
     end;

     PPXrViewConfigurationView=^PXrViewConfigurationView;
     PXrViewConfigurationView=^TXrViewConfigurationView;
     TXrViewConfigurationView=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       recommendedImageRectWidth:TXrUInt32;
       maxImageRectWidth:TXrUInt32;
       recommendedImageRectHeight:TXrUInt32;
       maxImageRectHeight:TXrUInt32;
       recommendedSwapchainSampleCount:TXrUInt32;
       maxSwapchainSampleCount:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aRecommendedImageRectWidth:TXrUInt32;
                          const aMaxImageRectWidth:TXrUInt32;
                          const aRecommendedImageRectHeight:TXrUInt32;
                          const aMaxImageRectHeight:TXrUInt32;
                          const aRecommendedSwapchainSampleCount:TXrUInt32;
                          const aMaxSwapchainSampleCount:TXrUInt32);
{$endif}
     end;

     PPXrSwapchainSubImage=^PXrSwapchainSubImage;
     PXrSwapchainSubImage=^TXrSwapchainSubImage;
     TXrSwapchainSubImage=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       swapchain:TXrSwapchain;
       imageRect:TXrRect2Di;
       imageArrayIndex:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSwapchain:TXrSwapchain;
                          const aImageRect:TXrRect2Di;
                          const aImageArrayIndex:TXrUInt32);
{$endif}
     end;

     PPXrCompositionLayerBaseHeader=^PXrCompositionLayerBaseHeader;
     PXrCompositionLayerBaseHeader=^TXrCompositionLayerBaseHeader;
     TXrCompositionLayerBaseHeader=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace);
{$endif}
     end;

     PPXrCompositionLayerProjectionView=^PXrCompositionLayerProjectionView;
     PXrCompositionLayerProjectionView=^TXrCompositionLayerProjectionView;
     TXrCompositionLayerProjectionView=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       pose:TXrPosef;
       fov:TXrFovf;
       subImage:TXrSwapchainSubImage;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPose:TXrPosef;
                          const aFov:TXrFovf;
                          const aSubImage:TXrSwapchainSubImage);
{$endif}
     end;

     PPXrCompositionLayerProjection=^PXrCompositionLayerProjection;
     PXrCompositionLayerProjection=^TXrCompositionLayerProjection;
     TXrCompositionLayerProjection=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
       viewCount:TXrUInt32;
       views:PXrCompositionLayerProjectionView;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace;
                          const aViewCount:TXrUInt32;
                          const aViews:PXrCompositionLayerProjectionView);
{$endif}
     end;

     PPXrCompositionLayerQuad=^PXrCompositionLayerQuad;
     PXrCompositionLayerQuad=^TXrCompositionLayerQuad;
     TXrCompositionLayerQuad=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
       eyeVisibility:TXrEyeVisibility;
       subImage:TXrSwapchainSubImage;
       pose:TXrPosef;
       size:TXrExtent2Df;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace;
                          const aEyeVisibility:TXrEyeVisibility;
                          const aSubImage:TXrSwapchainSubImage;
                          const aPose:TXrPosef;
                          const aSize:TXrExtent2Df);
{$endif}
     end;

     PPXrCompositionLayerCylinderKHR=^PXrCompositionLayerCylinderKHR;
     PXrCompositionLayerCylinderKHR=^TXrCompositionLayerCylinderKHR;
     TXrCompositionLayerCylinderKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
       eyeVisibility:TXrEyeVisibility;
       subImage:TXrSwapchainSubImage;
       pose:TXrPosef;
       radius:TXrFloat;
       centralAngle:TXrFloat;
       aspectRatio:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace;
                          const aEyeVisibility:TXrEyeVisibility;
                          const aSubImage:TXrSwapchainSubImage;
                          const aPose:TXrPosef;
                          const aRadius:TXrFloat;
                          const aCentralAngle:TXrFloat;
                          const aAspectRatio:TXrFloat);
{$endif}
     end;

     PPXrCompositionLayerCubeKHR=^PXrCompositionLayerCubeKHR;
     PXrCompositionLayerCubeKHR=^TXrCompositionLayerCubeKHR;
     TXrCompositionLayerCubeKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
       eyeVisibility:TXrEyeVisibility;
       swapchain:TXrSwapchain;
       imageArrayIndex:TXrUInt32;
       orientation:TXrQuaternionf;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace;
                          const aEyeVisibility:TXrEyeVisibility;
                          const aSwapchain:TXrSwapchain;
                          const aImageArrayIndex:TXrUInt32;
                          const aOrientation:TXrQuaternionf);
{$endif}
     end;

     PPXrCompositionLayerEquirectKHR=^PXrCompositionLayerEquirectKHR;
     PXrCompositionLayerEquirectKHR=^TXrCompositionLayerEquirectKHR;
     TXrCompositionLayerEquirectKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
       eyeVisibility:TXrEyeVisibility;
       subImage:TXrSwapchainSubImage;
       pose:TXrPosef;
       radius:TXrFloat;
       scale:TXrVector2f;
       bias:TXrVector2f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace;
                          const aEyeVisibility:TXrEyeVisibility;
                          const aSubImage:TXrSwapchainSubImage;
                          const aPose:TXrPosef;
                          const aRadius:TXrFloat;
                          const aScale:TXrVector2f;
                          const aBias:TXrVector2f);
{$endif}
     end;

     PPXrCompositionLayerDepthInfoKHR=^PXrCompositionLayerDepthInfoKHR;
     PXrCompositionLayerDepthInfoKHR=^TXrCompositionLayerDepthInfoKHR;
     TXrCompositionLayerDepthInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       subImage:TXrSwapchainSubImage;
       minDepth:TXrFloat;
       maxDepth:TXrFloat;
       nearZ:TXrFloat;
       farZ:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSubImage:TXrSwapchainSubImage;
                          const aMinDepth:TXrFloat;
                          const aMaxDepth:TXrFloat;
                          const aNearZ:TXrFloat;
                          const aFarZ:TXrFloat);
{$endif}
     end;

     PPXrFrameBeginInfo=^PXrFrameBeginInfo;
     PXrFrameBeginInfo=^TXrFrameBeginInfo;
     TXrFrameBeginInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid; //< This is currently empty, awaiting future extensions.
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrFrameEndInfo=^PXrFrameEndInfo;
     PXrFrameEndInfo=^TXrFrameEndInfo;
     TXrFrameEndInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       displayTime:TXrTime;
       environmentBlendMode:TXrEnvironmentBlendMode;
       layerCount:TXrUInt32;
       layers:PPXrCompositionLayerBaseHeader;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDisplayTime:TXrTime;
                          const aEnvironmentBlendMode:TXrEnvironmentBlendMode;
                          const aLayerCount:TXrUInt32;
                          const aLayers:PPXrCompositionLayerBaseHeader);
{$endif}
     end;

     PPXrFrameWaitInfo=^PXrFrameWaitInfo;
     PXrFrameWaitInfo=^TXrFrameWaitInfo;
     TXrFrameWaitInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrFrameState=^PXrFrameState;
     PXrFrameState=^TXrFrameState;
     TXrFrameState=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       predictedDisplayTime:TXrTime;
       predictedDisplayPeriod:TXrDuration;
       shouldRender:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPredictedDisplayTime:TXrTime;
                          const aPredictedDisplayPeriod:TXrDuration;
                          const aShouldRender:TXrBool32);
{$endif}
     end;

     PPXrHapticBaseHeader=^PXrHapticBaseHeader;
     PXrHapticBaseHeader=^TXrHapticBaseHeader;
     TXrHapticBaseHeader=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrHapticVibration=^PXrHapticVibration;
     PXrHapticVibration=^TXrHapticVibration;
     TXrHapticVibration=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       duration:TXrDuration;
       frequency:TXrFloat;
       amplitude:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDuration:TXrDuration;
                          const aFrequency:TXrFloat;
                          const aAmplitude:TXrFloat);
{$endif}
     end;

     PPXrEventDataBaseHeader=^PXrEventDataBaseHeader;
     PXrEventDataBaseHeader=^TXrEventDataBaseHeader;
     TXrEventDataBaseHeader=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrEventDataBuffer=^PXrEventDataBuffer;
     PXrEventDataBuffer=^TXrEventDataBuffer;
     TXrEventDataBuffer=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       varying:array[0..3999] of TXrUInt8;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVarying:array of TXrUInt8);
{$endif}
     end;

     PPXrEventDataEventsLost=^PXrEventDataEventsLost;
     PXrEventDataEventsLost=^TXrEventDataEventsLost;
     TXrEventDataEventsLost=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       lostEventCount:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLostEventCount:TXrUInt32);
{$endif}
     end;

     PPXrEventDataInstanceLossPending=^PXrEventDataInstanceLossPending;
     PXrEventDataInstanceLossPending=^TXrEventDataInstanceLossPending;
     TXrEventDataInstanceLossPending=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       lossTime:TXrTime;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLossTime:TXrTime);
{$endif}
     end;

     PPXrEventDataSessionStateChanged=^PXrEventDataSessionStateChanged;
     PXrEventDataSessionStateChanged=^TXrEventDataSessionStateChanged;
     TXrEventDataSessionStateChanged=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       session:TXrSession;
       state:TXrSessionState;
       time:TXrTime;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSession:TXrSession;
                          const aState:TXrSessionState;
                          const aTime:TXrTime);
{$endif}
     end;

     PPXrEventDataReferenceSpaceChangePending=^PXrEventDataReferenceSpaceChangePending;
     PXrEventDataReferenceSpaceChangePending=^TXrEventDataReferenceSpaceChangePending;
     TXrEventDataReferenceSpaceChangePending=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       session:TXrSession;
       referenceSpaceType:TXrReferenceSpaceType;
       changeTime:TXrTime;
       poseValid:TXrBool32;
       poseInPreviousSpace:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSession:TXrSession;
                          const aReferenceSpaceType:TXrReferenceSpaceType;
                          const aChangeTime:TXrTime;
                          const aPoseValid:TXrBool32;
                          const aPoseInPreviousSpace:TXrPosef);
{$endif}
     end;

     PPXrEventDataPerfSettingsEXT=^PXrEventDataPerfSettingsEXT;
     PXrEventDataPerfSettingsEXT=^TXrEventDataPerfSettingsEXT;
     TXrEventDataPerfSettingsEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       domain:TXrPerfSettingsDomainEXT;
       subDomain:TXrPerfSettingsSubDomainEXT;
       fromLevel:TXrPerfSettingsNotificationLevelEXT;
       toLevel:TXrPerfSettingsNotificationLevelEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDomain:TXrPerfSettingsDomainEXT;
                          const aSubDomain:TXrPerfSettingsSubDomainEXT;
                          const aFromLevel:TXrPerfSettingsNotificationLevelEXT;
                          const aToLevel:TXrPerfSettingsNotificationLevelEXT);
{$endif}
     end;

     PPXrEventDataVisibilityMaskChangedKHR=^PXrEventDataVisibilityMaskChangedKHR;
     PXrEventDataVisibilityMaskChangedKHR=^TXrEventDataVisibilityMaskChangedKHR;
     TXrEventDataVisibilityMaskChangedKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       session:TXrSession;
       viewConfigurationType:TXrViewConfigurationType;
       viewIndex:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSession:TXrSession;
                          const aViewConfigurationType:TXrViewConfigurationType;
                          const aViewIndex:TXrUInt32);
{$endif}
     end;

     PPXrViewConfigurationProperties=^PXrViewConfigurationProperties;
     PXrViewConfigurationProperties=^TXrViewConfigurationProperties;
     TXrViewConfigurationProperties=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationType:TXrViewConfigurationType;
       fovMutable:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationType:TXrViewConfigurationType;
                          const aFovMutable:TXrBool32);
{$endif}
     end;

     PPXrActionStateBoolean=^PXrActionStateBoolean;
     PXrActionStateBoolean=^TXrActionStateBoolean;
     TXrActionStateBoolean=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       currentState:TXrBool32;
       changedSinceLastSync:TXrBool32;
       lastChangeTime:TXrTime;
       isActive:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCurrentState:TXrBool32;
                          const aChangedSinceLastSync:TXrBool32;
                          const aLastChangeTime:TXrTime;
                          const aIsActive:TXrBool32);
{$endif}
     end;

     PPXrActionStateFloat=^PXrActionStateFloat;
     PXrActionStateFloat=^TXrActionStateFloat;
     TXrActionStateFloat=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       currentState:TXrFloat;
       changedSinceLastSync:TXrBool32;
       lastChangeTime:TXrTime;
       isActive:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCurrentState:TXrFloat;
                          const aChangedSinceLastSync:TXrBool32;
                          const aLastChangeTime:TXrTime;
                          const aIsActive:TXrBool32);
{$endif}
     end;

     PPXrActionStateVector2f=^PXrActionStateVector2f;
     PXrActionStateVector2f=^TXrActionStateVector2f;
     TXrActionStateVector2f=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       currentState:TXrVector2f;
       changedSinceLastSync:TXrBool32;
       lastChangeTime:TXrTime;
       isActive:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCurrentState:TXrVector2f;
                          const aChangedSinceLastSync:TXrBool32;
                          const aLastChangeTime:TXrTime;
                          const aIsActive:TXrBool32);
{$endif}
     end;

     PPXrActionStatePose=^PXrActionStatePose;
     PXrActionStatePose=^TXrActionStatePose;
     TXrActionStatePose=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       isActive:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aIsActive:TXrBool32);
{$endif}
     end;

     PPXrActionStateGetInfo=^PXrActionStateGetInfo;
     PXrActionStateGetInfo=^TXrActionStateGetInfo;
     TXrActionStateGetInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       action:TXrAction;
       subactionPath:TXrPath;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAction:TXrAction;
                          const aSubactionPath:TXrPath);
{$endif}
     end;

     PPXrHapticActionInfo=^PXrHapticActionInfo;
     PXrHapticActionInfo=^TXrHapticActionInfo;
     TXrHapticActionInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       action:TXrAction;
       subactionPath:TXrPath;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAction:TXrAction;
                          const aSubactionPath:TXrPath);
{$endif}
     end;

     PPXrActionSetCreateInfo=^PXrActionSetCreateInfo;
     PXrActionSetCreateInfo=^TXrActionSetCreateInfo;
     TXrActionSetCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       actionSetName:array[0..XR_MAX_ACTION_SET_NAME_SIZE-1] of TXrChar;
       localizedActionSetName:array[0..XR_MAX_LOCALIZED_ACTION_SET_NAME_SIZE-1] of TXrChar;
       priority:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aActionSetName:TXrCharString;
                          const aLocalizedActionSetName:TXrCharString;
                          const aPriority:TXrUInt32);
{$endif}
     end;

     PPXrActionSuggestedBinding=^PXrActionSuggestedBinding;
     PXrActionSuggestedBinding=^TXrActionSuggestedBinding;
     TXrActionSuggestedBinding=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       action:TXrAction;
       binding:TXrPath;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAction:TXrAction;
                          const aBinding:TXrPath);
{$endif}
     end;

     PPXrInteractionProfileSuggestedBinding=^PXrInteractionProfileSuggestedBinding;
     PXrInteractionProfileSuggestedBinding=^TXrInteractionProfileSuggestedBinding;
     TXrInteractionProfileSuggestedBinding=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       interactionProfile:TXrPath;
       countSuggestedBindings:TXrUInt32;
       suggestedBindings:PXrActionSuggestedBinding;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aInteractionProfile:TXrPath;
                          const aCountSuggestedBindings:TXrUInt32;
                          const aSuggestedBindings:PXrActionSuggestedBinding);
{$endif}
     end;

     PPXrActiveActionSet=^PXrActiveActionSet;
     PXrActiveActionSet=^TXrActiveActionSet;
     TXrActiveActionSet=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       actionSet:TXrActionSet;
       subactionPath:TXrPath;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aActionSet:TXrActionSet;
                          const aSubactionPath:TXrPath);
{$endif}
     end;

     PPXrSessionActionSetsAttachInfo=^PXrSessionActionSetsAttachInfo;
     PXrSessionActionSetsAttachInfo=^TXrSessionActionSetsAttachInfo;
     TXrSessionActionSetsAttachInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       countActionSets:TXrUInt32;
       actionSets:PXrActionSet;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCountActionSets:TXrUInt32;
                          const aActionSets:PXrActionSet);
{$endif}
     end;

     PPXrActionsSyncInfo=^PXrActionsSyncInfo;
     PXrActionsSyncInfo=^TXrActionsSyncInfo;
     TXrActionsSyncInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       countActiveActionSets:TXrUInt32;
       activeActionSets:PXrActiveActionSet;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCountActiveActionSets:TXrUInt32;
                          const aActiveActionSets:PXrActiveActionSet);
{$endif}
     end;

     PPXrBoundSourcesForActionEnumerateInfo=^PXrBoundSourcesForActionEnumerateInfo;
     PXrBoundSourcesForActionEnumerateInfo=^TXrBoundSourcesForActionEnumerateInfo;
     TXrBoundSourcesForActionEnumerateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       action:TXrAction;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAction:TXrAction);
{$endif}
     end;

     PPXrInputSourceLocalizedNameGetInfo=^PXrInputSourceLocalizedNameGetInfo;
     PXrInputSourceLocalizedNameGetInfo=^TXrInputSourceLocalizedNameGetInfo;
     TXrInputSourceLocalizedNameGetInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       sourcePath:TXrPath;
       whichComponents:TXrInputSourceLocalizedNameFlags;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSourcePath:TXrPath;
                          const aWhichComponents:TXrInputSourceLocalizedNameFlags);
{$endif}
     end;

     PPXrEventDataInteractionProfileChanged=^PXrEventDataInteractionProfileChanged;
     PXrEventDataInteractionProfileChanged=^TXrEventDataInteractionProfileChanged;
     TXrEventDataInteractionProfileChanged=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       session:TXrSession;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSession:TXrSession);
{$endif}
     end;

     PPXrInteractionProfileState=^PXrInteractionProfileState;
     PXrInteractionProfileState=^TXrInteractionProfileState;
     TXrInteractionProfileState=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       interactionProfile:TXrPath;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aInteractionProfile:TXrPath);
{$endif}
     end;

     PPXrActionCreateInfo=^PXrActionCreateInfo;
     PXrActionCreateInfo=^TXrActionCreateInfo;
     TXrActionCreateInfo=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       actionName:array[0..XR_MAX_ACTION_NAME_SIZE-1] of TXrChar;
       actionType:TXrActionType;
       countSubactionPaths:TXrUInt32;
       subactionPaths:PXrPath;
       localizedActionName:array[0..XR_MAX_LOCALIZED_ACTION_NAME_SIZE-1] of TXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aActionName:TXrCharString;
                          const aActionType:TXrActionType;
                          const aCountSubactionPaths:TXrUInt32;
                          const aSubactionPaths:PXrPath;
                          const aLocalizedActionName:TXrCharString);
{$endif}
     end;

{$ifdef Android}
     PPXrInstanceCreateInfoAndroidKHR=^PXrInstanceCreateInfoAndroidKHR;
     PXrInstanceCreateInfoAndroidKHR=^TXrInstanceCreateInfoAndroidKHR;
     TXrInstanceCreateInfoAndroidKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       applicationVM:PXrVoid;
       applicationActivity:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aApplicationVM:PXrVoid;
                          const aApplicationActivity:PXrVoid);
{$endif}
     end;
{$endif}

     PPXrVulkanSwapchainFormatListCreateInfoKHR=^PXrVulkanSwapchainFormatListCreateInfoKHR;
     PXrVulkanSwapchainFormatListCreateInfoKHR=^TXrVulkanSwapchainFormatListCreateInfoKHR;
     TXrVulkanSwapchainFormatListCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewFormatCount:TXrUInt32;
       viewFormats:PVkFormat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewFormatCount:TXrUInt32;
                          const aViewFormats:PVkFormat);
{$endif}
     end;

     PPXrDebugUtilsObjectNameInfoEXT=^PXrDebugUtilsObjectNameInfoEXT;
     PXrDebugUtilsObjectNameInfoEXT=^TXrDebugUtilsObjectNameInfoEXT;
     TXrDebugUtilsObjectNameInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       objectType:TXrObjectType;
       objectHandle:TXrUInt64;
       objectName:PXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aObjectType:TXrObjectType;
                          const aObjectHandle:TXrUInt64;
                          const aObjectName:PXrChar);
{$endif}
     end;

     PPXrDebugUtilsLabelEXT=^PXrDebugUtilsLabelEXT;
     PXrDebugUtilsLabelEXT=^TXrDebugUtilsLabelEXT;
     TXrDebugUtilsLabelEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       labelName:PXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLabelName:PXrChar);
{$endif}
     end;

     PPXrDebugUtilsMessengerCallbackDataEXT=^PXrDebugUtilsMessengerCallbackDataEXT;
     PXrDebugUtilsMessengerCallbackDataEXT=^TXrDebugUtilsMessengerCallbackDataEXT;
     TXrDebugUtilsMessengerCallbackDataEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       messageId:PXrChar;
       functionName:PXrChar;
       message:PXrChar;
       objectCount:TXrUInt32;
       objects:PXrDebugUtilsObjectNameInfoEXT;
       sessionLabelCount:TXrUInt32;
       sessionLabels:PXrDebugUtilsLabelEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMessageId:PXrChar;
                          const aFunctionName:PXrChar;
                          const aMessage:PXrChar;
                          const aObjectCount:TXrUInt32;
                          const aObjects:PXrDebugUtilsObjectNameInfoEXT;
                          const aSessionLabelCount:TXrUInt32;
                          const aSessionLabels:PXrDebugUtilsLabelEXT);
{$endif}
     end;

     PPPFN_xrDebugUtilsMessengerCallbackEXT=^PPFN_xrDebugUtilsMessengerCallbackEXT;
     PPFN_xrDebugUtilsMessengerCallbackEXT=^TPFN_xrDebugUtilsMessengerCallbackEXT;
     TPFN_xrDebugUtilsMessengerCallbackEXT=function(messageSeverity:TXrDebugUtilsMessageSeverityFlagsEXT;messageTypes:TXrDebugUtilsMessageTypeFlagsEXT;const callbackData:PXrDebugUtilsMessengerCallbackDataEXT;userData:PXrVoid):TXrBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPXrVisibilityMaskKHR=^PXrVisibilityMaskKHR;
     PXrVisibilityMaskKHR=^TXrVisibilityMaskKHR;
     TXrVisibilityMaskKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       vertexCapacityInput:TXrUInt32;
       vertexCountOutput:TXrUInt32;
       vertices:PXrVector2f;
       indexCapacityInput:TXrUInt32;
       indexCountOutput:TXrUInt32;
       indices:PXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVertexCapacityInput:TXrUInt32;
                          const aVertexCountOutput:TXrUInt32;
                          const aVertices:PXrVector2f;
                          const aIndexCapacityInput:TXrUInt32;
                          const aIndexCountOutput:TXrUInt32;
                          const aIndices:PXrUInt32);
{$endif}
     end;

     PPXrGraphicsRequirementsOpenGLKHR=^PXrGraphicsRequirementsOpenGLKHR;
     PXrGraphicsRequirementsOpenGLKHR=^TXrGraphicsRequirementsOpenGLKHR;
     TXrGraphicsRequirementsOpenGLKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       minApiVersionSupported:TXrVersion;
       maxApiVersionSupported:TXrVersion;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMinApiVersionSupported:TXrVersion;
                          const aMaxApiVersionSupported:TXrVersion);
{$endif}
     end;

     PPXrGraphicsRequirementsOpenGLESKHR=^PXrGraphicsRequirementsOpenGLESKHR;
     PXrGraphicsRequirementsOpenGLESKHR=^TXrGraphicsRequirementsOpenGLESKHR;
     TXrGraphicsRequirementsOpenGLESKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       minApiVersionSupported:TXrVersion;
       maxApiVersionSupported:TXrVersion;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMinApiVersionSupported:TXrVersion;
                          const aMaxApiVersionSupported:TXrVersion);
{$endif}
     end;

     PPXrGraphicsRequirementsVulkanKHR=^PXrGraphicsRequirementsVulkanKHR;
     PXrGraphicsRequirementsVulkanKHR=^TXrGraphicsRequirementsVulkanKHR;
     TXrGraphicsRequirementsVulkanKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       minApiVersionSupported:TXrVersion;
       maxApiVersionSupported:TXrVersion;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMinApiVersionSupported:TXrVersion;
                          const aMaxApiVersionSupported:TXrVersion);
{$endif}
     end;

     PPXrGraphicsRequirementsD3D11KHR=^PXrGraphicsRequirementsD3D11KHR;
     PXrGraphicsRequirementsD3D11KHR=^TXrGraphicsRequirementsD3D11KHR;
     TXrGraphicsRequirementsD3D11KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       adapterLuid:TLUID;
       minFeatureLevel:TD3D_FEATURE_LEVEL;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAdapterLuid:TLUID;
                          const aMinFeatureLevel:TD3D_FEATURE_LEVEL);
{$endif}
     end;

     PPXrGraphicsRequirementsD3D12KHR=^PXrGraphicsRequirementsD3D12KHR;
     PXrGraphicsRequirementsD3D12KHR=^TXrGraphicsRequirementsD3D12KHR;
     TXrGraphicsRequirementsD3D12KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       adapterLuid:TLUID;
       minFeatureLevel:TD3D_FEATURE_LEVEL;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAdapterLuid:TLUID;
                          const aMinFeatureLevel:TD3D_FEATURE_LEVEL);
{$endif}
     end;

     PPXrVulkanInstanceCreateInfoKHR=^PXrVulkanInstanceCreateInfoKHR;
     PXrVulkanInstanceCreateInfoKHR=^TXrVulkanInstanceCreateInfoKHR;
     TXrVulkanInstanceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       systemId:TXrSystemId;
       createFlags:TXrVulkanInstanceCreateFlagsKHR;
       pfnGetInstanceProcAddr:TPFN_vkGetInstanceProcAddr;
       vulkanCreateInfo:PVkInstanceCreateInfo;
       vulkanAllocator:PVkAllocationCallbacks;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSystemId:TXrSystemId;
                          const aCreateFlags:TXrVulkanInstanceCreateFlagsKHR;
                          const aPfnGetInstanceProcAddr:TPFN_vkGetInstanceProcAddr;
                          const aVulkanCreateInfo:PVkInstanceCreateInfo;
                          const aVulkanAllocator:PVkAllocationCallbacks);
{$endif}
     end;

     PPXrVulkanDeviceCreateInfoKHR=^PXrVulkanDeviceCreateInfoKHR;
     PXrVulkanDeviceCreateInfoKHR=^TXrVulkanDeviceCreateInfoKHR;
     TXrVulkanDeviceCreateInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       systemId:TXrSystemId;
       createFlags:TXrVulkanDeviceCreateFlagsKHR;
       pfnGetInstanceProcAddr:TPFN_vkGetInstanceProcAddr;
       vulkanPhysicalDevice:TVkPhysicalDevice;
       vulkanCreateInfo:PVkDeviceCreateInfo;
       vulkanAllocator:PVkAllocationCallbacks;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSystemId:TXrSystemId;
                          const aCreateFlags:TXrVulkanDeviceCreateFlagsKHR;
                          const aPfnGetInstanceProcAddr:TPFN_vkGetInstanceProcAddr;
                          const aVulkanPhysicalDevice:TVkPhysicalDevice;
                          const aVulkanCreateInfo:PVkDeviceCreateInfo;
                          const aVulkanAllocator:PVkAllocationCallbacks);
{$endif}
     end;

     PPXrGraphicsBindingVulkan2KHR=^PXrGraphicsBindingVulkan2KHR;
     PXrGraphicsBindingVulkan2KHR=^TXrGraphicsBindingVulkan2KHR;
     TXrGraphicsBindingVulkan2KHR=TXrGraphicsBindingVulkanKHR;

     PPXrVulkanGraphicsDeviceGetInfoKHR=^PXrVulkanGraphicsDeviceGetInfoKHR;
     PXrVulkanGraphicsDeviceGetInfoKHR=^TXrVulkanGraphicsDeviceGetInfoKHR;
     TXrVulkanGraphicsDeviceGetInfoKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       systemId:TXrSystemId;
       vulkanInstance:TVkInstance;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSystemId:TXrSystemId;
                          const aVulkanInstance:TVkInstance);
{$endif}
     end;

     PPXrSwapchainImageVulkan2KHR=^PXrSwapchainImageVulkan2KHR;
     PXrSwapchainImageVulkan2KHR=^TXrSwapchainImageVulkan2KHR;
     TXrSwapchainImageVulkan2KHR=TXrSwapchainImageVulkanKHR;

     PPXrGraphicsRequirementsVulkan2KHR=^PXrGraphicsRequirementsVulkan2KHR;
     PXrGraphicsRequirementsVulkan2KHR=^TXrGraphicsRequirementsVulkan2KHR;
     TXrGraphicsRequirementsVulkan2KHR=TXrGraphicsRequirementsVulkanKHR;

     PPXrSessionCreateInfoOverlayEXTX=^PXrSessionCreateInfoOverlayEXTX;
     PXrSessionCreateInfoOverlayEXTX=^TXrSessionCreateInfoOverlayEXTX;
     TXrSessionCreateInfoOverlayEXTX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       createFlags:TXrOverlaySessionCreateFlagsEXTX;
       sessionLayersPlacement:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCreateFlags:TXrOverlaySessionCreateFlagsEXTX;
                          const aSessionLayersPlacement:TXrUInt32);
{$endif}
     end;

     PPXrEventDataMainSessionVisibilityChangedEXTX=^PXrEventDataMainSessionVisibilityChangedEXTX;
     PXrEventDataMainSessionVisibilityChangedEXTX=^TXrEventDataMainSessionVisibilityChangedEXTX;
     TXrEventDataMainSessionVisibilityChangedEXTX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       visible:TXrBool32;
       flags:TXrOverlayMainSessionFlagsEXTX;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVisible:TXrBool32;
                          const aFlags:TXrOverlayMainSessionFlagsEXTX);
{$endif}
     end;

     PPXrEventDataDisplayRefreshRateChangedFB=^PXrEventDataDisplayRefreshRateChangedFB;
     PXrEventDataDisplayRefreshRateChangedFB=^TXrEventDataDisplayRefreshRateChangedFB;
     TXrEventDataDisplayRefreshRateChangedFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       fromDisplayRefreshRate:TXrFloat;
       toDisplayRefreshRate:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aFromDisplayRefreshRate:TXrFloat;
                          const aToDisplayRefreshRate:TXrFloat);
{$endif}
     end;

     PPXrViewConfigurationDepthRangeEXT=^PXrViewConfigurationDepthRangeEXT;
     PXrViewConfigurationDepthRangeEXT=^TXrViewConfigurationDepthRangeEXT;
     TXrViewConfigurationDepthRangeEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       recommendedNearZ:TXrFloat;
       minNearZ:TXrFloat;
       recommendedFarZ:TXrFloat;
       maxFarZ:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aRecommendedNearZ:TXrFloat;
                          const aMinNearZ:TXrFloat;
                          const aRecommendedFarZ:TXrFloat;
                          const aMaxFarZ:TXrFloat);
{$endif}
     end;

     PPXrViewConfigurationViewFovEPIC=^PXrViewConfigurationViewFovEPIC;
     PXrViewConfigurationViewFovEPIC=^TXrViewConfigurationViewFovEPIC;
     TXrViewConfigurationViewFovEPIC=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       recommendedFov:TXrFovf;
       maxMutableFov:TXrFovf;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aRecommendedFov:TXrFovf;
                          const aMaxMutableFov:TXrFovf);
{$endif}
     end;

     PPXrInteractionProfileAnalogThresholdVALVE=^PXrInteractionProfileAnalogThresholdVALVE;
     PXrInteractionProfileAnalogThresholdVALVE=^TXrInteractionProfileAnalogThresholdVALVE;
     TXrInteractionProfileAnalogThresholdVALVE=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       action:TXrAction;
       binding:TXrPath;
       onThreshold:TXrFloat;
       offThreshold:TXrFloat;
       onHaptic:PXrHapticBaseHeader;
       offHaptic:PXrHapticBaseHeader;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAction:TXrAction;
                          const aBinding:TXrPath;
                          const aOnThreshold:TXrFloat;
                          const aOffThreshold:TXrFloat;
                          const aOnHaptic:PXrHapticBaseHeader;
                          const aOffHaptic:PXrHapticBaseHeader);
{$endif}
     end;

     PPXrBindingModificationBaseHeaderKHR=^PXrBindingModificationBaseHeaderKHR;
     PXrBindingModificationBaseHeaderKHR=^TXrBindingModificationBaseHeaderKHR;
     TXrBindingModificationBaseHeaderKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrBindingModificationsKHR=^PXrBindingModificationsKHR;
     PXrBindingModificationsKHR=^TXrBindingModificationsKHR;
     TXrBindingModificationsKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       bindingModificationCount:TXrUInt32;
       bindingModifications:PPXrBindingModificationBaseHeaderKHR;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aBindingModificationCount:TXrUInt32;
                          const aBindingModifications:PPXrBindingModificationBaseHeaderKHR);
{$endif}
     end;

     PPPFN_xrVoidFunction=^PPFN_xrVoidFunction;
     PPFN_xrVoidFunction=^TPFN_xrVoidFunction;
     TPFN_xrVoidFunction=procedure(); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     PPXrDebugUtilsMessengerCreateInfoEXT=^PXrDebugUtilsMessengerCreateInfoEXT;
     PXrDebugUtilsMessengerCreateInfoEXT=^TXrDebugUtilsMessengerCreateInfoEXT;
     TXrDebugUtilsMessengerCreateInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       messageSeverities:TXrDebugUtilsMessageSeverityFlagsEXT;
       messageTypes:TXrDebugUtilsMessageTypeFlagsEXT;
       userCallback:TPFN_xrDebugUtilsMessengerCallbackEXT;
       userData:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMessageSeverities:TXrDebugUtilsMessageSeverityFlagsEXT;
                          const aMessageTypes:TXrDebugUtilsMessageTypeFlagsEXT;
                          const aUserCallback:TPFN_xrDebugUtilsMessengerCallbackEXT;
                          const aUserData:PXrVoid);
{$endif}
     end;

     PPXrSystemEyeGazeInteractionPropertiesEXT=^PXrSystemEyeGazeInteractionPropertiesEXT;
     PXrSystemEyeGazeInteractionPropertiesEXT=^TXrSystemEyeGazeInteractionPropertiesEXT;
     TXrSystemEyeGazeInteractionPropertiesEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       supportsEyeGazeInteraction:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSupportsEyeGazeInteraction:TXrBool32);
{$endif}
     end;

     PPXrEyeGazeSampleTimeEXT=^PXrEyeGazeSampleTimeEXT;
     PXrEyeGazeSampleTimeEXT=^TXrEyeGazeSampleTimeEXT;
     TXrEyeGazeSampleTimeEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       time:TXrTime;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aTime:TXrTime);
{$endif}
     end;

     PPXrSpatialAnchorCreateInfoMSFT=^PXrSpatialAnchorCreateInfoMSFT;
     PXrSpatialAnchorCreateInfoMSFT=^TXrSpatialAnchorCreateInfoMSFT;
     TXrSpatialAnchorCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       space:TXrSpace;
       pose:TXrPosef;
       time:TXrTime;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSpace:TXrSpace;
                          const aPose:TXrPosef;
                          const aTime:TXrTime);
{$endif}
     end;

     PPXrSpatialAnchorSpaceCreateInfoMSFT=^PXrSpatialAnchorSpaceCreateInfoMSFT;
     PXrSpatialAnchorSpaceCreateInfoMSFT=^TXrSpatialAnchorSpaceCreateInfoMSFT;
     TXrSpatialAnchorSpaceCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       anchor:TXrSpatialAnchorMSFT;
       poseInAnchorSpace:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAnchor:TXrSpatialAnchorMSFT;
                          const aPoseInAnchorSpace:TXrPosef);
{$endif}
     end;

{$ifdef EGL}
     PPXrGraphicsBindingEGLMNDX=^PXrGraphicsBindingEGLMNDX;
     PXrGraphicsBindingEGLMNDX=^TXrGraphicsBindingEGLMNDX;
     TXrGraphicsBindingEGLMNDX=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       getProcAddress:TPFNEGLGETPROCADDRESSPROC;
       display:TEGLDisplay;
       config:TEGLConfig;
       context:TEGLContext;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aGetProcAddress:TPFNEGLGETPROCADDRESSPROC;
                          const aDisplay:TEGLDisplay;
                          const aConfig:TEGLConfig;
                          const aContext:TEGLContext);
{$endif}
     end;
{$endif}

     PPXrSpatialGraphNodeSpaceCreateInfoMSFT=^PXrSpatialGraphNodeSpaceCreateInfoMSFT;
     PXrSpatialGraphNodeSpaceCreateInfoMSFT=^TXrSpatialGraphNodeSpaceCreateInfoMSFT;
     TXrSpatialGraphNodeSpaceCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       nodeType:TXrSpatialGraphNodeTypeMSFT;
       nodeId:array[0..15] of TXrUInt8;
       pose:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aNodeType:TXrSpatialGraphNodeTypeMSFT;
                          const aNodeId:array of TXrUInt8;
                          const aPose:TXrPosef);
{$endif}
     end;

     PPXrSystemHandTrackingPropertiesEXT=^PXrSystemHandTrackingPropertiesEXT;
     PXrSystemHandTrackingPropertiesEXT=^TXrSystemHandTrackingPropertiesEXT;
     TXrSystemHandTrackingPropertiesEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       supportsHandTracking:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSupportsHandTracking:TXrBool32);
{$endif}
     end;

     PPXrHandTrackerCreateInfoEXT=^PXrHandTrackerCreateInfoEXT;
     PXrHandTrackerCreateInfoEXT=^TXrHandTrackerCreateInfoEXT;
     TXrHandTrackerCreateInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid; //< Pointer to next structure
       hand:TXrHandEXT;
       handJointSet:TXrHandJointSetEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aHand:TXrHandEXT;
                          const aHandJointSet:TXrHandJointSetEXT);
{$endif}
     end;

     PPXrHandJointsLocateInfoEXT=^PXrHandJointsLocateInfoEXT;
     PXrHandJointsLocateInfoEXT=^TXrHandJointsLocateInfoEXT;
     TXrHandJointsLocateInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       baseSpace:TXrSpace;
       time:TXrTime;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aBaseSpace:TXrSpace;
                          const aTime:TXrTime);
{$endif}
     end;

     PPXrHandJointLocationEXT=^PXrHandJointLocationEXT;
     PXrHandJointLocationEXT=^TXrHandJointLocationEXT;
     TXrHandJointLocationEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       locationFlags:TXrSpaceLocationFlags;
       pose:TXrPosef;
       radius:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLocationFlags:TXrSpaceLocationFlags;
                          const aPose:TXrPosef;
                          const aRadius:TXrFloat);
{$endif}
     end;

     PPXrHandJointVelocityEXT=^PXrHandJointVelocityEXT;
     PXrHandJointVelocityEXT=^TXrHandJointVelocityEXT;
     TXrHandJointVelocityEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       velocityFlags:TXrSpaceVelocityFlags;
       linearVelocity:TXrVector3f;
       angularVelocity:TXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVelocityFlags:TXrSpaceVelocityFlags;
                          const aLinearVelocity:TXrVector3f;
                          const aAngularVelocity:TXrVector3f);
{$endif}
     end;

     PPXrHandJointLocationsEXT=^PXrHandJointLocationsEXT;
     PXrHandJointLocationsEXT=^TXrHandJointLocationsEXT;
     TXrHandJointLocationsEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       isActive:TXrBool32;
       jointCount:TXrUInt32;
       jointLocations:PXrHandJointLocationEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aIsActive:TXrBool32;
                          const aJointCount:TXrUInt32;
                          const aJointLocations:PXrHandJointLocationEXT);
{$endif}
     end;

     PPXrHandJointVelocitiesEXT=^PXrHandJointVelocitiesEXT;
     PXrHandJointVelocitiesEXT=^TXrHandJointVelocitiesEXT;
     TXrHandJointVelocitiesEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       jointCount:TXrUInt32;
       jointVelocities:PXrHandJointVelocityEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aJointCount:TXrUInt32;
                          const aJointVelocities:PXrHandJointVelocityEXT);
{$endif}
     end;

     PPXrHandJointsMotionRangeInfoEXT=^PXrHandJointsMotionRangeInfoEXT;
     PXrHandJointsMotionRangeInfoEXT=^TXrHandJointsMotionRangeInfoEXT;
     TXrHandJointsMotionRangeInfoEXT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       handJointsMotionRange:TXrHandJointsMotionRangeEXT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aHandJointsMotionRange:TXrHandJointsMotionRangeEXT);
{$endif}
     end;

     PPXrHandMeshSpaceCreateInfoMSFT=^PXrHandMeshSpaceCreateInfoMSFT;
     PXrHandMeshSpaceCreateInfoMSFT=^TXrHandMeshSpaceCreateInfoMSFT;
     TXrHandMeshSpaceCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       handPoseType:TXrHandPoseTypeMSFT;
       poseInHandMeshSpace:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aHandPoseType:TXrHandPoseTypeMSFT;
                          const aPoseInHandMeshSpace:TXrPosef);
{$endif}
     end;

     PPXrHandMeshUpdateInfoMSFT=^PXrHandMeshUpdateInfoMSFT;
     PXrHandMeshUpdateInfoMSFT=^TXrHandMeshUpdateInfoMSFT;
     TXrHandMeshUpdateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       time:TXrTime;
       handPoseType:TXrHandPoseTypeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aTime:TXrTime;
                          const aHandPoseType:TXrHandPoseTypeMSFT);
{$endif}
     end;

     PPXrHandMeshIndexBufferMSFT=^PXrHandMeshIndexBufferMSFT;
     PXrHandMeshIndexBufferMSFT=^TXrHandMeshIndexBufferMSFT;
     TXrHandMeshIndexBufferMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       indexBufferKey:TXrUInt32;
       indexCapacityInput:TXrUInt32;
       indexCountOutput:TXrUInt32;
       indices:PXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aIndexBufferKey:TXrUInt32;
                          const aIndexCapacityInput:TXrUInt32;
                          const aIndexCountOutput:TXrUInt32;
                          const aIndices:PXrUInt32);
{$endif}
     end;

     PPXrHandMeshVertexMSFT=^PXrHandMeshVertexMSFT;
     PXrHandMeshVertexMSFT=^TXrHandMeshVertexMSFT;
     TXrHandMeshVertexMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       position:TXrVector3f;
       normal:TXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPosition:TXrVector3f;
                          const aNormal:TXrVector3f);
{$endif}
     end;

     PPXrHandMeshVertexBufferMSFT=^PXrHandMeshVertexBufferMSFT;
     PXrHandMeshVertexBufferMSFT=^TXrHandMeshVertexBufferMSFT;
     TXrHandMeshVertexBufferMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       vertexUpdateTime:TXrTime;
       vertexCapacityInput:TXrUInt32;
       vertexCountOutput:TXrUInt32;
       vertices:PXrHandMeshVertexMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVertexUpdateTime:TXrTime;
                          const aVertexCapacityInput:TXrUInt32;
                          const aVertexCountOutput:TXrUInt32;
                          const aVertices:PXrHandMeshVertexMSFT);
{$endif}
     end;

     PPXrHandMeshMSFT=^PXrHandMeshMSFT;
     PXrHandMeshMSFT=^TXrHandMeshMSFT;
     TXrHandMeshMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       isActive:TXrBool32;
       indexBufferChanged:TXrBool32;
       vertexBufferChanged:TXrBool32;
       indexBuffer:TXrHandMeshIndexBufferMSFT;
       vertexBuffer:TXrHandMeshVertexBufferMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aIsActive:TXrBool32;
                          const aIndexBufferChanged:TXrBool32;
                          const aVertexBufferChanged:TXrBool32;
                          const aIndexBuffer:TXrHandMeshIndexBufferMSFT;
                          const aVertexBuffer:TXrHandMeshVertexBufferMSFT);
{$endif}
     end;

     PPXrSystemHandTrackingMeshPropertiesMSFT=^PXrSystemHandTrackingMeshPropertiesMSFT;
     PXrSystemHandTrackingMeshPropertiesMSFT=^TXrSystemHandTrackingMeshPropertiesMSFT;
     TXrSystemHandTrackingMeshPropertiesMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       supportsHandTrackingMesh:TXrBool32;
       maxHandMeshIndexCount:TXrUInt32;
       maxHandMeshVertexCount:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSupportsHandTrackingMesh:TXrBool32;
                          const aMaxHandMeshIndexCount:TXrUInt32;
                          const aMaxHandMeshVertexCount:TXrUInt32);
{$endif}
     end;

     PPXrHandPoseTypeInfoMSFT=^PXrHandPoseTypeInfoMSFT;
     PXrHandPoseTypeInfoMSFT=^TXrHandPoseTypeInfoMSFT;
     TXrHandPoseTypeInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid; //< Pointer to next structure
       handPoseType:TXrHandPoseTypeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aHandPoseType:TXrHandPoseTypeMSFT);
{$endif}
     end;

     PPXrSecondaryViewConfigurationSessionBeginInfoMSFT=^PXrSecondaryViewConfigurationSessionBeginInfoMSFT;
     PXrSecondaryViewConfigurationSessionBeginInfoMSFT=^TXrSecondaryViewConfigurationSessionBeginInfoMSFT;
     TXrSecondaryViewConfigurationSessionBeginInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationCount:TXrUInt32;
       enabledViewConfigurationTypes:PXrViewConfigurationType;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationCount:TXrUInt32;
                          const aEnabledViewConfigurationTypes:PXrViewConfigurationType);
{$endif}
     end;

     PPXrSecondaryViewConfigurationStateMSFT=^PXrSecondaryViewConfigurationStateMSFT;
     PXrSecondaryViewConfigurationStateMSFT=^TXrSecondaryViewConfigurationStateMSFT;
     TXrSecondaryViewConfigurationStateMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationType:TXrViewConfigurationType;
       active:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationType:TXrViewConfigurationType;
                          const aActive:TXrBool32);
{$endif}
     end;

     PPXrSecondaryViewConfigurationFrameStateMSFT=^PXrSecondaryViewConfigurationFrameStateMSFT;
     PXrSecondaryViewConfigurationFrameStateMSFT=^TXrSecondaryViewConfigurationFrameStateMSFT;
     TXrSecondaryViewConfigurationFrameStateMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationCount:TXrUInt32;
       viewConfigurationStates:PXrSecondaryViewConfigurationStateMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationCount:TXrUInt32;
                          const aViewConfigurationStates:PXrSecondaryViewConfigurationStateMSFT);
{$endif}
     end;

     PPXrSecondaryViewConfigurationLayerInfoMSFT=^PXrSecondaryViewConfigurationLayerInfoMSFT;
     PXrSecondaryViewConfigurationLayerInfoMSFT=^TXrSecondaryViewConfigurationLayerInfoMSFT;
     TXrSecondaryViewConfigurationLayerInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationType:TXrViewConfigurationType;
       environmentBlendMode:TXrEnvironmentBlendMode;
       layerCount:TXrUInt32;
       layers:PPXrCompositionLayerBaseHeader;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationType:TXrViewConfigurationType;
                          const aEnvironmentBlendMode:TXrEnvironmentBlendMode;
                          const aLayerCount:TXrUInt32;
                          const aLayers:PPXrCompositionLayerBaseHeader);
{$endif}
     end;

     PPXrSecondaryViewConfigurationFrameEndInfoMSFT=^PXrSecondaryViewConfigurationFrameEndInfoMSFT;
     PXrSecondaryViewConfigurationFrameEndInfoMSFT=^TXrSecondaryViewConfigurationFrameEndInfoMSFT;
     TXrSecondaryViewConfigurationFrameEndInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationCount:TXrUInt32;
       viewConfigurationLayersInfo:PXrSecondaryViewConfigurationLayerInfoMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationCount:TXrUInt32;
                          const aViewConfigurationLayersInfo:PXrSecondaryViewConfigurationLayerInfoMSFT);
{$endif}
     end;

     PPXrSecondaryViewConfigurationSwapchainCreateInfoMSFT=^PXrSecondaryViewConfigurationSwapchainCreateInfoMSFT;
     PXrSecondaryViewConfigurationSwapchainCreateInfoMSFT=^TXrSecondaryViewConfigurationSwapchainCreateInfoMSFT;
     TXrSecondaryViewConfigurationSwapchainCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       viewConfigurationType:TXrViewConfigurationType;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aViewConfigurationType:TXrViewConfigurationType);
{$endif}
     end;

     PPXrHolographicWindowAttachmentMSFT=^PXrHolographicWindowAttachmentMSFT;
     PXrHolographicWindowAttachmentMSFT=^TXrHolographicWindowAttachmentMSFT;
     TXrHolographicWindowAttachmentMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       holographicSpace:IUnknown;
       coreWindow:IUnknown;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aHolographicSpace:IUnknown;
                          const aCoreWindow:IUnknown);
{$endif}
     end;

{$ifdef Android}
     PPXrAndroidSurfaceSwapchainCreateInfoFB=^PXrAndroidSurfaceSwapchainCreateInfoFB;
     PXrAndroidSurfaceSwapchainCreateInfoFB=^TXrAndroidSurfaceSwapchainCreateInfoFB;
     TXrAndroidSurfaceSwapchainCreateInfoFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       createFlags:TXrAndroidSurfaceSwapchainFlagsFB;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCreateFlags:TXrAndroidSurfaceSwapchainFlagsFB);
{$endif}
     end;
{$endif}

     PPXrSwapchainStateBaseHeaderFB=^PXrSwapchainStateBaseHeaderFB;
     PXrSwapchainStateBaseHeaderFB=^TXrSwapchainStateBaseHeaderFB;
     TXrSwapchainStateBaseHeaderFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

{$ifdef Android}
     PPXrSwapchainStateAndroidSurfaceDimensionsFB=^PXrSwapchainStateAndroidSurfaceDimensionsFB;
     PXrSwapchainStateAndroidSurfaceDimensionsFB=^TXrSwapchainStateAndroidSurfaceDimensionsFB;
     TXrSwapchainStateAndroidSurfaceDimensionsFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       width:TXrUInt32;
       height:TXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aWidth:TXrUInt32;
                          const aHeight:TXrUInt32);
{$endif}
     end;
{$endif}

{$ifdef EGL}
     PPXrSwapchainStateSamplerOpenGLESFB=^PXrSwapchainStateSamplerOpenGLESFB;
     PXrSwapchainStateSamplerOpenGLESFB=^TXrSwapchainStateSamplerOpenGLESFB;
     TXrSwapchainStateSamplerOpenGLESFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       minFilter:TEGLenum;
       magFilter:TEGLenum;
       wrapModeS:TEGLenum;
       wrapModeT:TEGLenum;
       swizzleRed:TEGLenum;
       swizzleGreen:TEGLenum;
       swizzleBlue:TEGLenum;
       swizzleAlpha:TEGLenum;
       maxAnisotropy:TXrFloat;
       borderColor:TXrColor4f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMinFilter:TEGLenum;
                          const aMagFilter:TEGLenum;
                          const aWrapModeS:TEGLenum;
                          const aWrapModeT:TEGLenum;
                          const aSwizzleRed:TEGLenum;
                          const aSwizzleGreen:TEGLenum;
                          const aSwizzleBlue:TEGLenum;
                          const aSwizzleAlpha:TEGLenum;
                          const aMaxAnisotropy:TXrFloat;
                          const aBorderColor:TXrColor4f);
{$endif}
     end;
{$endif}

     PPXrSwapchainStateSamplerVulkanFB=^PXrSwapchainStateSamplerVulkanFB;
     PXrSwapchainStateSamplerVulkanFB=^TXrSwapchainStateSamplerVulkanFB;
     TXrSwapchainStateSamplerVulkanFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       minFilter:TVkFilter;
       magFilter:TVkFilter;
       mipmapMode:TVkSamplerMipmapMode;
       wrapModeS:TVkSamplerAddressMode;
       wrapModeT:TVkSamplerAddressMode;
       swizzleRed:TVkComponentSwizzle;
       swizzleGreen:TVkComponentSwizzle;
       swizzleBlue:TVkComponentSwizzle;
       swizzleAlpha:TVkComponentSwizzle;
       maxAnisotropy:TXrFloat;
       borderColor:TXrColor4f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMinFilter:TVkFilter;
                          const aMagFilter:TVkFilter;
                          const aMipmapMode:TVkSamplerMipmapMode;
                          const aWrapModeS:TVkSamplerAddressMode;
                          const aWrapModeT:TVkSamplerAddressMode;
                          const aSwizzleRed:TVkComponentSwizzle;
                          const aSwizzleGreen:TVkComponentSwizzle;
                          const aSwizzleBlue:TVkComponentSwizzle;
                          const aSwizzleAlpha:TVkComponentSwizzle;
                          const aMaxAnisotropy:TXrFloat;
                          const aBorderColor:TXrColor4f);
{$endif}
     end;

     PPXrLoaderInitInfoBaseHeaderKHR=^PXrLoaderInitInfoBaseHeaderKHR;
     PXrLoaderInitInfoBaseHeaderKHR=^TXrLoaderInitInfoBaseHeaderKHR;
     TXrLoaderInitInfoBaseHeaderKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

{$ifdef Android}
     PPXrLoaderInitInfoAndroidKHR=^PXrLoaderInitInfoAndroidKHR;
     PXrLoaderInitInfoAndroidKHR=^TXrLoaderInitInfoAndroidKHR;
     TXrLoaderInitInfoAndroidKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       applicationVM:PXrVoid;
       applicationContext:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aApplicationVM:PXrVoid;
                          const aApplicationContext:PXrVoid);
{$endif}
     end;
{$endif}

     PPXrCompositionLayerEquirect2KHR=^PXrCompositionLayerEquirect2KHR;
     PXrCompositionLayerEquirect2KHR=^TXrCompositionLayerEquirect2KHR;
     TXrCompositionLayerEquirect2KHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       layerFlags:TXrCompositionLayerFlags;
       space:TXrSpace;
       eyeVisibility:TXrEyeVisibility;
       subImage:TXrSwapchainSubImage;
       pose:TXrPosef;
       radius:TXrFloat;
       centralHorizontalAngle:TXrFloat;
       upperVerticalAngle:TXrFloat;
       lowerVerticalAngle:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLayerFlags:TXrCompositionLayerFlags;
                          const aSpace:TXrSpace;
                          const aEyeVisibility:TXrEyeVisibility;
                          const aSubImage:TXrSwapchainSubImage;
                          const aPose:TXrPosef;
                          const aRadius:TXrFloat;
                          const aCentralHorizontalAngle:TXrFloat;
                          const aUpperVerticalAngle:TXrFloat;
                          const aLowerVerticalAngle:TXrFloat);
{$endif}
     end;

     PPXrCompositionLayerColorScaleBiasKHR=^PXrCompositionLayerColorScaleBiasKHR;
     PXrCompositionLayerColorScaleBiasKHR=^TXrCompositionLayerColorScaleBiasKHR;
     TXrCompositionLayerColorScaleBiasKHR=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       colorScale:TXrColor4f;
       colorBias:TXrColor4f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aColorScale:TXrColor4f;
                          const aColorBias:TXrColor4f);
{$endif}
     end;

     PPXrControllerModelKeyStateMSFT=^PXrControllerModelKeyStateMSFT;
     PXrControllerModelKeyStateMSFT=^TXrControllerModelKeyStateMSFT;
     TXrControllerModelKeyStateMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       modelKey:TXrControllerModelKeyMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aModelKey:TXrControllerModelKeyMSFT);
{$endif}
     end;

     PPXrControllerModelNodePropertiesMSFT=^PXrControllerModelNodePropertiesMSFT;
     PXrControllerModelNodePropertiesMSFT=^TXrControllerModelNodePropertiesMSFT;
     TXrControllerModelNodePropertiesMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       parentNodeName:array[0..XR_MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT-1] of TXrChar;
       nodeName:array[0..XR_MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT-1] of TXrChar;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aParentNodeName:TXrCharString;
                          const aNodeName:TXrCharString);
{$endif}
     end;

     PPXrControllerModelPropertiesMSFT=^PXrControllerModelPropertiesMSFT;
     PXrControllerModelPropertiesMSFT=^TXrControllerModelPropertiesMSFT;
     TXrControllerModelPropertiesMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       nodeCapacityInput:TXrUInt32;
       nodeCountOutput:TXrUInt32;
       nodeProperties:PXrControllerModelNodePropertiesMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aNodeCapacityInput:TXrUInt32;
                          const aNodeCountOutput:TXrUInt32;
                          const aNodeProperties:PXrControllerModelNodePropertiesMSFT);
{$endif}
     end;

     PPXrControllerModelNodeStateMSFT=^PXrControllerModelNodeStateMSFT;
     PXrControllerModelNodeStateMSFT=^TXrControllerModelNodeStateMSFT;
     TXrControllerModelNodeStateMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       nodePose:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aNodePose:TXrPosef);
{$endif}
     end;

     PPXrControllerModelStateMSFT=^PXrControllerModelStateMSFT;
     PXrControllerModelStateMSFT=^TXrControllerModelStateMSFT;
     TXrControllerModelStateMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       nodeCapacityInput:TXrUInt32;
       nodeCountOutput:TXrUInt32;
       nodeStates:PXrControllerModelNodeStateMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aNodeCapacityInput:TXrUInt32;
                          const aNodeCountOutput:TXrUInt32;
                          const aNodeStates:PXrControllerModelNodeStateMSFT);
{$endif}
     end;

     PPXrUuidMSFT=^PXrUuidMSFT;
     PXrUuidMSFT=^TXrUuidMSFT;
     TXrUuidMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       bytes:array[0..15] of TXrUInt8;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aBytes:array of TXrUInt8);
{$endif}
     end;

     PPXrSceneObserverCreateInfoMSFT=^PXrSceneObserverCreateInfoMSFT;
     PXrSceneObserverCreateInfoMSFT=^TXrSceneObserverCreateInfoMSFT;
     TXrSceneObserverCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrSceneCreateInfoMSFT=^PXrSceneCreateInfoMSFT;
     PXrSceneCreateInfoMSFT=^TXrSceneCreateInfoMSFT;
     TXrSceneCreateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrSceneSphereBoundMSFT=^PXrSceneSphereBoundMSFT;
     PXrSceneSphereBoundMSFT=^TXrSceneSphereBoundMSFT;
     TXrSceneSphereBoundMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       center:TXrVector3f;
       radius:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aCenter:TXrVector3f;
                          const aRadius:TXrFloat);
{$endif}
     end;

     PPXrVisualMeshComputeLodInfoMSFT=^PXrVisualMeshComputeLodInfoMSFT;
     PXrVisualMeshComputeLodInfoMSFT=^TXrVisualMeshComputeLodInfoMSFT;
     TXrVisualMeshComputeLodInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       lod:TXrMeshComputeLodMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLod:TXrMeshComputeLodMSFT);
{$endif}
     end;

     PPXrSceneOrientedBoxBoundMSFT=^PXrSceneOrientedBoxBoundMSFT;
     PXrSceneOrientedBoxBoundMSFT=^TXrSceneOrientedBoxBoundMSFT;
     TXrSceneOrientedBoxBoundMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       pose:TXrPosef;
       extents:TXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPose:TXrPosef;
                          const aExtents:TXrVector3f);
{$endif}
     end;

     PPXrSceneFrustumBoundMSFT=^PXrSceneFrustumBoundMSFT;
     PXrSceneFrustumBoundMSFT=^TXrSceneFrustumBoundMSFT;
     TXrSceneFrustumBoundMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       pose:TXrPosef;
       fov:TXrFovf;
       farDistance:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPose:TXrPosef;
                          const aFov:TXrFovf;
                          const aFarDistance:TXrFloat);
{$endif}
     end;

     PPXrSceneBoundsMSFT=^PXrSceneBoundsMSFT;
     PXrSceneBoundsMSFT=^TXrSceneBoundsMSFT;
     TXrSceneBoundsMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       space:TXrSpace;
       time:TXrTime;
       sphereCount:TXrUInt32;
       spheres:PXrSceneSphereBoundMSFT;
       boxCount:TXrUInt32;
       boxes:PXrSceneOrientedBoxBoundMSFT;
       frustumCount:TXrUInt32;
       frustums:PXrSceneFrustumBoundMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSpace:TXrSpace;
                          const aTime:TXrTime;
                          const aSphereCount:TXrUInt32;
                          const aSpheres:PXrSceneSphereBoundMSFT;
                          const aBoxCount:TXrUInt32;
                          const aBoxes:PXrSceneOrientedBoxBoundMSFT;
                          const aFrustumCount:TXrUInt32;
                          const aFrustums:PXrSceneFrustumBoundMSFT);
{$endif}
     end;

     PPXrNewSceneComputeInfoMSFT=^PXrNewSceneComputeInfoMSFT;
     PXrNewSceneComputeInfoMSFT=^TXrNewSceneComputeInfoMSFT;
     TXrNewSceneComputeInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       requestedFeatureCount:TXrUInt32;
       requestedFeatures:PXrSceneComputeFeatureMSFT;
       consistency:TXrSceneComputeConsistencyMSFT;
       bounds:TXrSceneBoundsMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aRequestedFeatureCount:TXrUInt32;
                          const aRequestedFeatures:PXrSceneComputeFeatureMSFT;
                          const aConsistency:TXrSceneComputeConsistencyMSFT;
                          const aBounds:TXrSceneBoundsMSFT);
{$endif}
     end;

     PPXrSceneComponentMSFT=^PXrSceneComponentMSFT;
     PXrSceneComponentMSFT=^TXrSceneComponentMSFT;
     TXrSceneComponentMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       componentType:TXrSceneComponentTypeMSFT;
       id:TXrUuidMSFT;
       parentId:TXrUuidMSFT;
       updateTime:TXrTime;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aComponentType:TXrSceneComponentTypeMSFT;
                          const aId:TXrUuidMSFT;
                          const aParentId:TXrUuidMSFT;
                          const aUpdateTime:TXrTime);
{$endif}
     end;

     PPXrSceneComponentsMSFT=^PXrSceneComponentsMSFT;
     PXrSceneComponentsMSFT=^TXrSceneComponentsMSFT;
     TXrSceneComponentsMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       componentCapacityInput:TXrUInt32;
       componentCountOutput:TXrUInt32;
       components:PXrSceneComponentMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aComponentCapacityInput:TXrUInt32;
                          const aComponentCountOutput:TXrUInt32;
                          const aComponents:PXrSceneComponentMSFT);
{$endif}
     end;

     PPXrSceneComponentsGetInfoMSFT=^PXrSceneComponentsGetInfoMSFT;
     PXrSceneComponentsGetInfoMSFT=^TXrSceneComponentsGetInfoMSFT;
     TXrSceneComponentsGetInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       componentType:TXrSceneComponentTypeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aComponentType:TXrSceneComponentTypeMSFT);
{$endif}
     end;

     PPXrSceneComponentLocationMSFT=^PXrSceneComponentLocationMSFT;
     PXrSceneComponentLocationMSFT=^TXrSceneComponentLocationMSFT;
     TXrSceneComponentLocationMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       flags:TXrSpaceLocationFlags;
       pose:TXrPosef;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aFlags:TXrSpaceLocationFlags;
                          const aPose:TXrPosef);
{$endif}
     end;

     PPXrSceneComponentLocationsMSFT=^PXrSceneComponentLocationsMSFT;
     PXrSceneComponentLocationsMSFT=^TXrSceneComponentLocationsMSFT;
     TXrSceneComponentLocationsMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       locationCount:TXrUInt32;
       locations:PXrSceneComponentLocationMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aLocationCount:TXrUInt32;
                          const aLocations:PXrSceneComponentLocationMSFT);
{$endif}
     end;

     PPXrSceneComponentsLocateInfoMSFT=^PXrSceneComponentsLocateInfoMSFT;
     PXrSceneComponentsLocateInfoMSFT=^TXrSceneComponentsLocateInfoMSFT;
     TXrSceneComponentsLocateInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       baseSpace:TXrSpace;
       time:TXrTime;
       componentIdCount:TXrUInt32;
       componentIds:PXrUuidMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aBaseSpace:TXrSpace;
                          const aTime:TXrTime;
                          const aComponentIdCount:TXrUInt32;
                          const aComponentIds:PXrUuidMSFT);
{$endif}
     end;

     PPXrSceneObjectMSFT=^PXrSceneObjectMSFT;
     PXrSceneObjectMSFT=^TXrSceneObjectMSFT;
     TXrSceneObjectMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       objectType:TXrSceneObjectTypeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aObjectType:TXrSceneObjectTypeMSFT);
{$endif}
     end;

     PPXrSceneObjectsMSFT=^PXrSceneObjectsMSFT;
     PXrSceneObjectsMSFT=^TXrSceneObjectsMSFT;
     TXrSceneObjectsMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       sceneObjectCount:TXrUInt32;
       sceneObjects:PXrSceneObjectMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSceneObjectCount:TXrUInt32;
                          const aSceneObjects:PXrSceneObjectMSFT);
{$endif}
     end;

     PPXrSceneComponentParentFilterInfoMSFT=^PXrSceneComponentParentFilterInfoMSFT;
     PXrSceneComponentParentFilterInfoMSFT=^TXrSceneComponentParentFilterInfoMSFT;
     TXrSceneComponentParentFilterInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       parentId:TXrUuidMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aParentId:TXrUuidMSFT);
{$endif}
     end;

     PPXrSceneObjectTypesFilterInfoMSFT=^PXrSceneObjectTypesFilterInfoMSFT;
     PXrSceneObjectTypesFilterInfoMSFT=^TXrSceneObjectTypesFilterInfoMSFT;
     TXrSceneObjectTypesFilterInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       objectTypeCount:TXrUInt32;
       objectTypes:PXrSceneObjectTypeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aObjectTypeCount:TXrUInt32;
                          const aObjectTypes:PXrSceneObjectTypeMSFT);
{$endif}
     end;

     PPXrScenePlaneMSFT=^PXrScenePlaneMSFT;
     PXrScenePlaneMSFT=^TXrScenePlaneMSFT;
     TXrScenePlaneMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       alignment:TXrScenePlaneAlignmentTypeMSFT;
       size:TXrExtent2Df;
       meshBufferId:TXrUInt64;
       supportsIndicesUint16:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAlignment:TXrScenePlaneAlignmentTypeMSFT;
                          const aSize:TXrExtent2Df;
                          const aMeshBufferId:TXrUInt64;
                          const aSupportsIndicesUint16:TXrBool32);
{$endif}
     end;

     PPXrScenePlanesMSFT=^PXrScenePlanesMSFT;
     PXrScenePlanesMSFT=^TXrScenePlanesMSFT;
     TXrScenePlanesMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       scenePlaneCount:TXrUInt32;
       scenePlanes:PXrScenePlaneMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aScenePlaneCount:TXrUInt32;
                          const aScenePlanes:PXrScenePlaneMSFT);
{$endif}
     end;

     PPXrScenePlaneAlignmentFilterInfoMSFT=^PXrScenePlaneAlignmentFilterInfoMSFT;
     PXrScenePlaneAlignmentFilterInfoMSFT=^TXrScenePlaneAlignmentFilterInfoMSFT;
     TXrScenePlaneAlignmentFilterInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       alignmentCount:TXrUInt32;
       alignments:PXrScenePlaneAlignmentTypeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aAlignmentCount:TXrUInt32;
                          const aAlignments:PXrScenePlaneAlignmentTypeMSFT);
{$endif}
     end;

     PPXrSceneMeshMSFT=^PXrSceneMeshMSFT;
     PXrSceneMeshMSFT=^TXrSceneMeshMSFT;
     TXrSceneMeshMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       meshBufferId:TXrUInt64;
       supportsIndicesUint16:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMeshBufferId:TXrUInt64;
                          const aSupportsIndicesUint16:TXrBool32);
{$endif}
     end;

     PPXrSceneMeshesMSFT=^PXrSceneMeshesMSFT;
     PXrSceneMeshesMSFT=^TXrSceneMeshesMSFT;
     TXrSceneMeshesMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       sceneMeshCount:TXrUInt32;
       sceneMeshes:PXrSceneMeshMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSceneMeshCount:TXrUInt32;
                          const aSceneMeshes:PXrSceneMeshMSFT);
{$endif}
     end;

     PPXrSceneMeshBuffersGetInfoMSFT=^PXrSceneMeshBuffersGetInfoMSFT;
     PXrSceneMeshBuffersGetInfoMSFT=^TXrSceneMeshBuffersGetInfoMSFT;
     TXrSceneMeshBuffersGetInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       meshBufferId:TXrUInt64;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aMeshBufferId:TXrUInt64);
{$endif}
     end;

     PPXrSceneMeshBuffersMSFT=^PXrSceneMeshBuffersMSFT;
     PXrSceneMeshBuffersMSFT=^TXrSceneMeshBuffersMSFT;
     TXrSceneMeshBuffersMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
{$ifdef HAS_ADVANCED_RECORDS}
{$endif}
     end;

     PPXrSceneMeshVertexBufferMSFT=^PXrSceneMeshVertexBufferMSFT;
     PXrSceneMeshVertexBufferMSFT=^TXrSceneMeshVertexBufferMSFT;
     TXrSceneMeshVertexBufferMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       vertexCapacityInput:TXrUInt32;
       vertexCountOutput:TXrUInt32;
       vertices:PXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aVertexCapacityInput:TXrUInt32;
                          const aVertexCountOutput:TXrUInt32;
                          const aVertices:PXrVector3f);
{$endif}
     end;

     PPXrSceneMeshIndicesUint32MSFT=^PXrSceneMeshIndicesUint32MSFT;
     PXrSceneMeshIndicesUint32MSFT=^TXrSceneMeshIndicesUint32MSFT;
     TXrSceneMeshIndicesUint32MSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       indexCapacityInput:TXrUInt32;
       indexCountOutput:TXrUInt32;
       indices:PXrUInt32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aIndexCapacityInput:TXrUInt32;
                          const aIndexCountOutput:TXrUInt32;
                          const aIndices:PXrUInt32);
{$endif}
     end;

     PPXrSceneMeshIndicesUint16MSFT=^PXrSceneMeshIndicesUint16MSFT;
     PXrSceneMeshIndicesUint16MSFT=^TXrSceneMeshIndicesUint16MSFT;
     TXrSceneMeshIndicesUint16MSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       indexCapacityInput:TXrUInt32;
       indexCountOutput:TXrUInt32;
       indices:PXrUInt16;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aIndexCapacityInput:TXrUInt32;
                          const aIndexCountOutput:TXrUInt32;
                          const aIndices:PXrUInt16);
{$endif}
     end;

     PPXrSerializedSceneFragmentDataGetInfoMSFT=^PXrSerializedSceneFragmentDataGetInfoMSFT;
     PXrSerializedSceneFragmentDataGetInfoMSFT=^TXrSerializedSceneFragmentDataGetInfoMSFT;
     TXrSerializedSceneFragmentDataGetInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       sceneFragmentId:TXrUuidMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSceneFragmentId:TXrUuidMSFT);
{$endif}
     end;

     PPXrDeserializeSceneFragmentMSFT=^PXrDeserializeSceneFragmentMSFT;
     PXrDeserializeSceneFragmentMSFT=^TXrDeserializeSceneFragmentMSFT;
     TXrDeserializeSceneFragmentMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       bufferSize:TXrUInt32;
       buffer:PXrUInt8;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aBufferSize:TXrUInt32;
                          const aBuffer:PXrUInt8);
{$endif}
     end;

     PPXrSceneDeserializeInfoMSFT=^PXrSceneDeserializeInfoMSFT;
     PXrSceneDeserializeInfoMSFT=^TXrSceneDeserializeInfoMSFT;
     TXrSceneDeserializeInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       fragmentCount:TXrUInt32;
       fragments:PXrDeserializeSceneFragmentMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aFragmentCount:TXrUInt32;
                          const aFragments:PXrDeserializeSceneFragmentMSFT);
{$endif}
     end;

     PPXrSystemColorSpacePropertiesFB=^PXrSystemColorSpacePropertiesFB;
     PXrSystemColorSpacePropertiesFB=^TXrSystemColorSpacePropertiesFB;
     TXrSystemColorSpacePropertiesFB=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       colorSpace:TXrColorSpaceFB;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aColorSpace:TXrColorSpaceFB);
{$endif}
     end;

     PPXrCompositionLayerDepthTestVARJO=^PXrCompositionLayerDepthTestVARJO;
     PXrCompositionLayerDepthTestVARJO=^TXrCompositionLayerDepthTestVARJO;
     TXrCompositionLayerDepthTestVARJO=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       depthTestRangeNearZ:TXrFloat;
       depthTestRangeFarZ:TXrFloat;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aDepthTestRangeNearZ:TXrFloat;
                          const aDepthTestRangeFarZ:TXrFloat);
{$endif}
     end;

     PPXrViewLocateFoveatedRenderingVARJO=^PXrViewLocateFoveatedRenderingVARJO;
     PXrViewLocateFoveatedRenderingVARJO=^TXrViewLocateFoveatedRenderingVARJO;
     TXrViewLocateFoveatedRenderingVARJO=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       foveatedRenderingActive:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aFoveatedRenderingActive:TXrBool32);
{$endif}
     end;

     PPXrFoveatedViewConfigurationViewVARJO=^PXrFoveatedViewConfigurationViewVARJO;
     PXrFoveatedViewConfigurationViewVARJO=^TXrFoveatedViewConfigurationViewVARJO;
     TXrFoveatedViewConfigurationViewVARJO=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       foveatedRenderingActive:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aFoveatedRenderingActive:TXrBool32);
{$endif}
     end;

     PPXrSystemFoveatedRenderingPropertiesVARJO=^PXrSystemFoveatedRenderingPropertiesVARJO;
     PXrSystemFoveatedRenderingPropertiesVARJO=^TXrSystemFoveatedRenderingPropertiesVARJO;
     TXrSystemFoveatedRenderingPropertiesVARJO=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       supportsFoveatedRendering:TXrBool32;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aSupportsFoveatedRendering:TXrBool32);
{$endif}
     end;

     PPXrCompositionLayerReprojectionInfoMSFT=^PXrCompositionLayerReprojectionInfoMSFT;
     PXrCompositionLayerReprojectionInfoMSFT=^TXrCompositionLayerReprojectionInfoMSFT;
     TXrCompositionLayerReprojectionInfoMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       reprojectionMode:TXrReprojectionModeMSFT;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aReprojectionMode:TXrReprojectionModeMSFT);
{$endif}
     end;

     PPXrCompositionLayerReprojectionPlaneOverrideMSFT=^PXrCompositionLayerReprojectionPlaneOverrideMSFT;
     PXrCompositionLayerReprojectionPlaneOverrideMSFT=^TXrCompositionLayerReprojectionPlaneOverrideMSFT;
     TXrCompositionLayerReprojectionPlaneOverrideMSFT=record
{$ifdef HAS_ADVANCED_RECORDS}
      public
{$endif}
       type_:TXrStructureType;
       next:PXrVoid;
       position:TXrVector3f;
       normal:TXrVector3f;
       velocity:TXrVector3f;
{$ifdef HAS_ADVANCED_RECORDS}
       constructor Create(const aPosition:TXrVector3f;
                          const aNormal:TXrVector3f;
                          const aVelocity:TXrVector3f);
{$endif}
     end;

     TxrGetInstanceProcAddr=function(instance:TXrInstance;const name:PXrChar;_function:PPFN_xrVoidFunction):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateApiLayerProperties=function(propertyCapacityInput:TXrUInt32;propertyCountOutput:PXrUInt32;properties:PXrApiLayerProperties):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateInstanceExtensionProperties=function(const layerName:PXrChar;propertyCapacityInput:TXrUInt32;propertyCountOutput:PXrUInt32;properties:PXrExtensionProperties):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateInstance=function(const createInfo:PXrInstanceCreateInfo;instance:PXrInstance):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroyInstance=function(instance:TXrInstance):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrResultToString=function(instance:TXrInstance;value:TXrResult;buffer:TXrChar):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrStructureTypeToString=function(instance:TXrInstance;value:TXrStructureType;buffer:TXrChar):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetInstanceProperties=function(instance:TXrInstance;instanceProperties:PXrInstanceProperties):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSystem=function(instance:TXrInstance;const getInfo:PXrSystemGetInfo;systemId:PXrSystemId):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSystemProperties=function(instance:TXrInstance;systemId:TXrSystemId;properties:PXrSystemProperties):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSession=function(instance:TXrInstance;const createInfo:PXrSessionCreateInfo;session:PXrSession):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroySession=function(session:TXrSession):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroySpace=function(space:TXrSpace):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateSwapchainFormats=function(session:TXrSession;formatCapacityInput:TXrUInt32;formatCountOutput:PXrUInt32;formats:PXrInt64):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSwapchain=function(session:TXrSession;const createInfo:PXrSwapchainCreateInfo;swapchain:PXrSwapchain):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroySwapchain=function(swapchain:TXrSwapchain):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateSwapchainImages=function(swapchain:TXrSwapchain;imageCapacityInput:TXrUInt32;imageCountOutput:PXrUInt32;images:PXrSwapchainImageBaseHeader):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrAcquireSwapchainImage=function(swapchain:TXrSwapchain;const acquireInfo:PXrSwapchainImageAcquireInfo;index:PXrUInt32):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrWaitSwapchainImage=function(swapchain:TXrSwapchain;const waitInfo:PXrSwapchainImageWaitInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrReleaseSwapchainImage=function(swapchain:TXrSwapchain;const releaseInfo:PXrSwapchainImageReleaseInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrBeginSession=function(session:TXrSession;const beginInfo:PXrSessionBeginInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEndSession=function(session:TXrSession):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrRequestExitSession=function(session:TXrSession):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateReferenceSpaces=function(session:TXrSession;spaceCapacityInput:TXrUInt32;spaceCountOutput:PXrUInt32;spaces:PXrReferenceSpaceType):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateReferenceSpace=function(session:TXrSession;const createInfo:PXrReferenceSpaceCreateInfo;space:PXrSpace):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateActionSpace=function(session:TXrSession;const createInfo:PXrActionSpaceCreateInfo;space:PXrSpace):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrLocateSpace=function(space:TXrSpace;baseSpace:TXrSpace;time:TXrTime;location:PXrSpaceLocation):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateViewConfigurations=function(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationTypeCapacityInput:TXrUInt32;viewConfigurationTypeCountOutput:PXrUInt32;viewConfigurationTypes:PXrViewConfigurationType):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateEnvironmentBlendModes=function(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;environmentBlendModeCapacityInput:TXrUInt32;environmentBlendModeCountOutput:PXrUInt32;environmentBlendModes:PXrEnvironmentBlendMode):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetViewConfigurationProperties=function(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;configurationProperties:PXrViewConfigurationProperties):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateViewConfigurationViews=function(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;viewCapacityInput:TXrUInt32;viewCountOutput:PXrUInt32;views:PXrViewConfigurationView):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrBeginFrame=function(session:TXrSession;const frameBeginInfo:PXrFrameBeginInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrLocateViews=function(session:TXrSession;const viewLocateInfo:PXrViewLocateInfo;viewState:PXrViewState;viewCapacityInput:TXrUInt32;viewCountOutput:PXrUInt32;views:PXrView):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEndFrame=function(session:TXrSession;const frameEndInfo:PXrFrameEndInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrWaitFrame=function(session:TXrSession;const frameWaitInfo:PXrFrameWaitInfo;frameState:PXrFrameState):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrApplyHapticFeedback=function(session:TXrSession;const hapticActionInfo:PXrHapticActionInfo;const hapticFeedback:PXrHapticBaseHeader):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrStopHapticFeedback=function(session:TXrSession;const hapticActionInfo:PXrHapticActionInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrPollEvent=function(instance:TXrInstance;eventData:PXrEventDataBuffer):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrStringToPath=function(instance:TXrInstance;const pathString:PXrChar;path:PXrPath):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrPathToString=function(instance:TXrInstance;path:TXrPath;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetReferenceSpaceBoundsRect=function(session:TXrSession;referenceSpaceType:TXrReferenceSpaceType;bounds:PXrExtent2Df):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef Android}
     TxrSetAndroidApplicationThreadKHR=function(session:TXrSession;threadType:TXrAndroidThreadTypeKHR;threadId:TXrUInt32):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef Android}
     TxrCreateSwapchainAndroidSurfaceKHR=function(session:TXrSession;const info:PXrSwapchainCreateInfo;swapchain:PXrSwapchain;surface:Pjobject):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TxrGetActionStateBoolean=function(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateBoolean):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetActionStateFloat=function(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateFloat):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetActionStateVector2f=function(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateVector2f):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetActionStatePose=function(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStatePose):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateActionSet=function(instance:TXrInstance;const createInfo:PXrActionSetCreateInfo;actionSet:PXrActionSet):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroyActionSet=function(actionSet:TXrActionSet):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateAction=function(actionSet:TXrActionSet;const createInfo:PXrActionCreateInfo;action:PXrAction):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroyAction=function(action:TXrAction):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSuggestInteractionProfileBindings=function(instance:TXrInstance;const suggestedBindings:PXrInteractionProfileSuggestedBinding):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrAttachSessionActionSets=function(session:TXrSession;const attachInfo:PXrSessionActionSetsAttachInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetCurrentInteractionProfile=function(session:TXrSession;topLevelUserPath:TXrPath;interactionProfile:PXrInteractionProfileState):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSyncActions=function(session:TXrSession;const syncInfo:PXrActionsSyncInfo):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateBoundSourcesForAction=function(session:TXrSession;const enumerateInfo:PXrBoundSourcesForActionEnumerateInfo;sourceCapacityInput:TXrUInt32;sourceCountOutput:PXrUInt32;sources:PXrPath):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetInputSourceLocalizedName=function(session:TXrSession;const getInfo:PXrInputSourceLocalizedNameGetInfo;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVulkanInstanceExtensionsKHR=function(instance:TXrInstance;systemId:TXrSystemId;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVulkanDeviceExtensionsKHR=function(instance:TXrInstance;systemId:TXrSystemId;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVulkanGraphicsDeviceKHR=function(instance:TXrInstance;systemId:TXrSystemId;vkInstance:TVkInstance;vkPhysicalDevice:PVkPhysicalDevice):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetOpenGLGraphicsRequirementsKHR=function(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsOpenGLKHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetOpenGLESGraphicsRequirementsKHR=function(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsOpenGLESKHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVulkanGraphicsRequirementsKHR=function(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsVulkanKHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetD3D11GraphicsRequirementsKHR=function(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsD3D11KHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetD3D12GraphicsRequirementsKHR=function(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsD3D12KHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrPerfSettingsSetPerformanceLevelEXT=function(session:TXrSession;domain:TXrPerfSettingsDomainEXT;level:TXrPerfSettingsLevelEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrThermalGetTemperatureTrendEXT=function(session:TXrSession;domain:TXrPerfSettingsDomainEXT;notificationLevel:PXrPerfSettingsNotificationLevelEXT;tempHeadroom:PXrFloat;tempSlope:PXrFloat):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetDebugUtilsObjectNameEXT=function(instance:TXrInstance;const nameInfo:PXrDebugUtilsObjectNameInfoEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateDebugUtilsMessengerEXT=function(instance:TXrInstance;const createInfo:PXrDebugUtilsMessengerCreateInfoEXT;messenger:PXrDebugUtilsMessengerEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroyDebugUtilsMessengerEXT=function(messenger:TXrDebugUtilsMessengerEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSubmitDebugUtilsMessageEXT=function(instance:TXrInstance;messageSeverity:TXrDebugUtilsMessageSeverityFlagsEXT;messageTypes:TXrDebugUtilsMessageTypeFlagsEXT;const callbackData:PXrDebugUtilsMessengerCallbackDataEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSessionBeginDebugUtilsLabelRegionEXT=function(session:TXrSession;const labelInfo:PXrDebugUtilsLabelEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSessionEndDebugUtilsLabelRegionEXT=function(session:TXrSession):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSessionInsertDebugUtilsLabelEXT=function(session:TXrSession;const labelInfo:PXrDebugUtilsLabelEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

{$ifdef Windows}
     TxrConvertTimeToWin32PerformanceCounterKHR=function(instance:TXrInstance;time:TXrTime;performanceCounter:PLargeInteger):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

{$ifdef Windows}
     TxrConvertWin32PerformanceCounterToTimeKHR=function(instance:TXrInstance;const performanceCounter:PLargeInteger;time:PXrTime):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
{$endif}

     TxrCreateVulkanInstanceKHR=function(instance:TXrInstance;const createInfo:PXrVulkanInstanceCreateInfoKHR;vulkanInstance:PVkInstance;vulkanResult:PVkResult):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateVulkanDeviceKHR=function(instance:TXrInstance;const createInfo:PXrVulkanDeviceCreateInfoKHR;vulkanDevice:PVkDevice;vulkanResult:PVkResult):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVulkanGraphicsDevice2KHR=function(instance:TXrInstance;const getInfo:PXrVulkanGraphicsDeviceGetInfoKHR;vulkanPhysicalDevice:PVkPhysicalDevice):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVulkanGraphicsRequirements2KHR=function(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsVulkanKHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrConvertTimeToTimespecTimeKHR=function(instance:TXrInstance;time:TXrTime;timespecTime:Ptimespec):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrConvertTimespecTimeToTimeKHR=function(instance:TXrInstance;const timespecTime:Ptimespec;time:PXrTime):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetVisibilityMaskKHR=function(session:TXrSession;viewConfigurationType:TXrViewConfigurationType;viewIndex:TXrUInt32;visibilityMaskType:TXrVisibilityMaskTypeKHR;visibilityMask:PXrVisibilityMaskKHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSpatialAnchorMSFT=function(session:TXrSession;const createInfo:PXrSpatialAnchorCreateInfoMSFT;anchor:PXrSpatialAnchorMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSpatialAnchorSpaceMSFT=function(session:TXrSession;const createInfo:PXrSpatialAnchorSpaceCreateInfoMSFT;space:PXrSpace):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroySpatialAnchorMSFT=function(anchor:TXrSpatialAnchorMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetInputDeviceActiveEXT=function(session:TXrSession;interactionProfile:TXrPath;topLevelPath:TXrPath;isActive:TXrBool32):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetInputDeviceStateBoolEXT=function(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrBool32):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetInputDeviceStateFloatEXT=function(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrFloat):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetInputDeviceStateVector2fEXT=function(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrVector2f):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetInputDeviceLocationEXT=function(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;space:TXrSpace;pose:TXrPosef):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrInitializeLoaderKHR=function(const loaderInitInfo:PXrLoaderInitInfoBaseHeaderKHR):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSpatialGraphNodeSpaceMSFT=function(session:TXrSession;const createInfo:PXrSpatialGraphNodeSpaceCreateInfoMSFT;space:PXrSpace):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateHandTrackerEXT=function(session:TXrSession;const createInfo:PXrHandTrackerCreateInfoEXT;handTracker:PXrHandTrackerEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroyHandTrackerEXT=function(handTracker:TXrHandTrackerEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrLocateHandJointsEXT=function(handTracker:TXrHandTrackerEXT;const locateInfo:PXrHandJointsLocateInfoEXT;locations:PXrHandJointLocationsEXT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateHandMeshSpaceMSFT=function(handTracker:TXrHandTrackerEXT;const createInfo:PXrHandMeshSpaceCreateInfoMSFT;space:PXrSpace):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrUpdateHandMeshMSFT=function(handTracker:TXrHandTrackerEXT;const updateInfo:PXrHandMeshUpdateInfoMSFT;handMesh:PXrHandMeshMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetControllerModelKeyMSFT=function(session:TXrSession;topLevelUserPath:TXrPath;controllerModelKeyState:PXrControllerModelKeyStateMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrLoadControllerModelMSFT=function(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrUInt8):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetControllerModelPropertiesMSFT=function(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;properties:PXrControllerModelPropertiesMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetControllerModelStateMSFT=function(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;state:PXrControllerModelStateMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateSceneComputeFeaturesMSFT=function(instance:TXrInstance;systemId:TXrSystemId;featureCapacityInput:TXrUInt32;featureCountOutput:PXrUInt32;features:PXrSceneComputeFeatureMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSceneObserverMSFT=function(session:TXrSession;const createInfo:PXrSceneObserverCreateInfoMSFT;sceneObserver:PXrSceneObserverMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroySceneObserverMSFT=function(sceneObserver:TXrSceneObserverMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSceneMSFT=function(sceneObserver:TXrSceneObserverMSFT;const createInfo:PXrSceneCreateInfoMSFT;scene:PXrSceneMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDestroySceneMSFT=function(scene:TXrSceneMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrComputeNewSceneMSFT=function(sceneObserver:TXrSceneObserverMSFT;const computeInfo:PXrNewSceneComputeInfoMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSceneComputeStateMSFT=function(sceneObserver:TXrSceneObserverMSFT;state:PXrSceneComputeStateMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSceneComponentsMSFT=function(scene:TXrSceneMSFT;const getInfo:PXrSceneComponentsGetInfoMSFT;components:PXrSceneComponentsMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrLocateSceneComponentsMSFT=function(scene:TXrSceneMSFT;const locateInfo:PXrSceneComponentsLocateInfoMSFT;locations:PXrSceneComponentLocationsMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSceneMeshBuffersMSFT=function(scene:TXrSceneMSFT;const getInfo:PXrSceneMeshBuffersGetInfoMSFT;buffers:PXrSceneMeshBuffersMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrDeserializeSceneMSFT=function(sceneObserver:TXrSceneObserverMSFT;const deserializeInfo:PXrSceneDeserializeInfoMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSerializedSceneFragmentDataMSFT=function(scene:TXrSceneMSFT;const getInfo:PXrSerializedSceneFragmentDataGetInfoMSFT;countInput:TXrUInt32;readOutput:PXrUInt32;buffer:PXrUInt8):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateDisplayRefreshRatesFB=function(session:TXrSession;displayRefreshRateCapacityInput:TXrUInt32;displayRefreshRateCountOutput:PXrUInt32;displayRefreshRates:PXrFloat):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetDisplayRefreshRateFB=function(session:TXrSession;displayRefreshRate:PXrFloat):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrRequestDisplayRefreshRateFB=function(session:TXrSession;displayRefreshRate:TXrFloat):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrCreateSpatialAnchorFromPerceptionAnchorMSFT=function(session:TXrSession;perceptionAnchor:IUnknown;anchor:PXrSpatialAnchorMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrTryGetPerceptionAnchorFromSpatialAnchorMSFT=function(session:TXrSession;anchor:TXrSpatialAnchorMSFT;perceptionAnchor:PPIUnknown):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrUpdateSwapchainFB=function(swapchain:TXrSwapchain;const state:PXrSwapchainStateBaseHeaderFB):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetSwapchainStateFB=function(swapchain:TXrSwapchain;state:PXrSwapchainStateBaseHeaderFB):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateColorSpacesFB=function(session:TXrSession;colorSpaceCapacityInput:TXrUInt32;colorSpaceCountOutput:PXrUInt32;colorSpaces:PXrColorSpaceFB):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetColorSpaceFB=function(session:TXrSession;const colorspace:TXrColorSpaceFB):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrSetEnvironmentDepthEstimationVARJO=function(session:TXrSession;enabled:TXrBool32):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrEnumerateReprojectionModesMSFT=function(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;modeCapacityInput:TXrUInt32;modeCountOutput:PXrUInt32;modes:PXrReprojectionModeMSFT):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetAudioOutputDeviceGuidOculus=function(instance:TXrInstance;buffer:Twchar_t):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}

     TxrGetAudioInputDeviceGuidOculus=function(instance:TXrInstance;buffer:Twchar_t):TXrResult; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}


     PPOpenXRCommands=^POpenXRCommands;
     POpenXRCommands=^TOpenXRCommands;
     TOpenXRCommands=record
      GetInstanceProcAddr:TxrGetInstanceProcAddr;

      EnumerateApiLayerProperties:TxrEnumerateApiLayerProperties;

      EnumerateInstanceExtensionProperties:TxrEnumerateInstanceExtensionProperties;

      CreateInstance:TxrCreateInstance;

      DestroyInstance:TxrDestroyInstance;

      ResultToString:TxrResultToString;

      StructureTypeToString:TxrStructureTypeToString;

      GetInstanceProperties:TxrGetInstanceProperties;

      GetSystem:TxrGetSystem;

      GetSystemProperties:TxrGetSystemProperties;

      CreateSession:TxrCreateSession;

      DestroySession:TxrDestroySession;

      DestroySpace:TxrDestroySpace;

      EnumerateSwapchainFormats:TxrEnumerateSwapchainFormats;

      CreateSwapchain:TxrCreateSwapchain;

      DestroySwapchain:TxrDestroySwapchain;

      EnumerateSwapchainImages:TxrEnumerateSwapchainImages;

      AcquireSwapchainImage:TxrAcquireSwapchainImage;

      WaitSwapchainImage:TxrWaitSwapchainImage;

      ReleaseSwapchainImage:TxrReleaseSwapchainImage;

      BeginSession:TxrBeginSession;

      EndSession:TxrEndSession;

      RequestExitSession:TxrRequestExitSession;

      EnumerateReferenceSpaces:TxrEnumerateReferenceSpaces;

      CreateReferenceSpace:TxrCreateReferenceSpace;

      CreateActionSpace:TxrCreateActionSpace;

      LocateSpace:TxrLocateSpace;

      EnumerateViewConfigurations:TxrEnumerateViewConfigurations;

      EnumerateEnvironmentBlendModes:TxrEnumerateEnvironmentBlendModes;

      GetViewConfigurationProperties:TxrGetViewConfigurationProperties;

      EnumerateViewConfigurationViews:TxrEnumerateViewConfigurationViews;

      BeginFrame:TxrBeginFrame;

      LocateViews:TxrLocateViews;

      EndFrame:TxrEndFrame;

      WaitFrame:TxrWaitFrame;

      ApplyHapticFeedback:TxrApplyHapticFeedback;

      StopHapticFeedback:TxrStopHapticFeedback;

      PollEvent:TxrPollEvent;

      StringToPath:TxrStringToPath;

      PathToString:TxrPathToString;

      GetReferenceSpaceBoundsRect:TxrGetReferenceSpaceBoundsRect;

{$ifdef Android}
      SetAndroidApplicationThreadKHR:TxrSetAndroidApplicationThreadKHR;
{$endif}

{$ifdef Android}
      CreateSwapchainAndroidSurfaceKHR:TxrCreateSwapchainAndroidSurfaceKHR;
{$endif}

      GetActionStateBoolean:TxrGetActionStateBoolean;

      GetActionStateFloat:TxrGetActionStateFloat;

      GetActionStateVector2f:TxrGetActionStateVector2f;

      GetActionStatePose:TxrGetActionStatePose;

      CreateActionSet:TxrCreateActionSet;

      DestroyActionSet:TxrDestroyActionSet;

      CreateAction:TxrCreateAction;

      DestroyAction:TxrDestroyAction;

      SuggestInteractionProfileBindings:TxrSuggestInteractionProfileBindings;

      AttachSessionActionSets:TxrAttachSessionActionSets;

      GetCurrentInteractionProfile:TxrGetCurrentInteractionProfile;

      SyncActions:TxrSyncActions;

      EnumerateBoundSourcesForAction:TxrEnumerateBoundSourcesForAction;

      GetInputSourceLocalizedName:TxrGetInputSourceLocalizedName;

      GetVulkanInstanceExtensionsKHR:TxrGetVulkanInstanceExtensionsKHR;

      GetVulkanDeviceExtensionsKHR:TxrGetVulkanDeviceExtensionsKHR;

      GetVulkanGraphicsDeviceKHR:TxrGetVulkanGraphicsDeviceKHR;

      GetOpenGLGraphicsRequirementsKHR:TxrGetOpenGLGraphicsRequirementsKHR;

      GetOpenGLESGraphicsRequirementsKHR:TxrGetOpenGLESGraphicsRequirementsKHR;

      GetVulkanGraphicsRequirementsKHR:TxrGetVulkanGraphicsRequirementsKHR;

      GetD3D11GraphicsRequirementsKHR:TxrGetD3D11GraphicsRequirementsKHR;

      GetD3D12GraphicsRequirementsKHR:TxrGetD3D12GraphicsRequirementsKHR;

      PerfSettingsSetPerformanceLevelEXT:TxrPerfSettingsSetPerformanceLevelEXT;

      ThermalGetTemperatureTrendEXT:TxrThermalGetTemperatureTrendEXT;

      SetDebugUtilsObjectNameEXT:TxrSetDebugUtilsObjectNameEXT;

      CreateDebugUtilsMessengerEXT:TxrCreateDebugUtilsMessengerEXT;

      DestroyDebugUtilsMessengerEXT:TxrDestroyDebugUtilsMessengerEXT;

      SubmitDebugUtilsMessageEXT:TxrSubmitDebugUtilsMessageEXT;

      SessionBeginDebugUtilsLabelRegionEXT:TxrSessionBeginDebugUtilsLabelRegionEXT;

      SessionEndDebugUtilsLabelRegionEXT:TxrSessionEndDebugUtilsLabelRegionEXT;

      SessionInsertDebugUtilsLabelEXT:TxrSessionInsertDebugUtilsLabelEXT;

{$ifdef Windows}
      ConvertTimeToWin32PerformanceCounterKHR:TxrConvertTimeToWin32PerformanceCounterKHR;
{$endif}

{$ifdef Windows}
      ConvertWin32PerformanceCounterToTimeKHR:TxrConvertWin32PerformanceCounterToTimeKHR;
{$endif}

      CreateVulkanInstanceKHR:TxrCreateVulkanInstanceKHR;

      CreateVulkanDeviceKHR:TxrCreateVulkanDeviceKHR;

      GetVulkanGraphicsDevice2KHR:TxrGetVulkanGraphicsDevice2KHR;

      GetVulkanGraphicsRequirements2KHR:TxrGetVulkanGraphicsRequirements2KHR;

      ConvertTimeToTimespecTimeKHR:TxrConvertTimeToTimespecTimeKHR;

      ConvertTimespecTimeToTimeKHR:TxrConvertTimespecTimeToTimeKHR;

      GetVisibilityMaskKHR:TxrGetVisibilityMaskKHR;

      CreateSpatialAnchorMSFT:TxrCreateSpatialAnchorMSFT;

      CreateSpatialAnchorSpaceMSFT:TxrCreateSpatialAnchorSpaceMSFT;

      DestroySpatialAnchorMSFT:TxrDestroySpatialAnchorMSFT;

      SetInputDeviceActiveEXT:TxrSetInputDeviceActiveEXT;

      SetInputDeviceStateBoolEXT:TxrSetInputDeviceStateBoolEXT;

      SetInputDeviceStateFloatEXT:TxrSetInputDeviceStateFloatEXT;

      SetInputDeviceStateVector2fEXT:TxrSetInputDeviceStateVector2fEXT;

      SetInputDeviceLocationEXT:TxrSetInputDeviceLocationEXT;

      InitializeLoaderKHR:TxrInitializeLoaderKHR;

      CreateSpatialGraphNodeSpaceMSFT:TxrCreateSpatialGraphNodeSpaceMSFT;

      CreateHandTrackerEXT:TxrCreateHandTrackerEXT;

      DestroyHandTrackerEXT:TxrDestroyHandTrackerEXT;

      LocateHandJointsEXT:TxrLocateHandJointsEXT;

      CreateHandMeshSpaceMSFT:TxrCreateHandMeshSpaceMSFT;

      UpdateHandMeshMSFT:TxrUpdateHandMeshMSFT;

      GetControllerModelKeyMSFT:TxrGetControllerModelKeyMSFT;

      LoadControllerModelMSFT:TxrLoadControllerModelMSFT;

      GetControllerModelPropertiesMSFT:TxrGetControllerModelPropertiesMSFT;

      GetControllerModelStateMSFT:TxrGetControllerModelStateMSFT;

      EnumerateSceneComputeFeaturesMSFT:TxrEnumerateSceneComputeFeaturesMSFT;

      CreateSceneObserverMSFT:TxrCreateSceneObserverMSFT;

      DestroySceneObserverMSFT:TxrDestroySceneObserverMSFT;

      CreateSceneMSFT:TxrCreateSceneMSFT;

      DestroySceneMSFT:TxrDestroySceneMSFT;

      ComputeNewSceneMSFT:TxrComputeNewSceneMSFT;

      GetSceneComputeStateMSFT:TxrGetSceneComputeStateMSFT;

      GetSceneComponentsMSFT:TxrGetSceneComponentsMSFT;

      LocateSceneComponentsMSFT:TxrLocateSceneComponentsMSFT;

      GetSceneMeshBuffersMSFT:TxrGetSceneMeshBuffersMSFT;

      DeserializeSceneMSFT:TxrDeserializeSceneMSFT;

      GetSerializedSceneFragmentDataMSFT:TxrGetSerializedSceneFragmentDataMSFT;

      EnumerateDisplayRefreshRatesFB:TxrEnumerateDisplayRefreshRatesFB;

      GetDisplayRefreshRateFB:TxrGetDisplayRefreshRateFB;

      RequestDisplayRefreshRateFB:TxrRequestDisplayRefreshRateFB;

      CreateSpatialAnchorFromPerceptionAnchorMSFT:TxrCreateSpatialAnchorFromPerceptionAnchorMSFT;

      TryGetPerceptionAnchorFromSpatialAnchorMSFT:TxrTryGetPerceptionAnchorFromSpatialAnchorMSFT;

      UpdateSwapchainFB:TxrUpdateSwapchainFB;

      GetSwapchainStateFB:TxrGetSwapchainStateFB;

      EnumerateColorSpacesFB:TxrEnumerateColorSpacesFB;

      SetColorSpaceFB:TxrSetColorSpaceFB;

      SetEnvironmentDepthEstimationVARJO:TxrSetEnvironmentDepthEstimationVARJO;

      EnumerateReprojectionModesMSFT:TxrEnumerateReprojectionModesMSFT;

      GetAudioOutputDeviceGuidOculus:TxrGetAudioOutputDeviceGuidOculus;

      GetAudioInputDeviceGuidOculus:TxrGetAudioInputDeviceGuidOculus;

     end;

     TOpenXR=class
      private
       fCommands:TOpenXRCommands;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const AOpenXRCommands:TOpenXRCommands); reintroduce; overload;
       destructor Destroy; override;
       function GetInstanceProcAddr(instance:TXrInstance;const name:PXrChar;_function:PPFN_xrVoidFunction):TXrResult; virtual;

       function EnumerateApiLayerProperties(propertyCapacityInput:TXrUInt32;propertyCountOutput:PXrUInt32;properties:PXrApiLayerProperties):TXrResult; virtual;

       function EnumerateInstanceExtensionProperties(const layerName:PXrChar;propertyCapacityInput:TXrUInt32;propertyCountOutput:PXrUInt32;properties:PXrExtensionProperties):TXrResult; virtual;

       function CreateInstance(const createInfo:PXrInstanceCreateInfo;instance:PXrInstance):TXrResult; virtual;

       function DestroyInstance(instance:TXrInstance):TXrResult; virtual;

       function ResultToString(instance:TXrInstance;value:TXrResult;buffer:TXrChar):TXrResult; virtual;

       function StructureTypeToString(instance:TXrInstance;value:TXrStructureType;buffer:TXrChar):TXrResult; virtual;

       function GetInstanceProperties(instance:TXrInstance;instanceProperties:PXrInstanceProperties):TXrResult; virtual;

       function GetSystem(instance:TXrInstance;const getInfo:PXrSystemGetInfo;systemId:PXrSystemId):TXrResult; virtual;

       function GetSystemProperties(instance:TXrInstance;systemId:TXrSystemId;properties:PXrSystemProperties):TXrResult; virtual;

       function CreateSession(instance:TXrInstance;const createInfo:PXrSessionCreateInfo;session:PXrSession):TXrResult; virtual;

       function DestroySession(session:TXrSession):TXrResult; virtual;

       function DestroySpace(space:TXrSpace):TXrResult; virtual;

       function EnumerateSwapchainFormats(session:TXrSession;formatCapacityInput:TXrUInt32;formatCountOutput:PXrUInt32;formats:PXrInt64):TXrResult; virtual;

       function CreateSwapchain(session:TXrSession;const createInfo:PXrSwapchainCreateInfo;swapchain:PXrSwapchain):TXrResult; virtual;

       function DestroySwapchain(swapchain:TXrSwapchain):TXrResult; virtual;

       function EnumerateSwapchainImages(swapchain:TXrSwapchain;imageCapacityInput:TXrUInt32;imageCountOutput:PXrUInt32;images:PXrSwapchainImageBaseHeader):TXrResult; virtual;

       function AcquireSwapchainImage(swapchain:TXrSwapchain;const acquireInfo:PXrSwapchainImageAcquireInfo;index:PXrUInt32):TXrResult; virtual;

       function WaitSwapchainImage(swapchain:TXrSwapchain;const waitInfo:PXrSwapchainImageWaitInfo):TXrResult; virtual;

       function ReleaseSwapchainImage(swapchain:TXrSwapchain;const releaseInfo:PXrSwapchainImageReleaseInfo):TXrResult; virtual;

       function BeginSession(session:TXrSession;const beginInfo:PXrSessionBeginInfo):TXrResult; virtual;

       function EndSession(session:TXrSession):TXrResult; virtual;

       function RequestExitSession(session:TXrSession):TXrResult; virtual;

       function EnumerateReferenceSpaces(session:TXrSession;spaceCapacityInput:TXrUInt32;spaceCountOutput:PXrUInt32;spaces:PXrReferenceSpaceType):TXrResult; virtual;

       function CreateReferenceSpace(session:TXrSession;const createInfo:PXrReferenceSpaceCreateInfo;space:PXrSpace):TXrResult; virtual;

       function CreateActionSpace(session:TXrSession;const createInfo:PXrActionSpaceCreateInfo;space:PXrSpace):TXrResult; virtual;

       function LocateSpace(space:TXrSpace;baseSpace:TXrSpace;time:TXrTime;location:PXrSpaceLocation):TXrResult; virtual;

       function EnumerateViewConfigurations(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationTypeCapacityInput:TXrUInt32;viewConfigurationTypeCountOutput:PXrUInt32;viewConfigurationTypes:PXrViewConfigurationType):TXrResult; virtual;

       function EnumerateEnvironmentBlendModes(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;environmentBlendModeCapacityInput:TXrUInt32;environmentBlendModeCountOutput:PXrUInt32;environmentBlendModes:PXrEnvironmentBlendMode):TXrResult; virtual;

       function GetViewConfigurationProperties(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;configurationProperties:PXrViewConfigurationProperties):TXrResult; virtual;

       function EnumerateViewConfigurationViews(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;viewCapacityInput:TXrUInt32;viewCountOutput:PXrUInt32;views:PXrViewConfigurationView):TXrResult; virtual;

       function BeginFrame(session:TXrSession;const frameBeginInfo:PXrFrameBeginInfo):TXrResult; virtual;

       function LocateViews(session:TXrSession;const viewLocateInfo:PXrViewLocateInfo;viewState:PXrViewState;viewCapacityInput:TXrUInt32;viewCountOutput:PXrUInt32;views:PXrView):TXrResult; virtual;

       function EndFrame(session:TXrSession;const frameEndInfo:PXrFrameEndInfo):TXrResult; virtual;

       function WaitFrame(session:TXrSession;const frameWaitInfo:PXrFrameWaitInfo;frameState:PXrFrameState):TXrResult; virtual;

       function ApplyHapticFeedback(session:TXrSession;const hapticActionInfo:PXrHapticActionInfo;const hapticFeedback:PXrHapticBaseHeader):TXrResult; virtual;

       function StopHapticFeedback(session:TXrSession;const hapticActionInfo:PXrHapticActionInfo):TXrResult; virtual;

       function PollEvent(instance:TXrInstance;eventData:PXrEventDataBuffer):TXrResult; virtual;

       function StringToPath(instance:TXrInstance;const pathString:PXrChar;path:PXrPath):TXrResult; virtual;

       function PathToString(instance:TXrInstance;path:TXrPath;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; virtual;

       function GetReferenceSpaceBoundsRect(session:TXrSession;referenceSpaceType:TXrReferenceSpaceType;bounds:PXrExtent2Df):TXrResult; virtual;

{$ifdef Android}
       function SetAndroidApplicationThreadKHR(session:TXrSession;threadType:TXrAndroidThreadTypeKHR;threadId:TXrUInt32):TXrResult; virtual;
{$endif}

{$ifdef Android}
       function CreateSwapchainAndroidSurfaceKHR(session:TXrSession;const info:PXrSwapchainCreateInfo;swapchain:PXrSwapchain;surface:Pjobject):TXrResult; virtual;
{$endif}

       function GetActionStateBoolean(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateBoolean):TXrResult; virtual;

       function GetActionStateFloat(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateFloat):TXrResult; virtual;

       function GetActionStateVector2f(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateVector2f):TXrResult; virtual;

       function GetActionStatePose(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStatePose):TXrResult; virtual;

       function CreateActionSet(instance:TXrInstance;const createInfo:PXrActionSetCreateInfo;actionSet:PXrActionSet):TXrResult; virtual;

       function DestroyActionSet(actionSet:TXrActionSet):TXrResult; virtual;

       function CreateAction(actionSet:TXrActionSet;const createInfo:PXrActionCreateInfo;action:PXrAction):TXrResult; virtual;

       function DestroyAction(action:TXrAction):TXrResult; virtual;

       function SuggestInteractionProfileBindings(instance:TXrInstance;const suggestedBindings:PXrInteractionProfileSuggestedBinding):TXrResult; virtual;

       function AttachSessionActionSets(session:TXrSession;const attachInfo:PXrSessionActionSetsAttachInfo):TXrResult; virtual;

       function GetCurrentInteractionProfile(session:TXrSession;topLevelUserPath:TXrPath;interactionProfile:PXrInteractionProfileState):TXrResult; virtual;

       function SyncActions(session:TXrSession;const syncInfo:PXrActionsSyncInfo):TXrResult; virtual;

       function EnumerateBoundSourcesForAction(session:TXrSession;const enumerateInfo:PXrBoundSourcesForActionEnumerateInfo;sourceCapacityInput:TXrUInt32;sourceCountOutput:PXrUInt32;sources:PXrPath):TXrResult; virtual;

       function GetInputSourceLocalizedName(session:TXrSession;const getInfo:PXrInputSourceLocalizedNameGetInfo;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; virtual;

       function GetVulkanInstanceExtensionsKHR(instance:TXrInstance;systemId:TXrSystemId;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; virtual;

       function GetVulkanDeviceExtensionsKHR(instance:TXrInstance;systemId:TXrSystemId;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult; virtual;

       function GetVulkanGraphicsDeviceKHR(instance:TXrInstance;systemId:TXrSystemId;vkInstance:TVkInstance;vkPhysicalDevice:PVkPhysicalDevice):TXrResult; virtual;

       function GetOpenGLGraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsOpenGLKHR):TXrResult; virtual;

       function GetOpenGLESGraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsOpenGLESKHR):TXrResult; virtual;

       function GetVulkanGraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsVulkanKHR):TXrResult; virtual;

       function GetD3D11GraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsD3D11KHR):TXrResult; virtual;

       function GetD3D12GraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsD3D12KHR):TXrResult; virtual;

       function PerfSettingsSetPerformanceLevelEXT(session:TXrSession;domain:TXrPerfSettingsDomainEXT;level:TXrPerfSettingsLevelEXT):TXrResult; virtual;

       function ThermalGetTemperatureTrendEXT(session:TXrSession;domain:TXrPerfSettingsDomainEXT;notificationLevel:PXrPerfSettingsNotificationLevelEXT;tempHeadroom:PXrFloat;tempSlope:PXrFloat):TXrResult; virtual;

       function SetDebugUtilsObjectNameEXT(instance:TXrInstance;const nameInfo:PXrDebugUtilsObjectNameInfoEXT):TXrResult; virtual;

       function CreateDebugUtilsMessengerEXT(instance:TXrInstance;const createInfo:PXrDebugUtilsMessengerCreateInfoEXT;messenger:PXrDebugUtilsMessengerEXT):TXrResult; virtual;

       function DestroyDebugUtilsMessengerEXT(messenger:TXrDebugUtilsMessengerEXT):TXrResult; virtual;

       function SubmitDebugUtilsMessageEXT(instance:TXrInstance;messageSeverity:TXrDebugUtilsMessageSeverityFlagsEXT;messageTypes:TXrDebugUtilsMessageTypeFlagsEXT;const callbackData:PXrDebugUtilsMessengerCallbackDataEXT):TXrResult; virtual;

       function SessionBeginDebugUtilsLabelRegionEXT(session:TXrSession;const labelInfo:PXrDebugUtilsLabelEXT):TXrResult; virtual;

       function SessionEndDebugUtilsLabelRegionEXT(session:TXrSession):TXrResult; virtual;

       function SessionInsertDebugUtilsLabelEXT(session:TXrSession;const labelInfo:PXrDebugUtilsLabelEXT):TXrResult; virtual;

{$ifdef Windows}
       function ConvertTimeToWin32PerformanceCounterKHR(instance:TXrInstance;time:TXrTime;performanceCounter:PLargeInteger):TXrResult; virtual;
{$endif}

{$ifdef Windows}
       function ConvertWin32PerformanceCounterToTimeKHR(instance:TXrInstance;const performanceCounter:PLargeInteger;time:PXrTime):TXrResult; virtual;
{$endif}

       function CreateVulkanInstanceKHR(instance:TXrInstance;const createInfo:PXrVulkanInstanceCreateInfoKHR;vulkanInstance:PVkInstance;vulkanResult:PVkResult):TXrResult; virtual;

       function CreateVulkanDeviceKHR(instance:TXrInstance;const createInfo:PXrVulkanDeviceCreateInfoKHR;vulkanDevice:PVkDevice;vulkanResult:PVkResult):TXrResult; virtual;

       function GetVulkanGraphicsDevice2KHR(instance:TXrInstance;const getInfo:PXrVulkanGraphicsDeviceGetInfoKHR;vulkanPhysicalDevice:PVkPhysicalDevice):TXrResult; virtual;

       function GetVulkanGraphicsRequirements2KHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsVulkanKHR):TXrResult; virtual;

       function ConvertTimeToTimespecTimeKHR(instance:TXrInstance;time:TXrTime;timespecTime:Ptimespec):TXrResult; virtual;

       function ConvertTimespecTimeToTimeKHR(instance:TXrInstance;const timespecTime:Ptimespec;time:PXrTime):TXrResult; virtual;

       function GetVisibilityMaskKHR(session:TXrSession;viewConfigurationType:TXrViewConfigurationType;viewIndex:TXrUInt32;visibilityMaskType:TXrVisibilityMaskTypeKHR;visibilityMask:PXrVisibilityMaskKHR):TXrResult; virtual;

       function CreateSpatialAnchorMSFT(session:TXrSession;const createInfo:PXrSpatialAnchorCreateInfoMSFT;anchor:PXrSpatialAnchorMSFT):TXrResult; virtual;

       function CreateSpatialAnchorSpaceMSFT(session:TXrSession;const createInfo:PXrSpatialAnchorSpaceCreateInfoMSFT;space:PXrSpace):TXrResult; virtual;

       function DestroySpatialAnchorMSFT(anchor:TXrSpatialAnchorMSFT):TXrResult; virtual;

       function SetInputDeviceActiveEXT(session:TXrSession;interactionProfile:TXrPath;topLevelPath:TXrPath;isActive:TXrBool32):TXrResult; virtual;

       function SetInputDeviceStateBoolEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrBool32):TXrResult; virtual;

       function SetInputDeviceStateFloatEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrFloat):TXrResult; virtual;

       function SetInputDeviceStateVector2fEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrVector2f):TXrResult; virtual;

       function SetInputDeviceLocationEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;space:TXrSpace;pose:TXrPosef):TXrResult; virtual;

       function InitializeLoaderKHR(const loaderInitInfo:PXrLoaderInitInfoBaseHeaderKHR):TXrResult; virtual;

       function CreateSpatialGraphNodeSpaceMSFT(session:TXrSession;const createInfo:PXrSpatialGraphNodeSpaceCreateInfoMSFT;space:PXrSpace):TXrResult; virtual;

       function CreateHandTrackerEXT(session:TXrSession;const createInfo:PXrHandTrackerCreateInfoEXT;handTracker:PXrHandTrackerEXT):TXrResult; virtual;

       function DestroyHandTrackerEXT(handTracker:TXrHandTrackerEXT):TXrResult; virtual;

       function LocateHandJointsEXT(handTracker:TXrHandTrackerEXT;const locateInfo:PXrHandJointsLocateInfoEXT;locations:PXrHandJointLocationsEXT):TXrResult; virtual;

       function CreateHandMeshSpaceMSFT(handTracker:TXrHandTrackerEXT;const createInfo:PXrHandMeshSpaceCreateInfoMSFT;space:PXrSpace):TXrResult; virtual;

       function UpdateHandMeshMSFT(handTracker:TXrHandTrackerEXT;const updateInfo:PXrHandMeshUpdateInfoMSFT;handMesh:PXrHandMeshMSFT):TXrResult; virtual;

       function GetControllerModelKeyMSFT(session:TXrSession;topLevelUserPath:TXrPath;controllerModelKeyState:PXrControllerModelKeyStateMSFT):TXrResult; virtual;

       function LoadControllerModelMSFT(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrUInt8):TXrResult; virtual;

       function GetControllerModelPropertiesMSFT(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;properties:PXrControllerModelPropertiesMSFT):TXrResult; virtual;

       function GetControllerModelStateMSFT(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;state:PXrControllerModelStateMSFT):TXrResult; virtual;

       function EnumerateSceneComputeFeaturesMSFT(instance:TXrInstance;systemId:TXrSystemId;featureCapacityInput:TXrUInt32;featureCountOutput:PXrUInt32;features:PXrSceneComputeFeatureMSFT):TXrResult; virtual;

       function CreateSceneObserverMSFT(session:TXrSession;const createInfo:PXrSceneObserverCreateInfoMSFT;sceneObserver:PXrSceneObserverMSFT):TXrResult; virtual;

       function DestroySceneObserverMSFT(sceneObserver:TXrSceneObserverMSFT):TXrResult; virtual;

       function CreateSceneMSFT(sceneObserver:TXrSceneObserverMSFT;const createInfo:PXrSceneCreateInfoMSFT;scene:PXrSceneMSFT):TXrResult; virtual;

       function DestroySceneMSFT(scene:TXrSceneMSFT):TXrResult; virtual;

       function ComputeNewSceneMSFT(sceneObserver:TXrSceneObserverMSFT;const computeInfo:PXrNewSceneComputeInfoMSFT):TXrResult; virtual;

       function GetSceneComputeStateMSFT(sceneObserver:TXrSceneObserverMSFT;state:PXrSceneComputeStateMSFT):TXrResult; virtual;

       function GetSceneComponentsMSFT(scene:TXrSceneMSFT;const getInfo:PXrSceneComponentsGetInfoMSFT;components:PXrSceneComponentsMSFT):TXrResult; virtual;

       function LocateSceneComponentsMSFT(scene:TXrSceneMSFT;const locateInfo:PXrSceneComponentsLocateInfoMSFT;locations:PXrSceneComponentLocationsMSFT):TXrResult; virtual;

       function GetSceneMeshBuffersMSFT(scene:TXrSceneMSFT;const getInfo:PXrSceneMeshBuffersGetInfoMSFT;buffers:PXrSceneMeshBuffersMSFT):TXrResult; virtual;

       function DeserializeSceneMSFT(sceneObserver:TXrSceneObserverMSFT;const deserializeInfo:PXrSceneDeserializeInfoMSFT):TXrResult; virtual;

       function GetSerializedSceneFragmentDataMSFT(scene:TXrSceneMSFT;const getInfo:PXrSerializedSceneFragmentDataGetInfoMSFT;countInput:TXrUInt32;readOutput:PXrUInt32;buffer:PXrUInt8):TXrResult; virtual;

       function EnumerateDisplayRefreshRatesFB(session:TXrSession;displayRefreshRateCapacityInput:TXrUInt32;displayRefreshRateCountOutput:PXrUInt32;displayRefreshRates:PXrFloat):TXrResult; virtual;

       function GetDisplayRefreshRateFB(session:TXrSession;displayRefreshRate:PXrFloat):TXrResult; virtual;

       function RequestDisplayRefreshRateFB(session:TXrSession;displayRefreshRate:TXrFloat):TXrResult; virtual;

       function CreateSpatialAnchorFromPerceptionAnchorMSFT(session:TXrSession;perceptionAnchor:IUnknown;anchor:PXrSpatialAnchorMSFT):TXrResult; virtual;

       function TryGetPerceptionAnchorFromSpatialAnchorMSFT(session:TXrSession;anchor:TXrSpatialAnchorMSFT;perceptionAnchor:PPIUnknown):TXrResult; virtual;

       function UpdateSwapchainFB(swapchain:TXrSwapchain;const state:PXrSwapchainStateBaseHeaderFB):TXrResult; virtual;

       function GetSwapchainStateFB(swapchain:TXrSwapchain;state:PXrSwapchainStateBaseHeaderFB):TXrResult; virtual;

       function EnumerateColorSpacesFB(session:TXrSession;colorSpaceCapacityInput:TXrUInt32;colorSpaceCountOutput:PXrUInt32;colorSpaces:PXrColorSpaceFB):TXrResult; virtual;

       function SetColorSpaceFB(session:TXrSession;const colorspace:TXrColorSpaceFB):TXrResult; virtual;

       function SetEnvironmentDepthEstimationVARJO(session:TXrSession;enabled:TXrBool32):TXrResult; virtual;

       function EnumerateReprojectionModesMSFT(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;modeCapacityInput:TXrUInt32;modeCountOutput:PXrUInt32;modes:PXrReprojectionModeMSFT):TXrResult; virtual;

       function GetAudioOutputDeviceGuidOculus(instance:TXrInstance;buffer:Twchar_t):TXrResult; virtual;

       function GetAudioInputDeviceGuidOculus(instance:TXrInstance;buffer:Twchar_t):TXrResult; virtual;

       property Commands:TOpenXRCommands read fCommands;
     end;

var LibOpenXR:pointer=nil;

    xr:TOpenXR=nil;

    xrGetInstanceProcAddr:TxrGetInstanceProcAddr=nil;

    xrEnumerateApiLayerProperties:TxrEnumerateApiLayerProperties=nil;

    xrEnumerateInstanceExtensionProperties:TxrEnumerateInstanceExtensionProperties=nil;

    xrCreateInstance:TxrCreateInstance=nil;

    xrDestroyInstance:TxrDestroyInstance=nil;

    xrResultToString:TxrResultToString=nil;

    xrStructureTypeToString:TxrStructureTypeToString=nil;

    xrGetInstanceProperties:TxrGetInstanceProperties=nil;

    xrGetSystem:TxrGetSystem=nil;

    xrGetSystemProperties:TxrGetSystemProperties=nil;

    xrCreateSession:TxrCreateSession=nil;

    xrDestroySession:TxrDestroySession=nil;

    xrDestroySpace:TxrDestroySpace=nil;

    xrEnumerateSwapchainFormats:TxrEnumerateSwapchainFormats=nil;

    xrCreateSwapchain:TxrCreateSwapchain=nil;

    xrDestroySwapchain:TxrDestroySwapchain=nil;

    xrEnumerateSwapchainImages:TxrEnumerateSwapchainImages=nil;

    xrAcquireSwapchainImage:TxrAcquireSwapchainImage=nil;

    xrWaitSwapchainImage:TxrWaitSwapchainImage=nil;

    xrReleaseSwapchainImage:TxrReleaseSwapchainImage=nil;

    xrBeginSession:TxrBeginSession=nil;

    xrEndSession:TxrEndSession=nil;

    xrRequestExitSession:TxrRequestExitSession=nil;

    xrEnumerateReferenceSpaces:TxrEnumerateReferenceSpaces=nil;

    xrCreateReferenceSpace:TxrCreateReferenceSpace=nil;

    xrCreateActionSpace:TxrCreateActionSpace=nil;

    xrLocateSpace:TxrLocateSpace=nil;

    xrEnumerateViewConfigurations:TxrEnumerateViewConfigurations=nil;

    xrEnumerateEnvironmentBlendModes:TxrEnumerateEnvironmentBlendModes=nil;

    xrGetViewConfigurationProperties:TxrGetViewConfigurationProperties=nil;

    xrEnumerateViewConfigurationViews:TxrEnumerateViewConfigurationViews=nil;

    xrBeginFrame:TxrBeginFrame=nil;

    xrLocateViews:TxrLocateViews=nil;

    xrEndFrame:TxrEndFrame=nil;

    xrWaitFrame:TxrWaitFrame=nil;

    xrApplyHapticFeedback:TxrApplyHapticFeedback=nil;

    xrStopHapticFeedback:TxrStopHapticFeedback=nil;

    xrPollEvent:TxrPollEvent=nil;

    xrStringToPath:TxrStringToPath=nil;

    xrPathToString:TxrPathToString=nil;

    xrGetReferenceSpaceBoundsRect:TxrGetReferenceSpaceBoundsRect=nil;

{$ifdef Android}
    xrSetAndroidApplicationThreadKHR:TxrSetAndroidApplicationThreadKHR=nil;
{$endif}

{$ifdef Android}
    xrCreateSwapchainAndroidSurfaceKHR:TxrCreateSwapchainAndroidSurfaceKHR=nil;
{$endif}

    xrGetActionStateBoolean:TxrGetActionStateBoolean=nil;

    xrGetActionStateFloat:TxrGetActionStateFloat=nil;

    xrGetActionStateVector2f:TxrGetActionStateVector2f=nil;

    xrGetActionStatePose:TxrGetActionStatePose=nil;

    xrCreateActionSet:TxrCreateActionSet=nil;

    xrDestroyActionSet:TxrDestroyActionSet=nil;

    xrCreateAction:TxrCreateAction=nil;

    xrDestroyAction:TxrDestroyAction=nil;

    xrSuggestInteractionProfileBindings:TxrSuggestInteractionProfileBindings=nil;

    xrAttachSessionActionSets:TxrAttachSessionActionSets=nil;

    xrGetCurrentInteractionProfile:TxrGetCurrentInteractionProfile=nil;

    xrSyncActions:TxrSyncActions=nil;

    xrEnumerateBoundSourcesForAction:TxrEnumerateBoundSourcesForAction=nil;

    xrGetInputSourceLocalizedName:TxrGetInputSourceLocalizedName=nil;

    xrGetVulkanInstanceExtensionsKHR:TxrGetVulkanInstanceExtensionsKHR=nil;

    xrGetVulkanDeviceExtensionsKHR:TxrGetVulkanDeviceExtensionsKHR=nil;

    xrGetVulkanGraphicsDeviceKHR:TxrGetVulkanGraphicsDeviceKHR=nil;

    xrGetOpenGLGraphicsRequirementsKHR:TxrGetOpenGLGraphicsRequirementsKHR=nil;

    xrGetOpenGLESGraphicsRequirementsKHR:TxrGetOpenGLESGraphicsRequirementsKHR=nil;

    xrGetVulkanGraphicsRequirementsKHR:TxrGetVulkanGraphicsRequirementsKHR=nil;

    xrGetD3D11GraphicsRequirementsKHR:TxrGetD3D11GraphicsRequirementsKHR=nil;

    xrGetD3D12GraphicsRequirementsKHR:TxrGetD3D12GraphicsRequirementsKHR=nil;

    xrPerfSettingsSetPerformanceLevelEXT:TxrPerfSettingsSetPerformanceLevelEXT=nil;

    xrThermalGetTemperatureTrendEXT:TxrThermalGetTemperatureTrendEXT=nil;

    xrSetDebugUtilsObjectNameEXT:TxrSetDebugUtilsObjectNameEXT=nil;

    xrCreateDebugUtilsMessengerEXT:TxrCreateDebugUtilsMessengerEXT=nil;

    xrDestroyDebugUtilsMessengerEXT:TxrDestroyDebugUtilsMessengerEXT=nil;

    xrSubmitDebugUtilsMessageEXT:TxrSubmitDebugUtilsMessageEXT=nil;

    xrSessionBeginDebugUtilsLabelRegionEXT:TxrSessionBeginDebugUtilsLabelRegionEXT=nil;

    xrSessionEndDebugUtilsLabelRegionEXT:TxrSessionEndDebugUtilsLabelRegionEXT=nil;

    xrSessionInsertDebugUtilsLabelEXT:TxrSessionInsertDebugUtilsLabelEXT=nil;

{$ifdef Windows}
    xrConvertTimeToWin32PerformanceCounterKHR:TxrConvertTimeToWin32PerformanceCounterKHR=nil;
{$endif}

{$ifdef Windows}
    xrConvertWin32PerformanceCounterToTimeKHR:TxrConvertWin32PerformanceCounterToTimeKHR=nil;
{$endif}

    xrCreateVulkanInstanceKHR:TxrCreateVulkanInstanceKHR=nil;

    xrCreateVulkanDeviceKHR:TxrCreateVulkanDeviceKHR=nil;

    xrGetVulkanGraphicsDevice2KHR:TxrGetVulkanGraphicsDevice2KHR=nil;

    xrGetVulkanGraphicsRequirements2KHR:TxrGetVulkanGraphicsRequirements2KHR=nil;

    xrConvertTimeToTimespecTimeKHR:TxrConvertTimeToTimespecTimeKHR=nil;

    xrConvertTimespecTimeToTimeKHR:TxrConvertTimespecTimeToTimeKHR=nil;

    xrGetVisibilityMaskKHR:TxrGetVisibilityMaskKHR=nil;

    xrCreateSpatialAnchorMSFT:TxrCreateSpatialAnchorMSFT=nil;

    xrCreateSpatialAnchorSpaceMSFT:TxrCreateSpatialAnchorSpaceMSFT=nil;

    xrDestroySpatialAnchorMSFT:TxrDestroySpatialAnchorMSFT=nil;

    xrSetInputDeviceActiveEXT:TxrSetInputDeviceActiveEXT=nil;

    xrSetInputDeviceStateBoolEXT:TxrSetInputDeviceStateBoolEXT=nil;

    xrSetInputDeviceStateFloatEXT:TxrSetInputDeviceStateFloatEXT=nil;

    xrSetInputDeviceStateVector2fEXT:TxrSetInputDeviceStateVector2fEXT=nil;

    xrSetInputDeviceLocationEXT:TxrSetInputDeviceLocationEXT=nil;

    xrInitializeLoaderKHR:TxrInitializeLoaderKHR=nil;

    xrCreateSpatialGraphNodeSpaceMSFT:TxrCreateSpatialGraphNodeSpaceMSFT=nil;

    xrCreateHandTrackerEXT:TxrCreateHandTrackerEXT=nil;

    xrDestroyHandTrackerEXT:TxrDestroyHandTrackerEXT=nil;

    xrLocateHandJointsEXT:TxrLocateHandJointsEXT=nil;

    xrCreateHandMeshSpaceMSFT:TxrCreateHandMeshSpaceMSFT=nil;

    xrUpdateHandMeshMSFT:TxrUpdateHandMeshMSFT=nil;

    xrGetControllerModelKeyMSFT:TxrGetControllerModelKeyMSFT=nil;

    xrLoadControllerModelMSFT:TxrLoadControllerModelMSFT=nil;

    xrGetControllerModelPropertiesMSFT:TxrGetControllerModelPropertiesMSFT=nil;

    xrGetControllerModelStateMSFT:TxrGetControllerModelStateMSFT=nil;

    xrEnumerateSceneComputeFeaturesMSFT:TxrEnumerateSceneComputeFeaturesMSFT=nil;

    xrCreateSceneObserverMSFT:TxrCreateSceneObserverMSFT=nil;

    xrDestroySceneObserverMSFT:TxrDestroySceneObserverMSFT=nil;

    xrCreateSceneMSFT:TxrCreateSceneMSFT=nil;

    xrDestroySceneMSFT:TxrDestroySceneMSFT=nil;

    xrComputeNewSceneMSFT:TxrComputeNewSceneMSFT=nil;

    xrGetSceneComputeStateMSFT:TxrGetSceneComputeStateMSFT=nil;

    xrGetSceneComponentsMSFT:TxrGetSceneComponentsMSFT=nil;

    xrLocateSceneComponentsMSFT:TxrLocateSceneComponentsMSFT=nil;

    xrGetSceneMeshBuffersMSFT:TxrGetSceneMeshBuffersMSFT=nil;

    xrDeserializeSceneMSFT:TxrDeserializeSceneMSFT=nil;

    xrGetSerializedSceneFragmentDataMSFT:TxrGetSerializedSceneFragmentDataMSFT=nil;

    xrEnumerateDisplayRefreshRatesFB:TxrEnumerateDisplayRefreshRatesFB=nil;

    xrGetDisplayRefreshRateFB:TxrGetDisplayRefreshRateFB=nil;

    xrRequestDisplayRefreshRateFB:TxrRequestDisplayRefreshRateFB=nil;

    xrCreateSpatialAnchorFromPerceptionAnchorMSFT:TxrCreateSpatialAnchorFromPerceptionAnchorMSFT=nil;

    xrTryGetPerceptionAnchorFromSpatialAnchorMSFT:TxrTryGetPerceptionAnchorFromSpatialAnchorMSFT=nil;

    xrUpdateSwapchainFB:TxrUpdateSwapchainFB=nil;

    xrGetSwapchainStateFB:TxrGetSwapchainStateFB=nil;

    xrEnumerateColorSpacesFB:TxrEnumerateColorSpacesFB=nil;

    xrSetColorSpaceFB:TxrSetColorSpaceFB=nil;

    xrSetEnvironmentDepthEstimationVARJO:TxrSetEnvironmentDepthEstimationVARJO=nil;

    xrEnumerateReprojectionModesMSFT:TxrEnumerateReprojectionModesMSFT=nil;

    xrGetAudioOutputDeviceGuidOculus:TxrGetAudioOutputDeviceGuidOculus=nil;

    xrGetAudioInputDeviceGuidOculus:TxrGetAudioInputDeviceGuidOculus=nil;


function XR_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
function XR_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
function XR_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
function XR_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}

function xrLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
function xrFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}
function xrGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}

function xrVoidFunctionToPointer(const VoidFunction:TPFN_xrVoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}

function LoadOpenXRLibrary(const LibraryName:string=XR_DEFAULT_LIB_NAME):boolean;
function LoadOpenXRGlobalCommands:boolean;
function LoadOpenXRInstanceCommands(const GetInstanceProcAddr:TxrGetInstanceProcAddr;const Instance:TXrInstance;out InstanceCommands:TOpenXRCommands):boolean;
function LoadOpenXRDeviceCommands(const GetDeviceProcAddr:TxrGetDeviceProcAddr;const Device:TXrDevice;out DeviceCommands:TOpenXRCommands):boolean;

implementation

function XR_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(VersionMajor shl 22) or (VersionMinor shl 12) or (VersionPatch shl 0);
end;

function XR_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Version shr 22;
end;

function XR_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Version shr 12) and $3ff;
end;

function XR_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=(Version shr 0) and $fff;
end;

function xrLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:={%H-}pointer(LoadLibrary(PChar(LibraryName)));
{$else}
{$ifdef Unix}
 result:=dlopen(PChar(LibraryName),RTLD_NOW or RTLD_LAZY);
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function xrFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=assigned(LibraryHandle);
 if result then begin
{$ifdef Windows}
  result:=FreeLibrary({%H-}HMODULE(LibraryHandle));
{$else}
{$ifdef Unix}
  result:=dlclose(LibraryHandle)=0;
{$else}
  result:=false;
{$endif}
{$endif}
 end;
end;

function xrGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:=GetProcAddress({%H-}HMODULE(LibraryHandle),PChar(ProcName));
{$else}
{$ifdef Unix}
 result:=dlsym(LibraryHandle,PChar(ProcName));
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function xrVoidFunctionToPointer(const VoidFunction:TPFN_xrVoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=addr(VoidFunction);
end;

function LoadOpenXRLibrary(const LibraryName:string=XR_DEFAULT_LIB_NAME):boolean;
begin
 LibOpenXR:=xrLoadLibrary(LibraryName);
 result:=assigned(LibOpenXR);
 if result then begin
  xrGetInstanceProcAddr:=xrGetProcAddress(LibOpenXR,'xrGetInstanceProcAddr');
  @xr.fCommands.GetInstanceProcAddr:=addr(xrGetInstanceProcAddr);
  result:=assigned(xrGetInstanceProcAddr);
  if result then begin
   xrEnumerateInstanceExtensionProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar('xrEnumerateInstanceExtensionProperties')));
   @xr.fCommands.EnumerateInstanceExtensionProperties:=addr(xrEnumerateInstanceExtensionProperties);
   xrEnumerateInstanceLayerProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar('xrEnumerateInstanceLayerProperties')));
   @xr.fCommands.EnumerateInstanceLayerProperties:=addr(xrEnumerateInstanceLayerProperties);
   xrCreateInstance:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar('xrCreateInstance')));
   @xr.fCommands.CreateInstance:=addr(xrCreateInstance);
   result:=assigned(xrEnumerateInstanceExtensionProperties) and
           assigned(xrEnumerateInstanceLayerProperties) and 
           assigned(xrCreateInstance);
  end;
 end;
end;

function LoadOpenXRGlobalCommands:boolean;
begin
 result:=assigned(xrGetInstanceProcAddr);
 if result then begin
  if not assigned(xrGetInstanceProcAddr) then begin
   @xrGetInstanceProcAddr:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetInstanceProcAddr'));
   @xr.fCommands.GetInstanceProcAddr:=addr(xrGetInstanceProcAddr);
  end;
  if not assigned(xrEnumerateApiLayerProperties) then begin
   @xrEnumerateApiLayerProperties:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateApiLayerProperties'));
   @xr.fCommands.EnumerateApiLayerProperties:=addr(xrEnumerateApiLayerProperties);
  end;
  if not assigned(xrEnumerateInstanceExtensionProperties) then begin
   @xrEnumerateInstanceExtensionProperties:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateInstanceExtensionProperties'));
   @xr.fCommands.EnumerateInstanceExtensionProperties:=addr(xrEnumerateInstanceExtensionProperties);
  end;
  if not assigned(xrCreateInstance) then begin
   @xrCreateInstance:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateInstance'));
   @xr.fCommands.CreateInstance:=addr(xrCreateInstance);
  end;
  if not assigned(xrDestroyInstance) then begin
   @xrDestroyInstance:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroyInstance'));
   @xr.fCommands.DestroyInstance:=addr(xrDestroyInstance);
  end;
  if not assigned(xrResultToString) then begin
   @xrResultToString:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrResultToString'));
   @xr.fCommands.ResultToString:=addr(xrResultToString);
  end;
  if not assigned(xrStructureTypeToString) then begin
   @xrStructureTypeToString:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrStructureTypeToString'));
   @xr.fCommands.StructureTypeToString:=addr(xrStructureTypeToString);
  end;
  if not assigned(xrGetInstanceProperties) then begin
   @xrGetInstanceProperties:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetInstanceProperties'));
   @xr.fCommands.GetInstanceProperties:=addr(xrGetInstanceProperties);
  end;
  if not assigned(xrGetSystem) then begin
   @xrGetSystem:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSystem'));
   @xr.fCommands.GetSystem:=addr(xrGetSystem);
  end;
  if not assigned(xrGetSystemProperties) then begin
   @xrGetSystemProperties:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSystemProperties'));
   @xr.fCommands.GetSystemProperties:=addr(xrGetSystemProperties);
  end;
  if not assigned(xrCreateSession) then begin
   @xrCreateSession:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSession'));
   @xr.fCommands.CreateSession:=addr(xrCreateSession);
  end;
  if not assigned(xrDestroySession) then begin
   @xrDestroySession:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroySession'));
   @xr.fCommands.DestroySession:=addr(xrDestroySession);
  end;
  if not assigned(xrDestroySpace) then begin
   @xrDestroySpace:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroySpace'));
   @xr.fCommands.DestroySpace:=addr(xrDestroySpace);
  end;
  if not assigned(xrEnumerateSwapchainFormats) then begin
   @xrEnumerateSwapchainFormats:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateSwapchainFormats'));
   @xr.fCommands.EnumerateSwapchainFormats:=addr(xrEnumerateSwapchainFormats);
  end;
  if not assigned(xrCreateSwapchain) then begin
   @xrCreateSwapchain:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSwapchain'));
   @xr.fCommands.CreateSwapchain:=addr(xrCreateSwapchain);
  end;
  if not assigned(xrDestroySwapchain) then begin
   @xrDestroySwapchain:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroySwapchain'));
   @xr.fCommands.DestroySwapchain:=addr(xrDestroySwapchain);
  end;
  if not assigned(xrEnumerateSwapchainImages) then begin
   @xrEnumerateSwapchainImages:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateSwapchainImages'));
   @xr.fCommands.EnumerateSwapchainImages:=addr(xrEnumerateSwapchainImages);
  end;
  if not assigned(xrAcquireSwapchainImage) then begin
   @xrAcquireSwapchainImage:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrAcquireSwapchainImage'));
   @xr.fCommands.AcquireSwapchainImage:=addr(xrAcquireSwapchainImage);
  end;
  if not assigned(xrWaitSwapchainImage) then begin
   @xrWaitSwapchainImage:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrWaitSwapchainImage'));
   @xr.fCommands.WaitSwapchainImage:=addr(xrWaitSwapchainImage);
  end;
  if not assigned(xrReleaseSwapchainImage) then begin
   @xrReleaseSwapchainImage:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrReleaseSwapchainImage'));
   @xr.fCommands.ReleaseSwapchainImage:=addr(xrReleaseSwapchainImage);
  end;
  if not assigned(xrBeginSession) then begin
   @xrBeginSession:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrBeginSession'));
   @xr.fCommands.BeginSession:=addr(xrBeginSession);
  end;
  if not assigned(xrEndSession) then begin
   @xrEndSession:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEndSession'));
   @xr.fCommands.EndSession:=addr(xrEndSession);
  end;
  if not assigned(xrRequestExitSession) then begin
   @xrRequestExitSession:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrRequestExitSession'));
   @xr.fCommands.RequestExitSession:=addr(xrRequestExitSession);
  end;
  if not assigned(xrEnumerateReferenceSpaces) then begin
   @xrEnumerateReferenceSpaces:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateReferenceSpaces'));
   @xr.fCommands.EnumerateReferenceSpaces:=addr(xrEnumerateReferenceSpaces);
  end;
  if not assigned(xrCreateReferenceSpace) then begin
   @xrCreateReferenceSpace:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateReferenceSpace'));
   @xr.fCommands.CreateReferenceSpace:=addr(xrCreateReferenceSpace);
  end;
  if not assigned(xrCreateActionSpace) then begin
   @xrCreateActionSpace:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateActionSpace'));
   @xr.fCommands.CreateActionSpace:=addr(xrCreateActionSpace);
  end;
  if not assigned(xrLocateSpace) then begin
   @xrLocateSpace:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrLocateSpace'));
   @xr.fCommands.LocateSpace:=addr(xrLocateSpace);
  end;
  if not assigned(xrEnumerateViewConfigurations) then begin
   @xrEnumerateViewConfigurations:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateViewConfigurations'));
   @xr.fCommands.EnumerateViewConfigurations:=addr(xrEnumerateViewConfigurations);
  end;
  if not assigned(xrEnumerateEnvironmentBlendModes) then begin
   @xrEnumerateEnvironmentBlendModes:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateEnvironmentBlendModes'));
   @xr.fCommands.EnumerateEnvironmentBlendModes:=addr(xrEnumerateEnvironmentBlendModes);
  end;
  if not assigned(xrGetViewConfigurationProperties) then begin
   @xrGetViewConfigurationProperties:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetViewConfigurationProperties'));
   @xr.fCommands.GetViewConfigurationProperties:=addr(xrGetViewConfigurationProperties);
  end;
  if not assigned(xrEnumerateViewConfigurationViews) then begin
   @xrEnumerateViewConfigurationViews:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateViewConfigurationViews'));
   @xr.fCommands.EnumerateViewConfigurationViews:=addr(xrEnumerateViewConfigurationViews);
  end;
  if not assigned(xrBeginFrame) then begin
   @xrBeginFrame:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrBeginFrame'));
   @xr.fCommands.BeginFrame:=addr(xrBeginFrame);
  end;
  if not assigned(xrLocateViews) then begin
   @xrLocateViews:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrLocateViews'));
   @xr.fCommands.LocateViews:=addr(xrLocateViews);
  end;
  if not assigned(xrEndFrame) then begin
   @xrEndFrame:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEndFrame'));
   @xr.fCommands.EndFrame:=addr(xrEndFrame);
  end;
  if not assigned(xrWaitFrame) then begin
   @xrWaitFrame:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrWaitFrame'));
   @xr.fCommands.WaitFrame:=addr(xrWaitFrame);
  end;
  if not assigned(xrApplyHapticFeedback) then begin
   @xrApplyHapticFeedback:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrApplyHapticFeedback'));
   @xr.fCommands.ApplyHapticFeedback:=addr(xrApplyHapticFeedback);
  end;
  if not assigned(xrStopHapticFeedback) then begin
   @xrStopHapticFeedback:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrStopHapticFeedback'));
   @xr.fCommands.StopHapticFeedback:=addr(xrStopHapticFeedback);
  end;
  if not assigned(xrPollEvent) then begin
   @xrPollEvent:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrPollEvent'));
   @xr.fCommands.PollEvent:=addr(xrPollEvent);
  end;
  if not assigned(xrStringToPath) then begin
   @xrStringToPath:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrStringToPath'));
   @xr.fCommands.StringToPath:=addr(xrStringToPath);
  end;
  if not assigned(xrPathToString) then begin
   @xrPathToString:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrPathToString'));
   @xr.fCommands.PathToString:=addr(xrPathToString);
  end;
  if not assigned(xrGetReferenceSpaceBoundsRect) then begin
   @xrGetReferenceSpaceBoundsRect:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetReferenceSpaceBoundsRect'));
   @xr.fCommands.GetReferenceSpaceBoundsRect:=addr(xrGetReferenceSpaceBoundsRect);
  end;
{$ifdef Android}
  if not assigned(xrSetAndroidApplicationThreadKHR) then begin
   @xrSetAndroidApplicationThreadKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetAndroidApplicationThreadKHR'));
   @xr.fCommands.SetAndroidApplicationThreadKHR:=addr(xrSetAndroidApplicationThreadKHR);
  end;
{$endif}
{$ifdef Android}
  if not assigned(xrCreateSwapchainAndroidSurfaceKHR) then begin
   @xrCreateSwapchainAndroidSurfaceKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSwapchainAndroidSurfaceKHR'));
   @xr.fCommands.CreateSwapchainAndroidSurfaceKHR:=addr(xrCreateSwapchainAndroidSurfaceKHR);
  end;
{$endif}
  if not assigned(xrGetActionStateBoolean) then begin
   @xrGetActionStateBoolean:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetActionStateBoolean'));
   @xr.fCommands.GetActionStateBoolean:=addr(xrGetActionStateBoolean);
  end;
  if not assigned(xrGetActionStateFloat) then begin
   @xrGetActionStateFloat:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetActionStateFloat'));
   @xr.fCommands.GetActionStateFloat:=addr(xrGetActionStateFloat);
  end;
  if not assigned(xrGetActionStateVector2f) then begin
   @xrGetActionStateVector2f:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetActionStateVector2f'));
   @xr.fCommands.GetActionStateVector2f:=addr(xrGetActionStateVector2f);
  end;
  if not assigned(xrGetActionStatePose) then begin
   @xrGetActionStatePose:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetActionStatePose'));
   @xr.fCommands.GetActionStatePose:=addr(xrGetActionStatePose);
  end;
  if not assigned(xrCreateActionSet) then begin
   @xrCreateActionSet:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateActionSet'));
   @xr.fCommands.CreateActionSet:=addr(xrCreateActionSet);
  end;
  if not assigned(xrDestroyActionSet) then begin
   @xrDestroyActionSet:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroyActionSet'));
   @xr.fCommands.DestroyActionSet:=addr(xrDestroyActionSet);
  end;
  if not assigned(xrCreateAction) then begin
   @xrCreateAction:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateAction'));
   @xr.fCommands.CreateAction:=addr(xrCreateAction);
  end;
  if not assigned(xrDestroyAction) then begin
   @xrDestroyAction:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroyAction'));
   @xr.fCommands.DestroyAction:=addr(xrDestroyAction);
  end;
  if not assigned(xrSuggestInteractionProfileBindings) then begin
   @xrSuggestInteractionProfileBindings:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSuggestInteractionProfileBindings'));
   @xr.fCommands.SuggestInteractionProfileBindings:=addr(xrSuggestInteractionProfileBindings);
  end;
  if not assigned(xrAttachSessionActionSets) then begin
   @xrAttachSessionActionSets:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrAttachSessionActionSets'));
   @xr.fCommands.AttachSessionActionSets:=addr(xrAttachSessionActionSets);
  end;
  if not assigned(xrGetCurrentInteractionProfile) then begin
   @xrGetCurrentInteractionProfile:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetCurrentInteractionProfile'));
   @xr.fCommands.GetCurrentInteractionProfile:=addr(xrGetCurrentInteractionProfile);
  end;
  if not assigned(xrSyncActions) then begin
   @xrSyncActions:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSyncActions'));
   @xr.fCommands.SyncActions:=addr(xrSyncActions);
  end;
  if not assigned(xrEnumerateBoundSourcesForAction) then begin
   @xrEnumerateBoundSourcesForAction:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateBoundSourcesForAction'));
   @xr.fCommands.EnumerateBoundSourcesForAction:=addr(xrEnumerateBoundSourcesForAction);
  end;
  if not assigned(xrGetInputSourceLocalizedName) then begin
   @xrGetInputSourceLocalizedName:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetInputSourceLocalizedName'));
   @xr.fCommands.GetInputSourceLocalizedName:=addr(xrGetInputSourceLocalizedName);
  end;
  if not assigned(xrGetVulkanInstanceExtensionsKHR) then begin
   @xrGetVulkanInstanceExtensionsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVulkanInstanceExtensionsKHR'));
   @xr.fCommands.GetVulkanInstanceExtensionsKHR:=addr(xrGetVulkanInstanceExtensionsKHR);
  end;
  if not assigned(xrGetVulkanDeviceExtensionsKHR) then begin
   @xrGetVulkanDeviceExtensionsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVulkanDeviceExtensionsKHR'));
   @xr.fCommands.GetVulkanDeviceExtensionsKHR:=addr(xrGetVulkanDeviceExtensionsKHR);
  end;
  if not assigned(xrGetVulkanGraphicsDeviceKHR) then begin
   @xrGetVulkanGraphicsDeviceKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVulkanGraphicsDeviceKHR'));
   @xr.fCommands.GetVulkanGraphicsDeviceKHR:=addr(xrGetVulkanGraphicsDeviceKHR);
  end;
  if not assigned(xrGetOpenGLGraphicsRequirementsKHR) then begin
   @xrGetOpenGLGraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetOpenGLGraphicsRequirementsKHR'));
   @xr.fCommands.GetOpenGLGraphicsRequirementsKHR:=addr(xrGetOpenGLGraphicsRequirementsKHR);
  end;
  if not assigned(xrGetOpenGLESGraphicsRequirementsKHR) then begin
   @xrGetOpenGLESGraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetOpenGLESGraphicsRequirementsKHR'));
   @xr.fCommands.GetOpenGLESGraphicsRequirementsKHR:=addr(xrGetOpenGLESGraphicsRequirementsKHR);
  end;
  if not assigned(xrGetVulkanGraphicsRequirementsKHR) then begin
   @xrGetVulkanGraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVulkanGraphicsRequirementsKHR'));
   @xr.fCommands.GetVulkanGraphicsRequirementsKHR:=addr(xrGetVulkanGraphicsRequirementsKHR);
  end;
  if not assigned(xrGetD3D11GraphicsRequirementsKHR) then begin
   @xrGetD3D11GraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetD3D11GraphicsRequirementsKHR'));
   @xr.fCommands.GetD3D11GraphicsRequirementsKHR:=addr(xrGetD3D11GraphicsRequirementsKHR);
  end;
  if not assigned(xrGetD3D12GraphicsRequirementsKHR) then begin
   @xrGetD3D12GraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetD3D12GraphicsRequirementsKHR'));
   @xr.fCommands.GetD3D12GraphicsRequirementsKHR:=addr(xrGetD3D12GraphicsRequirementsKHR);
  end;
  if not assigned(xrPerfSettingsSetPerformanceLevelEXT) then begin
   @xrPerfSettingsSetPerformanceLevelEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrPerfSettingsSetPerformanceLevelEXT'));
   @xr.fCommands.PerfSettingsSetPerformanceLevelEXT:=addr(xrPerfSettingsSetPerformanceLevelEXT);
  end;
  if not assigned(xrThermalGetTemperatureTrendEXT) then begin
   @xrThermalGetTemperatureTrendEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrThermalGetTemperatureTrendEXT'));
   @xr.fCommands.ThermalGetTemperatureTrendEXT:=addr(xrThermalGetTemperatureTrendEXT);
  end;
  if not assigned(xrSetDebugUtilsObjectNameEXT) then begin
   @xrSetDebugUtilsObjectNameEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetDebugUtilsObjectNameEXT'));
   @xr.fCommands.SetDebugUtilsObjectNameEXT:=addr(xrSetDebugUtilsObjectNameEXT);
  end;
  if not assigned(xrCreateDebugUtilsMessengerEXT) then begin
   @xrCreateDebugUtilsMessengerEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateDebugUtilsMessengerEXT'));
   @xr.fCommands.CreateDebugUtilsMessengerEXT:=addr(xrCreateDebugUtilsMessengerEXT);
  end;
  if not assigned(xrDestroyDebugUtilsMessengerEXT) then begin
   @xrDestroyDebugUtilsMessengerEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroyDebugUtilsMessengerEXT'));
   @xr.fCommands.DestroyDebugUtilsMessengerEXT:=addr(xrDestroyDebugUtilsMessengerEXT);
  end;
  if not assigned(xrSubmitDebugUtilsMessageEXT) then begin
   @xrSubmitDebugUtilsMessageEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSubmitDebugUtilsMessageEXT'));
   @xr.fCommands.SubmitDebugUtilsMessageEXT:=addr(xrSubmitDebugUtilsMessageEXT);
  end;
  if not assigned(xrSessionBeginDebugUtilsLabelRegionEXT) then begin
   @xrSessionBeginDebugUtilsLabelRegionEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSessionBeginDebugUtilsLabelRegionEXT'));
   @xr.fCommands.SessionBeginDebugUtilsLabelRegionEXT:=addr(xrSessionBeginDebugUtilsLabelRegionEXT);
  end;
  if not assigned(xrSessionEndDebugUtilsLabelRegionEXT) then begin
   @xrSessionEndDebugUtilsLabelRegionEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSessionEndDebugUtilsLabelRegionEXT'));
   @xr.fCommands.SessionEndDebugUtilsLabelRegionEXT:=addr(xrSessionEndDebugUtilsLabelRegionEXT);
  end;
  if not assigned(xrSessionInsertDebugUtilsLabelEXT) then begin
   @xrSessionInsertDebugUtilsLabelEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSessionInsertDebugUtilsLabelEXT'));
   @xr.fCommands.SessionInsertDebugUtilsLabelEXT:=addr(xrSessionInsertDebugUtilsLabelEXT);
  end;
{$ifdef Windows}
  if not assigned(xrConvertTimeToWin32PerformanceCounterKHR) then begin
   @xrConvertTimeToWin32PerformanceCounterKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrConvertTimeToWin32PerformanceCounterKHR'));
   @xr.fCommands.ConvertTimeToWin32PerformanceCounterKHR:=addr(xrConvertTimeToWin32PerformanceCounterKHR);
  end;
{$endif}
{$ifdef Windows}
  if not assigned(xrConvertWin32PerformanceCounterToTimeKHR) then begin
   @xrConvertWin32PerformanceCounterToTimeKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrConvertWin32PerformanceCounterToTimeKHR'));
   @xr.fCommands.ConvertWin32PerformanceCounterToTimeKHR:=addr(xrConvertWin32PerformanceCounterToTimeKHR);
  end;
{$endif}
  if not assigned(xrCreateVulkanInstanceKHR) then begin
   @xrCreateVulkanInstanceKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateVulkanInstanceKHR'));
   @xr.fCommands.CreateVulkanInstanceKHR:=addr(xrCreateVulkanInstanceKHR);
  end;
  if not assigned(xrCreateVulkanDeviceKHR) then begin
   @xrCreateVulkanDeviceKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateVulkanDeviceKHR'));
   @xr.fCommands.CreateVulkanDeviceKHR:=addr(xrCreateVulkanDeviceKHR);
  end;
  if not assigned(xrGetVulkanGraphicsDevice2KHR) then begin
   @xrGetVulkanGraphicsDevice2KHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVulkanGraphicsDevice2KHR'));
   @xr.fCommands.GetVulkanGraphicsDevice2KHR:=addr(xrGetVulkanGraphicsDevice2KHR);
  end;
  if not assigned(xrGetVulkanGraphicsRequirements2KHR) then begin
   @xrGetVulkanGraphicsRequirements2KHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVulkanGraphicsRequirements2KHR'));
   @xr.fCommands.GetVulkanGraphicsRequirements2KHR:=addr(xrGetVulkanGraphicsRequirements2KHR);
  end;
  if not assigned(xrConvertTimeToTimespecTimeKHR) then begin
   @xrConvertTimeToTimespecTimeKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrConvertTimeToTimespecTimeKHR'));
   @xr.fCommands.ConvertTimeToTimespecTimeKHR:=addr(xrConvertTimeToTimespecTimeKHR);
  end;
  if not assigned(xrConvertTimespecTimeToTimeKHR) then begin
   @xrConvertTimespecTimeToTimeKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrConvertTimespecTimeToTimeKHR'));
   @xr.fCommands.ConvertTimespecTimeToTimeKHR:=addr(xrConvertTimespecTimeToTimeKHR);
  end;
  if not assigned(xrGetVisibilityMaskKHR) then begin
   @xrGetVisibilityMaskKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetVisibilityMaskKHR'));
   @xr.fCommands.GetVisibilityMaskKHR:=addr(xrGetVisibilityMaskKHR);
  end;
  if not assigned(xrCreateSpatialAnchorMSFT) then begin
   @xrCreateSpatialAnchorMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSpatialAnchorMSFT'));
   @xr.fCommands.CreateSpatialAnchorMSFT:=addr(xrCreateSpatialAnchorMSFT);
  end;
  if not assigned(xrCreateSpatialAnchorSpaceMSFT) then begin
   @xrCreateSpatialAnchorSpaceMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSpatialAnchorSpaceMSFT'));
   @xr.fCommands.CreateSpatialAnchorSpaceMSFT:=addr(xrCreateSpatialAnchorSpaceMSFT);
  end;
  if not assigned(xrDestroySpatialAnchorMSFT) then begin
   @xrDestroySpatialAnchorMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroySpatialAnchorMSFT'));
   @xr.fCommands.DestroySpatialAnchorMSFT:=addr(xrDestroySpatialAnchorMSFT);
  end;
  if not assigned(xrSetInputDeviceActiveEXT) then begin
   @xrSetInputDeviceActiveEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetInputDeviceActiveEXT'));
   @xr.fCommands.SetInputDeviceActiveEXT:=addr(xrSetInputDeviceActiveEXT);
  end;
  if not assigned(xrSetInputDeviceStateBoolEXT) then begin
   @xrSetInputDeviceStateBoolEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetInputDeviceStateBoolEXT'));
   @xr.fCommands.SetInputDeviceStateBoolEXT:=addr(xrSetInputDeviceStateBoolEXT);
  end;
  if not assigned(xrSetInputDeviceStateFloatEXT) then begin
   @xrSetInputDeviceStateFloatEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetInputDeviceStateFloatEXT'));
   @xr.fCommands.SetInputDeviceStateFloatEXT:=addr(xrSetInputDeviceStateFloatEXT);
  end;
  if not assigned(xrSetInputDeviceStateVector2fEXT) then begin
   @xrSetInputDeviceStateVector2fEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetInputDeviceStateVector2fEXT'));
   @xr.fCommands.SetInputDeviceStateVector2fEXT:=addr(xrSetInputDeviceStateVector2fEXT);
  end;
  if not assigned(xrSetInputDeviceLocationEXT) then begin
   @xrSetInputDeviceLocationEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetInputDeviceLocationEXT'));
   @xr.fCommands.SetInputDeviceLocationEXT:=addr(xrSetInputDeviceLocationEXT);
  end;
  if not assigned(xrInitializeLoaderKHR) then begin
   @xrInitializeLoaderKHR:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrInitializeLoaderKHR'));
   @xr.fCommands.InitializeLoaderKHR:=addr(xrInitializeLoaderKHR);
  end;
  if not assigned(xrCreateSpatialGraphNodeSpaceMSFT) then begin
   @xrCreateSpatialGraphNodeSpaceMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSpatialGraphNodeSpaceMSFT'));
   @xr.fCommands.CreateSpatialGraphNodeSpaceMSFT:=addr(xrCreateSpatialGraphNodeSpaceMSFT);
  end;
  if not assigned(xrCreateHandTrackerEXT) then begin
   @xrCreateHandTrackerEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateHandTrackerEXT'));
   @xr.fCommands.CreateHandTrackerEXT:=addr(xrCreateHandTrackerEXT);
  end;
  if not assigned(xrDestroyHandTrackerEXT) then begin
   @xrDestroyHandTrackerEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroyHandTrackerEXT'));
   @xr.fCommands.DestroyHandTrackerEXT:=addr(xrDestroyHandTrackerEXT);
  end;
  if not assigned(xrLocateHandJointsEXT) then begin
   @xrLocateHandJointsEXT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrLocateHandJointsEXT'));
   @xr.fCommands.LocateHandJointsEXT:=addr(xrLocateHandJointsEXT);
  end;
  if not assigned(xrCreateHandMeshSpaceMSFT) then begin
   @xrCreateHandMeshSpaceMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateHandMeshSpaceMSFT'));
   @xr.fCommands.CreateHandMeshSpaceMSFT:=addr(xrCreateHandMeshSpaceMSFT);
  end;
  if not assigned(xrUpdateHandMeshMSFT) then begin
   @xrUpdateHandMeshMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrUpdateHandMeshMSFT'));
   @xr.fCommands.UpdateHandMeshMSFT:=addr(xrUpdateHandMeshMSFT);
  end;
  if not assigned(xrGetControllerModelKeyMSFT) then begin
   @xrGetControllerModelKeyMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetControllerModelKeyMSFT'));
   @xr.fCommands.GetControllerModelKeyMSFT:=addr(xrGetControllerModelKeyMSFT);
  end;
  if not assigned(xrLoadControllerModelMSFT) then begin
   @xrLoadControllerModelMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrLoadControllerModelMSFT'));
   @xr.fCommands.LoadControllerModelMSFT:=addr(xrLoadControllerModelMSFT);
  end;
  if not assigned(xrGetControllerModelPropertiesMSFT) then begin
   @xrGetControllerModelPropertiesMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetControllerModelPropertiesMSFT'));
   @xr.fCommands.GetControllerModelPropertiesMSFT:=addr(xrGetControllerModelPropertiesMSFT);
  end;
  if not assigned(xrGetControllerModelStateMSFT) then begin
   @xrGetControllerModelStateMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetControllerModelStateMSFT'));
   @xr.fCommands.GetControllerModelStateMSFT:=addr(xrGetControllerModelStateMSFT);
  end;
  if not assigned(xrEnumerateSceneComputeFeaturesMSFT) then begin
   @xrEnumerateSceneComputeFeaturesMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateSceneComputeFeaturesMSFT'));
   @xr.fCommands.EnumerateSceneComputeFeaturesMSFT:=addr(xrEnumerateSceneComputeFeaturesMSFT);
  end;
  if not assigned(xrCreateSceneObserverMSFT) then begin
   @xrCreateSceneObserverMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSceneObserverMSFT'));
   @xr.fCommands.CreateSceneObserverMSFT:=addr(xrCreateSceneObserverMSFT);
  end;
  if not assigned(xrDestroySceneObserverMSFT) then begin
   @xrDestroySceneObserverMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroySceneObserverMSFT'));
   @xr.fCommands.DestroySceneObserverMSFT:=addr(xrDestroySceneObserverMSFT);
  end;
  if not assigned(xrCreateSceneMSFT) then begin
   @xrCreateSceneMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSceneMSFT'));
   @xr.fCommands.CreateSceneMSFT:=addr(xrCreateSceneMSFT);
  end;
  if not assigned(xrDestroySceneMSFT) then begin
   @xrDestroySceneMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDestroySceneMSFT'));
   @xr.fCommands.DestroySceneMSFT:=addr(xrDestroySceneMSFT);
  end;
  if not assigned(xrComputeNewSceneMSFT) then begin
   @xrComputeNewSceneMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrComputeNewSceneMSFT'));
   @xr.fCommands.ComputeNewSceneMSFT:=addr(xrComputeNewSceneMSFT);
  end;
  if not assigned(xrGetSceneComputeStateMSFT) then begin
   @xrGetSceneComputeStateMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSceneComputeStateMSFT'));
   @xr.fCommands.GetSceneComputeStateMSFT:=addr(xrGetSceneComputeStateMSFT);
  end;
  if not assigned(xrGetSceneComponentsMSFT) then begin
   @xrGetSceneComponentsMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSceneComponentsMSFT'));
   @xr.fCommands.GetSceneComponentsMSFT:=addr(xrGetSceneComponentsMSFT);
  end;
  if not assigned(xrLocateSceneComponentsMSFT) then begin
   @xrLocateSceneComponentsMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrLocateSceneComponentsMSFT'));
   @xr.fCommands.LocateSceneComponentsMSFT:=addr(xrLocateSceneComponentsMSFT);
  end;
  if not assigned(xrGetSceneMeshBuffersMSFT) then begin
   @xrGetSceneMeshBuffersMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSceneMeshBuffersMSFT'));
   @xr.fCommands.GetSceneMeshBuffersMSFT:=addr(xrGetSceneMeshBuffersMSFT);
  end;
  if not assigned(xrDeserializeSceneMSFT) then begin
   @xrDeserializeSceneMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrDeserializeSceneMSFT'));
   @xr.fCommands.DeserializeSceneMSFT:=addr(xrDeserializeSceneMSFT);
  end;
  if not assigned(xrGetSerializedSceneFragmentDataMSFT) then begin
   @xrGetSerializedSceneFragmentDataMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSerializedSceneFragmentDataMSFT'));
   @xr.fCommands.GetSerializedSceneFragmentDataMSFT:=addr(xrGetSerializedSceneFragmentDataMSFT);
  end;
  if not assigned(xrEnumerateDisplayRefreshRatesFB) then begin
   @xrEnumerateDisplayRefreshRatesFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateDisplayRefreshRatesFB'));
   @xr.fCommands.EnumerateDisplayRefreshRatesFB:=addr(xrEnumerateDisplayRefreshRatesFB);
  end;
  if not assigned(xrGetDisplayRefreshRateFB) then begin
   @xrGetDisplayRefreshRateFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetDisplayRefreshRateFB'));
   @xr.fCommands.GetDisplayRefreshRateFB:=addr(xrGetDisplayRefreshRateFB);
  end;
  if not assigned(xrRequestDisplayRefreshRateFB) then begin
   @xrRequestDisplayRefreshRateFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrRequestDisplayRefreshRateFB'));
   @xr.fCommands.RequestDisplayRefreshRateFB:=addr(xrRequestDisplayRefreshRateFB);
  end;
  if not assigned(xrCreateSpatialAnchorFromPerceptionAnchorMSFT) then begin
   @xrCreateSpatialAnchorFromPerceptionAnchorMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrCreateSpatialAnchorFromPerceptionAnchorMSFT'));
   @xr.fCommands.CreateSpatialAnchorFromPerceptionAnchorMSFT:=addr(xrCreateSpatialAnchorFromPerceptionAnchorMSFT);
  end;
  if not assigned(xrTryGetPerceptionAnchorFromSpatialAnchorMSFT) then begin
   @xrTryGetPerceptionAnchorFromSpatialAnchorMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrTryGetPerceptionAnchorFromSpatialAnchorMSFT'));
   @xr.fCommands.TryGetPerceptionAnchorFromSpatialAnchorMSFT:=addr(xrTryGetPerceptionAnchorFromSpatialAnchorMSFT);
  end;
  if not assigned(xrUpdateSwapchainFB) then begin
   @xrUpdateSwapchainFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrUpdateSwapchainFB'));
   @xr.fCommands.UpdateSwapchainFB:=addr(xrUpdateSwapchainFB);
  end;
  if not assigned(xrGetSwapchainStateFB) then begin
   @xrGetSwapchainStateFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetSwapchainStateFB'));
   @xr.fCommands.GetSwapchainStateFB:=addr(xrGetSwapchainStateFB);
  end;
  if not assigned(xrEnumerateColorSpacesFB) then begin
   @xrEnumerateColorSpacesFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateColorSpacesFB'));
   @xr.fCommands.EnumerateColorSpacesFB:=addr(xrEnumerateColorSpacesFB);
  end;
  if not assigned(xrSetColorSpaceFB) then begin
   @xrSetColorSpaceFB:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetColorSpaceFB'));
   @xr.fCommands.SetColorSpaceFB:=addr(xrSetColorSpaceFB);
  end;
  if not assigned(xrSetEnvironmentDepthEstimationVARJO) then begin
   @xrSetEnvironmentDepthEstimationVARJO:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrSetEnvironmentDepthEstimationVARJO'));
   @xr.fCommands.SetEnvironmentDepthEstimationVARJO:=addr(xrSetEnvironmentDepthEstimationVARJO);
  end;
  if not assigned(xrEnumerateReprojectionModesMSFT) then begin
   @xrEnumerateReprojectionModesMSFT:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrEnumerateReprojectionModesMSFT'));
   @xr.fCommands.EnumerateReprojectionModesMSFT:=addr(xrEnumerateReprojectionModesMSFT);
  end;
  if not assigned(xrGetAudioOutputDeviceGuidOculus) then begin
   @xrGetAudioOutputDeviceGuidOculus:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetAudioOutputDeviceGuidOculus'));
   @xr.fCommands.GetAudioOutputDeviceGuidOculus:=addr(xrGetAudioOutputDeviceGuidOculus);
  end;
  if not assigned(xrGetAudioInputDeviceGuidOculus) then begin
   @xrGetAudioInputDeviceGuidOculus:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'xrGetAudioInputDeviceGuidOculus'));
   @xr.fCommands.GetAudioInputDeviceGuidOculus:=addr(xrGetAudioInputDeviceGuidOculus);
  end;
  result:=assigned(xrCreateInstance);
 end;
end;

function LoadOpenXRInstanceCommands(const GetInstanceProcAddr:TxrGetInstanceProcAddr;const Instance:TXrInstance;out InstanceCommands:TOpenXRCommands):boolean;
begin
 FillChar(InstanceCommands,SizeOf(TOpenXRCommands),#0);
 result:=assigned(GetInstanceProcAddr);
 if result then begin
  @InstanceCommands.GetInstanceProcAddr:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetInstanceProcAddr')));
  @InstanceCommands.EnumerateApiLayerProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateApiLayerProperties')));
  @InstanceCommands.EnumerateInstanceExtensionProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateInstanceExtensionProperties')));
  @InstanceCommands.CreateInstance:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateInstance')));
  @InstanceCommands.DestroyInstance:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroyInstance')));
  @InstanceCommands.ResultToString:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrResultToString')));
  @InstanceCommands.StructureTypeToString:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrStructureTypeToString')));
  @InstanceCommands.GetInstanceProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetInstanceProperties')));
  @InstanceCommands.GetSystem:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSystem')));
  @InstanceCommands.GetSystemProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSystemProperties')));
  @InstanceCommands.CreateSession:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSession')));
  @InstanceCommands.DestroySession:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroySession')));
  @InstanceCommands.DestroySpace:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroySpace')));
  @InstanceCommands.EnumerateSwapchainFormats:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateSwapchainFormats')));
  @InstanceCommands.CreateSwapchain:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSwapchain')));
  @InstanceCommands.DestroySwapchain:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroySwapchain')));
  @InstanceCommands.EnumerateSwapchainImages:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateSwapchainImages')));
  @InstanceCommands.AcquireSwapchainImage:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrAcquireSwapchainImage')));
  @InstanceCommands.WaitSwapchainImage:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrWaitSwapchainImage')));
  @InstanceCommands.ReleaseSwapchainImage:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrReleaseSwapchainImage')));
  @InstanceCommands.BeginSession:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrBeginSession')));
  @InstanceCommands.EndSession:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEndSession')));
  @InstanceCommands.RequestExitSession:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrRequestExitSession')));
  @InstanceCommands.EnumerateReferenceSpaces:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateReferenceSpaces')));
  @InstanceCommands.CreateReferenceSpace:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateReferenceSpace')));
  @InstanceCommands.CreateActionSpace:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateActionSpace')));
  @InstanceCommands.LocateSpace:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrLocateSpace')));
  @InstanceCommands.EnumerateViewConfigurations:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateViewConfigurations')));
  @InstanceCommands.EnumerateEnvironmentBlendModes:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateEnvironmentBlendModes')));
  @InstanceCommands.GetViewConfigurationProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetViewConfigurationProperties')));
  @InstanceCommands.EnumerateViewConfigurationViews:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateViewConfigurationViews')));
  @InstanceCommands.BeginFrame:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrBeginFrame')));
  @InstanceCommands.LocateViews:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrLocateViews')));
  @InstanceCommands.EndFrame:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEndFrame')));
  @InstanceCommands.WaitFrame:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrWaitFrame')));
  @InstanceCommands.ApplyHapticFeedback:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrApplyHapticFeedback')));
  @InstanceCommands.StopHapticFeedback:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrStopHapticFeedback')));
  @InstanceCommands.PollEvent:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrPollEvent')));
  @InstanceCommands.StringToPath:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrStringToPath')));
  @InstanceCommands.PathToString:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrPathToString')));
  @InstanceCommands.GetReferenceSpaceBoundsRect:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetReferenceSpaceBoundsRect')));
{$ifdef Android}
  @InstanceCommands.SetAndroidApplicationThreadKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetAndroidApplicationThreadKHR')));
{$endif}
{$ifdef Android}
  @InstanceCommands.CreateSwapchainAndroidSurfaceKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSwapchainAndroidSurfaceKHR')));
{$endif}
  @InstanceCommands.GetActionStateBoolean:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetActionStateBoolean')));
  @InstanceCommands.GetActionStateFloat:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetActionStateFloat')));
  @InstanceCommands.GetActionStateVector2f:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetActionStateVector2f')));
  @InstanceCommands.GetActionStatePose:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetActionStatePose')));
  @InstanceCommands.CreateActionSet:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateActionSet')));
  @InstanceCommands.DestroyActionSet:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroyActionSet')));
  @InstanceCommands.CreateAction:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateAction')));
  @InstanceCommands.DestroyAction:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroyAction')));
  @InstanceCommands.SuggestInteractionProfileBindings:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSuggestInteractionProfileBindings')));
  @InstanceCommands.AttachSessionActionSets:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrAttachSessionActionSets')));
  @InstanceCommands.GetCurrentInteractionProfile:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetCurrentInteractionProfile')));
  @InstanceCommands.SyncActions:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSyncActions')));
  @InstanceCommands.EnumerateBoundSourcesForAction:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateBoundSourcesForAction')));
  @InstanceCommands.GetInputSourceLocalizedName:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetInputSourceLocalizedName')));
  @InstanceCommands.GetVulkanInstanceExtensionsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVulkanInstanceExtensionsKHR')));
  @InstanceCommands.GetVulkanDeviceExtensionsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVulkanDeviceExtensionsKHR')));
  @InstanceCommands.GetVulkanGraphicsDeviceKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVulkanGraphicsDeviceKHR')));
  @InstanceCommands.GetOpenGLGraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetOpenGLGraphicsRequirementsKHR')));
  @InstanceCommands.GetOpenGLESGraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetOpenGLESGraphicsRequirementsKHR')));
  @InstanceCommands.GetVulkanGraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVulkanGraphicsRequirementsKHR')));
  @InstanceCommands.GetD3D11GraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetD3D11GraphicsRequirementsKHR')));
  @InstanceCommands.GetD3D12GraphicsRequirementsKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetD3D12GraphicsRequirementsKHR')));
  @InstanceCommands.PerfSettingsSetPerformanceLevelEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrPerfSettingsSetPerformanceLevelEXT')));
  @InstanceCommands.ThermalGetTemperatureTrendEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrThermalGetTemperatureTrendEXT')));
  @InstanceCommands.SetDebugUtilsObjectNameEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetDebugUtilsObjectNameEXT')));
  @InstanceCommands.CreateDebugUtilsMessengerEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateDebugUtilsMessengerEXT')));
  @InstanceCommands.DestroyDebugUtilsMessengerEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroyDebugUtilsMessengerEXT')));
  @InstanceCommands.SubmitDebugUtilsMessageEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSubmitDebugUtilsMessageEXT')));
  @InstanceCommands.SessionBeginDebugUtilsLabelRegionEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSessionBeginDebugUtilsLabelRegionEXT')));
  @InstanceCommands.SessionEndDebugUtilsLabelRegionEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSessionEndDebugUtilsLabelRegionEXT')));
  @InstanceCommands.SessionInsertDebugUtilsLabelEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSessionInsertDebugUtilsLabelEXT')));
{$ifdef Windows}
  @InstanceCommands.ConvertTimeToWin32PerformanceCounterKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrConvertTimeToWin32PerformanceCounterKHR')));
{$endif}
{$ifdef Windows}
  @InstanceCommands.ConvertWin32PerformanceCounterToTimeKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrConvertWin32PerformanceCounterToTimeKHR')));
{$endif}
  @InstanceCommands.CreateVulkanInstanceKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateVulkanInstanceKHR')));
  @InstanceCommands.CreateVulkanDeviceKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateVulkanDeviceKHR')));
  @InstanceCommands.GetVulkanGraphicsDevice2KHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVulkanGraphicsDevice2KHR')));
  @InstanceCommands.GetVulkanGraphicsRequirements2KHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVulkanGraphicsRequirements2KHR')));
  @InstanceCommands.ConvertTimeToTimespecTimeKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrConvertTimeToTimespecTimeKHR')));
  @InstanceCommands.ConvertTimespecTimeToTimeKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrConvertTimespecTimeToTimeKHR')));
  @InstanceCommands.GetVisibilityMaskKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetVisibilityMaskKHR')));
  @InstanceCommands.CreateSpatialAnchorMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSpatialAnchorMSFT')));
  @InstanceCommands.CreateSpatialAnchorSpaceMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSpatialAnchorSpaceMSFT')));
  @InstanceCommands.DestroySpatialAnchorMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroySpatialAnchorMSFT')));
  @InstanceCommands.SetInputDeviceActiveEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetInputDeviceActiveEXT')));
  @InstanceCommands.SetInputDeviceStateBoolEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetInputDeviceStateBoolEXT')));
  @InstanceCommands.SetInputDeviceStateFloatEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetInputDeviceStateFloatEXT')));
  @InstanceCommands.SetInputDeviceStateVector2fEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetInputDeviceStateVector2fEXT')));
  @InstanceCommands.SetInputDeviceLocationEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetInputDeviceLocationEXT')));
  @InstanceCommands.InitializeLoaderKHR:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrInitializeLoaderKHR')));
  @InstanceCommands.CreateSpatialGraphNodeSpaceMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSpatialGraphNodeSpaceMSFT')));
  @InstanceCommands.CreateHandTrackerEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateHandTrackerEXT')));
  @InstanceCommands.DestroyHandTrackerEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroyHandTrackerEXT')));
  @InstanceCommands.LocateHandJointsEXT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrLocateHandJointsEXT')));
  @InstanceCommands.CreateHandMeshSpaceMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateHandMeshSpaceMSFT')));
  @InstanceCommands.UpdateHandMeshMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrUpdateHandMeshMSFT')));
  @InstanceCommands.GetControllerModelKeyMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetControllerModelKeyMSFT')));
  @InstanceCommands.LoadControllerModelMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrLoadControllerModelMSFT')));
  @InstanceCommands.GetControllerModelPropertiesMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetControllerModelPropertiesMSFT')));
  @InstanceCommands.GetControllerModelStateMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetControllerModelStateMSFT')));
  @InstanceCommands.EnumerateSceneComputeFeaturesMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateSceneComputeFeaturesMSFT')));
  @InstanceCommands.CreateSceneObserverMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSceneObserverMSFT')));
  @InstanceCommands.DestroySceneObserverMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroySceneObserverMSFT')));
  @InstanceCommands.CreateSceneMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSceneMSFT')));
  @InstanceCommands.DestroySceneMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDestroySceneMSFT')));
  @InstanceCommands.ComputeNewSceneMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrComputeNewSceneMSFT')));
  @InstanceCommands.GetSceneComputeStateMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSceneComputeStateMSFT')));
  @InstanceCommands.GetSceneComponentsMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSceneComponentsMSFT')));
  @InstanceCommands.LocateSceneComponentsMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrLocateSceneComponentsMSFT')));
  @InstanceCommands.GetSceneMeshBuffersMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSceneMeshBuffersMSFT')));
  @InstanceCommands.DeserializeSceneMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrDeserializeSceneMSFT')));
  @InstanceCommands.GetSerializedSceneFragmentDataMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSerializedSceneFragmentDataMSFT')));
  @InstanceCommands.EnumerateDisplayRefreshRatesFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateDisplayRefreshRatesFB')));
  @InstanceCommands.GetDisplayRefreshRateFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetDisplayRefreshRateFB')));
  @InstanceCommands.RequestDisplayRefreshRateFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrRequestDisplayRefreshRateFB')));
  @InstanceCommands.CreateSpatialAnchorFromPerceptionAnchorMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrCreateSpatialAnchorFromPerceptionAnchorMSFT')));
  @InstanceCommands.TryGetPerceptionAnchorFromSpatialAnchorMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrTryGetPerceptionAnchorFromSpatialAnchorMSFT')));
  @InstanceCommands.UpdateSwapchainFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrUpdateSwapchainFB')));
  @InstanceCommands.GetSwapchainStateFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetSwapchainStateFB')));
  @InstanceCommands.EnumerateColorSpacesFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateColorSpacesFB')));
  @InstanceCommands.SetColorSpaceFB:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetColorSpaceFB')));
  @InstanceCommands.SetEnvironmentDepthEstimationVARJO:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrSetEnvironmentDepthEstimationVARJO')));
  @InstanceCommands.EnumerateReprojectionModesMSFT:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrEnumerateReprojectionModesMSFT')));
  @InstanceCommands.GetAudioOutputDeviceGuidOculus:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetAudioOutputDeviceGuidOculus')));
  @InstanceCommands.GetAudioInputDeviceGuidOculus:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(Instance,PXrChar('xrGetAudioInputDeviceGuidOculus')));
  if not assigned(InstanceCommands.EnumerateInstanceExtensionProperties) then begin
   InstanceCommands.EnumerateInstanceExtensionProperties:=addr(xrEnumerateInstanceExtensionProperties);
  end;
  if not assigned(InstanceCommands.EnumerateInstanceLayerProperties) then begin
   InstanceCommands.EnumerateInstanceLayerProperties:=addr(xrEnumerateInstanceLayerProperties);
  end;
  if not assigned(InstanceCommands.CreateInstance) then begin
   InstanceCommands.CreateInstance:=addr(xrCreateInstance);
  end;
  result:=assigned(InstanceCommands.DestroyInstance);
 end;
end;

function LoadOpenXRDeviceCommands(const GetDeviceProcAddr:TxrGetDeviceProcAddr;const Device:TXrDevice;out DeviceCommands:TOpenXRCommands):boolean;
begin
 FillChar(DeviceCommands,SizeOf(TOpenXRCommands),#0);
 result:=assigned(GetDeviceProcAddr);
 if result then begin
  // Device commands of any OpenXR command whose first parameter is one of: xrDevice, XrQueue, XrCommandBuffer
  result:=assigned(DeviceCommands.DestroyDevice);
 end;
end;

{$ifdef HAS_ADVANCED_RECORDS}
constructor TXrVector2f.Create(const aX:TXrFloat;
                               const aY:TXrFloat);
begin
 x:=aX;
 y:=aY;
end;

constructor TXrVector3f.Create(const aX:TXrFloat;
                               const aY:TXrFloat;
                               const aZ:TXrFloat);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
end;

constructor TXrVector4f.Create(const aX:TXrFloat;
                               const aY:TXrFloat;
                               const aZ:TXrFloat;
                               const aW:TXrFloat);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
 w:=aW;
end;

constructor TXrColor4f.Create(const aR:TXrFloat;
                              const aG:TXrFloat;
                              const aB:TXrFloat;
                              const aA:TXrFloat);
begin
 r:=aR;
 g:=aG;
 b:=aB;
 a:=aA;
end;

constructor TXrQuaternionf.Create(const aX:TXrFloat;
                                  const aY:TXrFloat;
                                  const aZ:TXrFloat;
                                  const aW:TXrFloat);
begin
 x:=aX;
 y:=aY;
 z:=aZ;
 w:=aW;
end;

constructor TXrPosef.Create(const aOrientation:TXrQuaternionf;
                            const aPosition:TXrVector3f);
begin
 orientation:=aOrientation;
 position:=aPosition;
end;

constructor TXrOffset2Df.Create(const aX:TXrFloat;
                                const aY:TXrFloat);
begin
 x:=aX;
 y:=aY;
end;

constructor TXrExtent2Df.Create(const aWidth:TXrFloat;
                                const aHeight:TXrFloat);
begin
 width:=aWidth;
 height:=aHeight;
end;

constructor TXrRect2Df.Create(const aOffset:TXrOffset2Df;
                              const aExtent:TXrExtent2Df);
begin
 offset:=aOffset;
 extent:=aExtent;
end;

constructor TXrOffset2Di.Create(const aX:TXrInt32;
                                const aY:TXrInt32);
begin
 x:=aX;
 y:=aY;
end;

constructor TXrExtent2Di.Create(const aWidth:TXrInt32;
                                const aHeight:TXrInt32);
begin
 width:=aWidth;
 height:=aHeight;
end;

constructor TXrRect2Di.Create(const aOffset:TXrOffset2Di;
                              const aExtent:TXrExtent2Di);
begin
 offset:=aOffset;
 extent:=aExtent;
end;

constructor TXrApiLayerProperties.Create(const aLayerName:TXrCharString;
                                         const aSpecVersion:TXrVersion;
                                         const aLayerVersion:TXrUInt32;
                                         const aDescription:TXrCharString);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrApiLayerProperties),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 ArrayItemCount:=length(aLayerName);
 if ArrayItemCount>length(layerName) then begin
  ArrayItemCount:=length(layerName);
 end;
 if ArrayItemCount>0 then begin
  Move(aLayerName[1],layerName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 specVersion:=aSpecVersion;
 layerVersion:=aLayerVersion;
 ArrayItemCount:=length(aDescription);
 if ArrayItemCount>length(description) then begin
  ArrayItemCount:=length(description);
 end;
 if ArrayItemCount>0 then begin
  Move(aDescription[1],description[0],ArrayItemCount*SizeOf(TXrChar));
 end;
end;

constructor TXrExtensionProperties.Create(const aExtensionName:TXrCharString;
                                          const aExtensionVersion:TXrUInt32);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrExtensionProperties),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 ArrayItemCount:=length(aExtensionName);
 if ArrayItemCount>length(extensionName) then begin
  ArrayItemCount:=length(extensionName);
 end;
 if ArrayItemCount>0 then begin
  Move(aExtensionName[1],extensionName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 extensionVersion:=aExtensionVersion;
end;

constructor TXrApplicationInfo.Create(const aApplicationName:TXrCharString;
                                      const aApplicationVersion:TXrUInt32;
                                      const aEngineName:TXrCharString;
                                      const aEngineVersion:TXrUInt32;
                                      const aApiVersion:TXrVersion);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrApplicationInfo),#0);
 ArrayItemCount:=length(aApplicationName);
 if ArrayItemCount>length(applicationName) then begin
  ArrayItemCount:=length(applicationName);
 end;
 if ArrayItemCount>0 then begin
  Move(aApplicationName[1],applicationName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 applicationVersion:=aApplicationVersion;
 ArrayItemCount:=length(aEngineName);
 if ArrayItemCount>length(engineName) then begin
  ArrayItemCount:=length(engineName);
 end;
 if ArrayItemCount>0 then begin
  Move(aEngineName[1],engineName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 engineVersion:=aEngineVersion;
 apiVersion:=aApiVersion;
end;

constructor TXrInstanceCreateInfo.Create(const aCreateFlags:TXrInstanceCreateFlags;
                                         const aApplicationInfo:TXrApplicationInfo;
                                         const aEnabledApiLayerCount:TXrUInt32;
                                         const aEnabledApiLayerNames:PPXrChar;
                                         const aEnabledExtensionCount:TXrUInt32;
                                         const aEnabledExtensionNames:PPXrChar);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 createFlags:=aCreateFlags;
 applicationInfo:=aApplicationInfo;
 enabledApiLayerCount:=aEnabledApiLayerCount;
 enabledApiLayerNames:=aEnabledApiLayerNames;
 enabledExtensionCount:=aEnabledExtensionCount;
 enabledExtensionNames:=aEnabledExtensionNames;
end;

constructor TXrInstanceProperties.Create(const aRuntimeVersion:TXrVersion;
                                         const aRuntimeName:TXrCharString);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrInstanceProperties),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 runtimeVersion:=aRuntimeVersion;
 ArrayItemCount:=length(aRuntimeName);
 if ArrayItemCount>length(runtimeName) then begin
  ArrayItemCount:=length(runtimeName);
 end;
 if ArrayItemCount>0 then begin
  Move(aRuntimeName[1],runtimeName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
end;

constructor TXrSystemGetInfo.Create(const aFormFactor:TXrFormFactor);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 formFactor:=aFormFactor;
end;

constructor TXrSystemGraphicsProperties.Create(const aMaxSwapchainImageHeight:TXrUInt32;
                                               const aMaxSwapchainImageWidth:TXrUInt32;
                                               const aMaxLayerCount:TXrUInt32);
begin
 maxSwapchainImageHeight:=aMaxSwapchainImageHeight;
 maxSwapchainImageWidth:=aMaxSwapchainImageWidth;
 maxLayerCount:=aMaxLayerCount;
end;

constructor TXrSystemTrackingProperties.Create(const aOrientationTracking:TXrBool32;
                                               const aPositionTracking:TXrBool32);
begin
 orientationTracking:=aOrientationTracking;
 positionTracking:=aPositionTracking;
end;

constructor TXrSystemProperties.Create(const aSystemId:TXrSystemId;
                                       const aVendorId:TXrUInt32;
                                       const aSystemName:TXrCharString;
                                       const aGraphicsProperties:TXrSystemGraphicsProperties;
                                       const aTrackingProperties:TXrSystemTrackingProperties);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrSystemProperties),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 systemId:=aSystemId;
 vendorId:=aVendorId;
 ArrayItemCount:=length(aSystemName);
 if ArrayItemCount>length(systemName) then begin
  ArrayItemCount:=length(systemName);
 end;
 if ArrayItemCount>0 then begin
  Move(aSystemName[1],systemName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 graphicsProperties:=aGraphicsProperties;
 trackingProperties:=aTrackingProperties;
end;

constructor TXrGraphicsBindingOpenGLWin32KHR.Create(const aHDC:TXrHDC;
                                                    const aHGLRC:TXrHGLRC);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 hDC:=aHDC;
 hGLRC:=aHGLRC;
end;

{$ifdef XLIB}
constructor TXrGraphicsBindingOpenGLXlibKHR.Create(const aXDisplay:PXrXLIBDisplay;
                                                   const aVisualid:TXrUInt32;
                                                   const aGlxFBConfig:TGLXFBConfig;
                                                   const aGlxDrawable:TGLXDrawable;
                                                   const aGlxContext:TGLXContext);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 xDisplay:=aXDisplay;
 visualid:=aVisualid;
 glxFBConfig:=aGlxFBConfig;
 glxDrawable:=aGlxDrawable;
 glxContext:=aGlxContext;
end;
{$endif}

{$ifdef XCB}
constructor TXrGraphicsBindingOpenGLXcbKHR.Create(const aConnection:PXrXCBConnection;
                                                  const aScreenNumber:TXrUInt32;
                                                  const aFbconfigid:Txcb_glx_fbconfig_t;
                                                  const aVisualid:TXrXCBVisualID;
                                                  const aGlxDrawable:Txcb_glx_drawable_t;
                                                  const aGlxContext:Txcb_glx_context_t);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 connection:=aConnection;
 screenNumber:=aScreenNumber;
 fbconfigid:=aFbconfigid;
 visualid:=aVisualid;
 glxDrawable:=aGlxDrawable;
 glxContext:=aGlxContext;
end;
{$endif}

{$ifdef Wayland}
constructor TXrGraphicsBindingOpenGLWaylandKHR.Create(const aDisplay:PXrWaylandDisplay);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 display:=aDisplay;
end;
{$endif}

constructor TXrGraphicsBindingD3D11KHR.Create(const aDevice:ID3D11Device);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 device:=aDevice;
end;

constructor TXrGraphicsBindingD3D12KHR.Create(const aDevice:ID3D12Device;
                                              const aQueue:ID3D12CommandQueue);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 device:=aDevice;
 queue:=aQueue;
end;

{$ifdef Android}
constructor TXrGraphicsBindingOpenGLESAndroidKHR.Create(const aDisplay:TEGLDisplay;
                                                        const aConfig:TEGLConfig;
                                                        const aContext:TEGLContext);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 display:=aDisplay;
 config:=aConfig;
 context:=aContext;
end;
{$endif}

constructor TXrGraphicsBindingVulkanKHR.Create(const aInstance:TVkInstance;
                                               const aPhysicalDevice:TVkPhysicalDevice;
                                               const aDevice:TVkDevice;
                                               const aQueueFamilyIndex:TXrUInt32;
                                               const aQueueIndex:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 instance:=aInstance;
 physicalDevice:=aPhysicalDevice;
 device:=aDevice;
 queueFamilyIndex:=aQueueFamilyIndex;
 queueIndex:=aQueueIndex;
end;

constructor TXrSessionCreateInfo.Create(const aCreateFlags:TXrSessionCreateFlags;
                                        const aSystemId:TXrSystemId);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 createFlags:=aCreateFlags;
 systemId:=aSystemId;
end;

constructor TXrSessionBeginInfo.Create(const aPrimaryViewConfigurationType:TXrViewConfigurationType);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 primaryViewConfigurationType:=aPrimaryViewConfigurationType;
end;

constructor TXrSwapchainCreateInfo.Create(const aCreateFlags:TXrSwapchainCreateFlags;
                                          const aUsageFlags:TXrSwapchainUsageFlags;
                                          const aFormat:TXrInt64;
                                          const aSampleCount:TXrUInt32;
                                          const aWidth:TXrUInt32;
                                          const aHeight:TXrUInt32;
                                          const aFaceCount:TXrUInt32;
                                          const aArraySize:TXrUInt32;
                                          const aMipCount:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 createFlags:=aCreateFlags;
 usageFlags:=aUsageFlags;
 format:=aFormat;
 sampleCount:=aSampleCount;
 width:=aWidth;
 height:=aHeight;
 faceCount:=aFaceCount;
 arraySize:=aArraySize;
 mipCount:=aMipCount;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrSwapchainImageOpenGLKHR.Create(const aImage:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 image:=aImage;
end;

constructor TXrSwapchainImageOpenGLESKHR.Create(const aImage:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 image:=aImage;
end;

constructor TXrSwapchainImageVulkanKHR.Create(const aImage:TVkImage);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 image:=aImage;
end;

constructor TXrSwapchainImageD3D11KHR.Create(const aTexture:ID3D11Texture2D);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 texture:=aTexture;
end;

constructor TXrSwapchainImageD3D12KHR.Create(const aTexture:ID3D12Resource);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 texture:=aTexture;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrSwapchainImageWaitInfo.Create(const aTimeout:TXrDuration);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 timeout:=aTimeout;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrReferenceSpaceCreateInfo.Create(const aReferenceSpaceType:TXrReferenceSpaceType;
                                               const aPoseInReferenceSpace:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 referenceSpaceType:=aReferenceSpaceType;
 poseInReferenceSpace:=aPoseInReferenceSpace;
end;

constructor TXrActionSpaceCreateInfo.Create(const aAction:TXrAction;
                                            const aSubactionPath:TXrPath;
                                            const aPoseInActionSpace:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 action:=aAction;
 subactionPath:=aSubactionPath;
 poseInActionSpace:=aPoseInActionSpace;
end;

constructor TXrSpaceLocation.Create(const aLocationFlags:TXrSpaceLocationFlags;
                                    const aPose:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 locationFlags:=aLocationFlags;
 pose:=aPose;
end;

constructor TXrSpaceVelocity.Create(const aVelocityFlags:TXrSpaceVelocityFlags;
                                    const aLinearVelocity:TXrVector3f;
                                    const aAngularVelocity:TXrVector3f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 velocityFlags:=aVelocityFlags;
 linearVelocity:=aLinearVelocity;
 angularVelocity:=aAngularVelocity;
end;

constructor TXrFovf.Create(const aAngleLeft:TXrFloat;
                           const aAngleRight:TXrFloat;
                           const aAngleUp:TXrFloat;
                           const aAngleDown:TXrFloat);
begin
 angleLeft:=aAngleLeft;
 angleRight:=aAngleRight;
 angleUp:=aAngleUp;
 angleDown:=aAngleDown;
end;

constructor TXrView.Create(const aPose:TXrPosef;
                           const aFov:TXrFovf);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 pose:=aPose;
 fov:=aFov;
end;

constructor TXrViewLocateInfo.Create(const aViewConfigurationType:TXrViewConfigurationType;
                                     const aDisplayTime:TXrTime;
                                     const aSpace:TXrSpace);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationType:=aViewConfigurationType;
 displayTime:=aDisplayTime;
 space:=aSpace;
end;

constructor TXrViewState.Create(const aViewStateFlags:TXrViewStateFlags);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewStateFlags:=aViewStateFlags;
end;

constructor TXrViewConfigurationView.Create(const aRecommendedImageRectWidth:TXrUInt32;
                                            const aMaxImageRectWidth:TXrUInt32;
                                            const aRecommendedImageRectHeight:TXrUInt32;
                                            const aMaxImageRectHeight:TXrUInt32;
                                            const aRecommendedSwapchainSampleCount:TXrUInt32;
                                            const aMaxSwapchainSampleCount:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 recommendedImageRectWidth:=aRecommendedImageRectWidth;
 maxImageRectWidth:=aMaxImageRectWidth;
 recommendedImageRectHeight:=aRecommendedImageRectHeight;
 maxImageRectHeight:=aMaxImageRectHeight;
 recommendedSwapchainSampleCount:=aRecommendedSwapchainSampleCount;
 maxSwapchainSampleCount:=aMaxSwapchainSampleCount;
end;

constructor TXrSwapchainSubImage.Create(const aSwapchain:TXrSwapchain;
                                        const aImageRect:TXrRect2Di;
                                        const aImageArrayIndex:TXrUInt32);
begin
 swapchain:=aSwapchain;
 imageRect:=aImageRect;
 imageArrayIndex:=aImageArrayIndex;
end;

constructor TXrCompositionLayerBaseHeader.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                                 const aSpace:TXrSpace);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
end;

constructor TXrCompositionLayerProjectionView.Create(const aPose:TXrPosef;
                                                     const aFov:TXrFovf;
                                                     const aSubImage:TXrSwapchainSubImage);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 pose:=aPose;
 fov:=aFov;
 subImage:=aSubImage;
end;

constructor TXrCompositionLayerProjection.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                                 const aSpace:TXrSpace;
                                                 const aViewCount:TXrUInt32;
                                                 const aViews:PXrCompositionLayerProjectionView);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
 viewCount:=aViewCount;
 views:=aViews;
end;

constructor TXrCompositionLayerQuad.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                           const aSpace:TXrSpace;
                                           const aEyeVisibility:TXrEyeVisibility;
                                           const aSubImage:TXrSwapchainSubImage;
                                           const aPose:TXrPosef;
                                           const aSize:TXrExtent2Df);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
 eyeVisibility:=aEyeVisibility;
 subImage:=aSubImage;
 pose:=aPose;
 size:=aSize;
end;

constructor TXrCompositionLayerCylinderKHR.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                                  const aSpace:TXrSpace;
                                                  const aEyeVisibility:TXrEyeVisibility;
                                                  const aSubImage:TXrSwapchainSubImage;
                                                  const aPose:TXrPosef;
                                                  const aRadius:TXrFloat;
                                                  const aCentralAngle:TXrFloat;
                                                  const aAspectRatio:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
 eyeVisibility:=aEyeVisibility;
 subImage:=aSubImage;
 pose:=aPose;
 radius:=aRadius;
 centralAngle:=aCentralAngle;
 aspectRatio:=aAspectRatio;
end;

constructor TXrCompositionLayerCubeKHR.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                              const aSpace:TXrSpace;
                                              const aEyeVisibility:TXrEyeVisibility;
                                              const aSwapchain:TXrSwapchain;
                                              const aImageArrayIndex:TXrUInt32;
                                              const aOrientation:TXrQuaternionf);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
 eyeVisibility:=aEyeVisibility;
 swapchain:=aSwapchain;
 imageArrayIndex:=aImageArrayIndex;
 orientation:=aOrientation;
end;

constructor TXrCompositionLayerEquirectKHR.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                                  const aSpace:TXrSpace;
                                                  const aEyeVisibility:TXrEyeVisibility;
                                                  const aSubImage:TXrSwapchainSubImage;
                                                  const aPose:TXrPosef;
                                                  const aRadius:TXrFloat;
                                                  const aScale:TXrVector2f;
                                                  const aBias:TXrVector2f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
 eyeVisibility:=aEyeVisibility;
 subImage:=aSubImage;
 pose:=aPose;
 radius:=aRadius;
 scale:=aScale;
 bias:=aBias;
end;

constructor TXrCompositionLayerDepthInfoKHR.Create(const aSubImage:TXrSwapchainSubImage;
                                                   const aMinDepth:TXrFloat;
                                                   const aMaxDepth:TXrFloat;
                                                   const aNearZ:TXrFloat;
                                                   const aFarZ:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 subImage:=aSubImage;
 minDepth:=aMinDepth;
 maxDepth:=aMaxDepth;
 nearZ:=aNearZ;
 farZ:=aFarZ;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrFrameEndInfo.Create(const aDisplayTime:TXrTime;
                                   const aEnvironmentBlendMode:TXrEnvironmentBlendMode;
                                   const aLayerCount:TXrUInt32;
                                   const aLayers:PPXrCompositionLayerBaseHeader);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 displayTime:=aDisplayTime;
 environmentBlendMode:=aEnvironmentBlendMode;
 layerCount:=aLayerCount;
 layers:=aLayers;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrFrameState.Create(const aPredictedDisplayTime:TXrTime;
                                 const aPredictedDisplayPeriod:TXrDuration;
                                 const aShouldRender:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 predictedDisplayTime:=aPredictedDisplayTime;
 predictedDisplayPeriod:=aPredictedDisplayPeriod;
 shouldRender:=aShouldRender;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrHapticVibration.Create(const aDuration:TXrDuration;
                                      const aFrequency:TXrFloat;
                                      const aAmplitude:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 duration:=aDuration;
 frequency:=aFrequency;
 amplitude:=aAmplitude;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrEventDataBuffer.Create(const aVarying:array of TXrUInt8);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrEventDataBuffer),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 ArrayItemCount:=length(aVarying);
 if ArrayItemCount>length(varying) then begin
  ArrayItemCount:=length(varying);
 end;
 if ArrayItemCount>0 then begin
  Move(aVarying[0],varying[0],ArrayItemCount*SizeOf(TXrUInt8));
 end;
end;

constructor TXrEventDataEventsLost.Create(const aLostEventCount:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 lostEventCount:=aLostEventCount;
end;

constructor TXrEventDataInstanceLossPending.Create(const aLossTime:TXrTime);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 lossTime:=aLossTime;
end;

constructor TXrEventDataSessionStateChanged.Create(const aSession:TXrSession;
                                                   const aState:TXrSessionState;
                                                   const aTime:TXrTime);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 session:=aSession;
 state:=aState;
 time:=aTime;
end;

constructor TXrEventDataReferenceSpaceChangePending.Create(const aSession:TXrSession;
                                                           const aReferenceSpaceType:TXrReferenceSpaceType;
                                                           const aChangeTime:TXrTime;
                                                           const aPoseValid:TXrBool32;
                                                           const aPoseInPreviousSpace:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 session:=aSession;
 referenceSpaceType:=aReferenceSpaceType;
 changeTime:=aChangeTime;
 poseValid:=aPoseValid;
 poseInPreviousSpace:=aPoseInPreviousSpace;
end;

constructor TXrEventDataPerfSettingsEXT.Create(const aDomain:TXrPerfSettingsDomainEXT;
                                               const aSubDomain:TXrPerfSettingsSubDomainEXT;
                                               const aFromLevel:TXrPerfSettingsNotificationLevelEXT;
                                               const aToLevel:TXrPerfSettingsNotificationLevelEXT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 domain:=aDomain;
 subDomain:=aSubDomain;
 fromLevel:=aFromLevel;
 toLevel:=aToLevel;
end;

constructor TXrEventDataVisibilityMaskChangedKHR.Create(const aSession:TXrSession;
                                                        const aViewConfigurationType:TXrViewConfigurationType;
                                                        const aViewIndex:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 session:=aSession;
 viewConfigurationType:=aViewConfigurationType;
 viewIndex:=aViewIndex;
end;

constructor TXrViewConfigurationProperties.Create(const aViewConfigurationType:TXrViewConfigurationType;
                                                  const aFovMutable:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationType:=aViewConfigurationType;
 fovMutable:=aFovMutable;
end;

constructor TXrActionStateBoolean.Create(const aCurrentState:TXrBool32;
                                         const aChangedSinceLastSync:TXrBool32;
                                         const aLastChangeTime:TXrTime;
                                         const aIsActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 currentState:=aCurrentState;
 changedSinceLastSync:=aChangedSinceLastSync;
 lastChangeTime:=aLastChangeTime;
 isActive:=aIsActive;
end;

constructor TXrActionStateFloat.Create(const aCurrentState:TXrFloat;
                                       const aChangedSinceLastSync:TXrBool32;
                                       const aLastChangeTime:TXrTime;
                                       const aIsActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 currentState:=aCurrentState;
 changedSinceLastSync:=aChangedSinceLastSync;
 lastChangeTime:=aLastChangeTime;
 isActive:=aIsActive;
end;

constructor TXrActionStateVector2f.Create(const aCurrentState:TXrVector2f;
                                          const aChangedSinceLastSync:TXrBool32;
                                          const aLastChangeTime:TXrTime;
                                          const aIsActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 currentState:=aCurrentState;
 changedSinceLastSync:=aChangedSinceLastSync;
 lastChangeTime:=aLastChangeTime;
 isActive:=aIsActive;
end;

constructor TXrActionStatePose.Create(const aIsActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 isActive:=aIsActive;
end;

constructor TXrActionStateGetInfo.Create(const aAction:TXrAction;
                                         const aSubactionPath:TXrPath);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 action:=aAction;
 subactionPath:=aSubactionPath;
end;

constructor TXrHapticActionInfo.Create(const aAction:TXrAction;
                                       const aSubactionPath:TXrPath);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 action:=aAction;
 subactionPath:=aSubactionPath;
end;

constructor TXrActionSetCreateInfo.Create(const aActionSetName:TXrCharString;
                                          const aLocalizedActionSetName:TXrCharString;
                                          const aPriority:TXrUInt32);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrActionSetCreateInfo),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 ArrayItemCount:=length(aActionSetName);
 if ArrayItemCount>length(actionSetName) then begin
  ArrayItemCount:=length(actionSetName);
 end;
 if ArrayItemCount>0 then begin
  Move(aActionSetName[1],actionSetName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 ArrayItemCount:=length(aLocalizedActionSetName);
 if ArrayItemCount>length(localizedActionSetName) then begin
  ArrayItemCount:=length(localizedActionSetName);
 end;
 if ArrayItemCount>0 then begin
  Move(aLocalizedActionSetName[1],localizedActionSetName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 priority:=aPriority;
end;

constructor TXrActionSuggestedBinding.Create(const aAction:TXrAction;
                                             const aBinding:TXrPath);
begin
 action:=aAction;
 binding:=aBinding;
end;

constructor TXrInteractionProfileSuggestedBinding.Create(const aInteractionProfile:TXrPath;
                                                         const aCountSuggestedBindings:TXrUInt32;
                                                         const aSuggestedBindings:PXrActionSuggestedBinding);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 interactionProfile:=aInteractionProfile;
 countSuggestedBindings:=aCountSuggestedBindings;
 suggestedBindings:=aSuggestedBindings;
end;

constructor TXrActiveActionSet.Create(const aActionSet:TXrActionSet;
                                      const aSubactionPath:TXrPath);
begin
 actionSet:=aActionSet;
 subactionPath:=aSubactionPath;
end;

constructor TXrSessionActionSetsAttachInfo.Create(const aCountActionSets:TXrUInt32;
                                                  const aActionSets:PXrActionSet);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 countActionSets:=aCountActionSets;
 actionSets:=aActionSets;
end;

constructor TXrActionsSyncInfo.Create(const aCountActiveActionSets:TXrUInt32;
                                      const aActiveActionSets:PXrActiveActionSet);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 countActiveActionSets:=aCountActiveActionSets;
 activeActionSets:=aActiveActionSets;
end;

constructor TXrBoundSourcesForActionEnumerateInfo.Create(const aAction:TXrAction);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 action:=aAction;
end;

constructor TXrInputSourceLocalizedNameGetInfo.Create(const aSourcePath:TXrPath;
                                                      const aWhichComponents:TXrInputSourceLocalizedNameFlags);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 sourcePath:=aSourcePath;
 whichComponents:=aWhichComponents;
end;

constructor TXrEventDataInteractionProfileChanged.Create(const aSession:TXrSession);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 session:=aSession;
end;

constructor TXrInteractionProfileState.Create(const aInteractionProfile:TXrPath);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 interactionProfile:=aInteractionProfile;
end;

constructor TXrActionCreateInfo.Create(const aActionName:TXrCharString;
                                       const aActionType:TXrActionType;
                                       const aCountSubactionPaths:TXrUInt32;
                                       const aSubactionPaths:PXrPath;
                                       const aLocalizedActionName:TXrCharString);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrActionCreateInfo),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 ArrayItemCount:=length(aActionName);
 if ArrayItemCount>length(actionName) then begin
  ArrayItemCount:=length(actionName);
 end;
 if ArrayItemCount>0 then begin
  Move(aActionName[1],actionName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 actionType:=aActionType;
 countSubactionPaths:=aCountSubactionPaths;
 subactionPaths:=aSubactionPaths;
 ArrayItemCount:=length(aLocalizedActionName);
 if ArrayItemCount>length(localizedActionName) then begin
  ArrayItemCount:=length(localizedActionName);
 end;
 if ArrayItemCount>0 then begin
  Move(aLocalizedActionName[1],localizedActionName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
end;

{$ifdef Android}
constructor TXrInstanceCreateInfoAndroidKHR.Create(const aApplicationVM:PXrVoid;
                                                   const aApplicationActivity:PXrVoid);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 applicationVM:=aApplicationVM;
 applicationActivity:=aApplicationActivity;
end;
{$endif}

constructor TXrVulkanSwapchainFormatListCreateInfoKHR.Create(const aViewFormatCount:TXrUInt32;
                                                             const aViewFormats:PVkFormat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewFormatCount:=aViewFormatCount;
 viewFormats:=aViewFormats;
end;

constructor TXrDebugUtilsObjectNameInfoEXT.Create(const aObjectType:TXrObjectType;
                                                  const aObjectHandle:TXrUInt64;
                                                  const aObjectName:PXrChar);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 objectType:=aObjectType;
 objectHandle:=aObjectHandle;
 objectName:=aObjectName;
end;

constructor TXrDebugUtilsLabelEXT.Create(const aLabelName:PXrChar);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 labelName:=aLabelName;
end;

constructor TXrDebugUtilsMessengerCallbackDataEXT.Create(const aMessageId:PXrChar;
                                                         const aFunctionName:PXrChar;
                                                         const aMessage:PXrChar;
                                                         const aObjectCount:TXrUInt32;
                                                         const aObjects:PXrDebugUtilsObjectNameInfoEXT;
                                                         const aSessionLabelCount:TXrUInt32;
                                                         const aSessionLabels:PXrDebugUtilsLabelEXT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 messageId:=aMessageId;
 functionName:=aFunctionName;
 message:=aMessage;
 objectCount:=aObjectCount;
 objects:=aObjects;
 sessionLabelCount:=aSessionLabelCount;
 sessionLabels:=aSessionLabels;
end;

constructor TXrVisibilityMaskKHR.Create(const aVertexCapacityInput:TXrUInt32;
                                        const aVertexCountOutput:TXrUInt32;
                                        const aVertices:PXrVector2f;
                                        const aIndexCapacityInput:TXrUInt32;
                                        const aIndexCountOutput:TXrUInt32;
                                        const aIndices:PXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 vertexCapacityInput:=aVertexCapacityInput;
 vertexCountOutput:=aVertexCountOutput;
 vertices:=aVertices;
 indexCapacityInput:=aIndexCapacityInput;
 indexCountOutput:=aIndexCountOutput;
 indices:=aIndices;
end;

constructor TXrGraphicsRequirementsOpenGLKHR.Create(const aMinApiVersionSupported:TXrVersion;
                                                    const aMaxApiVersionSupported:TXrVersion);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 minApiVersionSupported:=aMinApiVersionSupported;
 maxApiVersionSupported:=aMaxApiVersionSupported;
end;

constructor TXrGraphicsRequirementsOpenGLESKHR.Create(const aMinApiVersionSupported:TXrVersion;
                                                      const aMaxApiVersionSupported:TXrVersion);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 minApiVersionSupported:=aMinApiVersionSupported;
 maxApiVersionSupported:=aMaxApiVersionSupported;
end;

constructor TXrGraphicsRequirementsVulkanKHR.Create(const aMinApiVersionSupported:TXrVersion;
                                                    const aMaxApiVersionSupported:TXrVersion);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 minApiVersionSupported:=aMinApiVersionSupported;
 maxApiVersionSupported:=aMaxApiVersionSupported;
end;

constructor TXrGraphicsRequirementsD3D11KHR.Create(const aAdapterLuid:TLUID;
                                                   const aMinFeatureLevel:TD3D_FEATURE_LEVEL);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 adapterLuid:=aAdapterLuid;
 minFeatureLevel:=aMinFeatureLevel;
end;

constructor TXrGraphicsRequirementsD3D12KHR.Create(const aAdapterLuid:TLUID;
                                                   const aMinFeatureLevel:TD3D_FEATURE_LEVEL);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 adapterLuid:=aAdapterLuid;
 minFeatureLevel:=aMinFeatureLevel;
end;

constructor TXrVulkanInstanceCreateInfoKHR.Create(const aSystemId:TXrSystemId;
                                                  const aCreateFlags:TXrVulkanInstanceCreateFlagsKHR;
                                                  const aPfnGetInstanceProcAddr:TPFN_vkGetInstanceProcAddr;
                                                  const aVulkanCreateInfo:PVkInstanceCreateInfo;
                                                  const aVulkanAllocator:PVkAllocationCallbacks);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 systemId:=aSystemId;
 createFlags:=aCreateFlags;
 pfnGetInstanceProcAddr:=aPfnGetInstanceProcAddr;
 vulkanCreateInfo:=aVulkanCreateInfo;
 vulkanAllocator:=aVulkanAllocator;
end;

constructor TXrVulkanDeviceCreateInfoKHR.Create(const aSystemId:TXrSystemId;
                                                const aCreateFlags:TXrVulkanDeviceCreateFlagsKHR;
                                                const aPfnGetInstanceProcAddr:TPFN_vkGetInstanceProcAddr;
                                                const aVulkanPhysicalDevice:TVkPhysicalDevice;
                                                const aVulkanCreateInfo:PVkDeviceCreateInfo;
                                                const aVulkanAllocator:PVkAllocationCallbacks);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 systemId:=aSystemId;
 createFlags:=aCreateFlags;
 pfnGetInstanceProcAddr:=aPfnGetInstanceProcAddr;
 vulkanPhysicalDevice:=aVulkanPhysicalDevice;
 vulkanCreateInfo:=aVulkanCreateInfo;
 vulkanAllocator:=aVulkanAllocator;
end;

constructor TXrVulkanGraphicsDeviceGetInfoKHR.Create(const aSystemId:TXrSystemId;
                                                     const aVulkanInstance:TVkInstance);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 systemId:=aSystemId;
 vulkanInstance:=aVulkanInstance;
end;

constructor TXrSessionCreateInfoOverlayEXTX.Create(const aCreateFlags:TXrOverlaySessionCreateFlagsEXTX;
                                                   const aSessionLayersPlacement:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 createFlags:=aCreateFlags;
 sessionLayersPlacement:=aSessionLayersPlacement;
end;

constructor TXrEventDataMainSessionVisibilityChangedEXTX.Create(const aVisible:TXrBool32;
                                                                const aFlags:TXrOverlayMainSessionFlagsEXTX);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 visible:=aVisible;
 flags:=aFlags;
end;

constructor TXrEventDataDisplayRefreshRateChangedFB.Create(const aFromDisplayRefreshRate:TXrFloat;
                                                           const aToDisplayRefreshRate:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 fromDisplayRefreshRate:=aFromDisplayRefreshRate;
 toDisplayRefreshRate:=aToDisplayRefreshRate;
end;

constructor TXrViewConfigurationDepthRangeEXT.Create(const aRecommendedNearZ:TXrFloat;
                                                     const aMinNearZ:TXrFloat;
                                                     const aRecommendedFarZ:TXrFloat;
                                                     const aMaxFarZ:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 recommendedNearZ:=aRecommendedNearZ;
 minNearZ:=aMinNearZ;
 recommendedFarZ:=aRecommendedFarZ;
 maxFarZ:=aMaxFarZ;
end;

constructor TXrViewConfigurationViewFovEPIC.Create(const aRecommendedFov:TXrFovf;
                                                   const aMaxMutableFov:TXrFovf);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 recommendedFov:=aRecommendedFov;
 maxMutableFov:=aMaxMutableFov;
end;

constructor TXrInteractionProfileAnalogThresholdVALVE.Create(const aAction:TXrAction;
                                                             const aBinding:TXrPath;
                                                             const aOnThreshold:TXrFloat;
                                                             const aOffThreshold:TXrFloat;
                                                             const aOnHaptic:PXrHapticBaseHeader;
                                                             const aOffHaptic:PXrHapticBaseHeader);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 action:=aAction;
 binding:=aBinding;
 onThreshold:=aOnThreshold;
 offThreshold:=aOffThreshold;
 onHaptic:=aOnHaptic;
 offHaptic:=aOffHaptic;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrBindingModificationsKHR.Create(const aBindingModificationCount:TXrUInt32;
                                              const aBindingModifications:PPXrBindingModificationBaseHeaderKHR);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 bindingModificationCount:=aBindingModificationCount;
 bindingModifications:=aBindingModifications;
end;

constructor TXrDebugUtilsMessengerCreateInfoEXT.Create(const aMessageSeverities:TXrDebugUtilsMessageSeverityFlagsEXT;
                                                       const aMessageTypes:TXrDebugUtilsMessageTypeFlagsEXT;
                                                       const aUserCallback:TPFN_xrDebugUtilsMessengerCallbackEXT;
                                                       const aUserData:PXrVoid);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 messageSeverities:=aMessageSeverities;
 messageTypes:=aMessageTypes;
 userCallback:=aUserCallback;
 userData:=aUserData;
end;

constructor TXrSystemEyeGazeInteractionPropertiesEXT.Create(const aSupportsEyeGazeInteraction:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 supportsEyeGazeInteraction:=aSupportsEyeGazeInteraction;
end;

constructor TXrEyeGazeSampleTimeEXT.Create(const aTime:TXrTime);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 time:=aTime;
end;

constructor TXrSpatialAnchorCreateInfoMSFT.Create(const aSpace:TXrSpace;
                                                  const aPose:TXrPosef;
                                                  const aTime:TXrTime);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 space:=aSpace;
 pose:=aPose;
 time:=aTime;
end;

constructor TXrSpatialAnchorSpaceCreateInfoMSFT.Create(const aAnchor:TXrSpatialAnchorMSFT;
                                                       const aPoseInAnchorSpace:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 anchor:=aAnchor;
 poseInAnchorSpace:=aPoseInAnchorSpace;
end;

{$ifdef EGL}
constructor TXrGraphicsBindingEGLMNDX.Create(const aGetProcAddress:TPFNEGLGETPROCADDRESSPROC;
                                             const aDisplay:TEGLDisplay;
                                             const aConfig:TEGLConfig;
                                             const aContext:TEGLContext);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 getProcAddress:=aGetProcAddress;
 display:=aDisplay;
 config:=aConfig;
 context:=aContext;
end;
{$endif}

constructor TXrSpatialGraphNodeSpaceCreateInfoMSFT.Create(const aNodeType:TXrSpatialGraphNodeTypeMSFT;
                                                          const aNodeId:array of TXrUInt8;
                                                          const aPose:TXrPosef);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrSpatialGraphNodeSpaceCreateInfoMSFT),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 nodeType:=aNodeType;
 ArrayItemCount:=length(aNodeId);
 if ArrayItemCount>length(nodeId) then begin
  ArrayItemCount:=length(nodeId);
 end;
 if ArrayItemCount>0 then begin
  Move(aNodeId[0],nodeId[0],ArrayItemCount*SizeOf(TXrUInt8));
 end;
 pose:=aPose;
end;

constructor TXrSystemHandTrackingPropertiesEXT.Create(const aSupportsHandTracking:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 supportsHandTracking:=aSupportsHandTracking;
end;

constructor TXrHandTrackerCreateInfoEXT.Create(const aHand:TXrHandEXT;
                                               const aHandJointSet:TXrHandJointSetEXT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 hand:=aHand;
 handJointSet:=aHandJointSet;
end;

constructor TXrHandJointsLocateInfoEXT.Create(const aBaseSpace:TXrSpace;
                                              const aTime:TXrTime);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 baseSpace:=aBaseSpace;
 time:=aTime;
end;

constructor TXrHandJointLocationEXT.Create(const aLocationFlags:TXrSpaceLocationFlags;
                                           const aPose:TXrPosef;
                                           const aRadius:TXrFloat);
begin
 locationFlags:=aLocationFlags;
 pose:=aPose;
 radius:=aRadius;
end;

constructor TXrHandJointVelocityEXT.Create(const aVelocityFlags:TXrSpaceVelocityFlags;
                                           const aLinearVelocity:TXrVector3f;
                                           const aAngularVelocity:TXrVector3f);
begin
 velocityFlags:=aVelocityFlags;
 linearVelocity:=aLinearVelocity;
 angularVelocity:=aAngularVelocity;
end;

constructor TXrHandJointLocationsEXT.Create(const aIsActive:TXrBool32;
                                            const aJointCount:TXrUInt32;
                                            const aJointLocations:PXrHandJointLocationEXT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 isActive:=aIsActive;
 jointCount:=aJointCount;
 jointLocations:=aJointLocations;
end;

constructor TXrHandJointVelocitiesEXT.Create(const aJointCount:TXrUInt32;
                                             const aJointVelocities:PXrHandJointVelocityEXT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 jointCount:=aJointCount;
 jointVelocities:=aJointVelocities;
end;

constructor TXrHandJointsMotionRangeInfoEXT.Create(const aHandJointsMotionRange:TXrHandJointsMotionRangeEXT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 handJointsMotionRange:=aHandJointsMotionRange;
end;

constructor TXrHandMeshSpaceCreateInfoMSFT.Create(const aHandPoseType:TXrHandPoseTypeMSFT;
                                                  const aPoseInHandMeshSpace:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 handPoseType:=aHandPoseType;
 poseInHandMeshSpace:=aPoseInHandMeshSpace;
end;

constructor TXrHandMeshUpdateInfoMSFT.Create(const aTime:TXrTime;
                                             const aHandPoseType:TXrHandPoseTypeMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 time:=aTime;
 handPoseType:=aHandPoseType;
end;

constructor TXrHandMeshIndexBufferMSFT.Create(const aIndexBufferKey:TXrUInt32;
                                              const aIndexCapacityInput:TXrUInt32;
                                              const aIndexCountOutput:TXrUInt32;
                                              const aIndices:PXrUInt32);
begin
 indexBufferKey:=aIndexBufferKey;
 indexCapacityInput:=aIndexCapacityInput;
 indexCountOutput:=aIndexCountOutput;
 indices:=aIndices;
end;

constructor TXrHandMeshVertexMSFT.Create(const aPosition:TXrVector3f;
                                         const aNormal:TXrVector3f);
begin
 position:=aPosition;
 normal:=aNormal;
end;

constructor TXrHandMeshVertexBufferMSFT.Create(const aVertexUpdateTime:TXrTime;
                                               const aVertexCapacityInput:TXrUInt32;
                                               const aVertexCountOutput:TXrUInt32;
                                               const aVertices:PXrHandMeshVertexMSFT);
begin
 vertexUpdateTime:=aVertexUpdateTime;
 vertexCapacityInput:=aVertexCapacityInput;
 vertexCountOutput:=aVertexCountOutput;
 vertices:=aVertices;
end;

constructor TXrHandMeshMSFT.Create(const aIsActive:TXrBool32;
                                   const aIndexBufferChanged:TXrBool32;
                                   const aVertexBufferChanged:TXrBool32;
                                   const aIndexBuffer:TXrHandMeshIndexBufferMSFT;
                                   const aVertexBuffer:TXrHandMeshVertexBufferMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 isActive:=aIsActive;
 indexBufferChanged:=aIndexBufferChanged;
 vertexBufferChanged:=aVertexBufferChanged;
 indexBuffer:=aIndexBuffer;
 vertexBuffer:=aVertexBuffer;
end;

constructor TXrSystemHandTrackingMeshPropertiesMSFT.Create(const aSupportsHandTrackingMesh:TXrBool32;
                                                           const aMaxHandMeshIndexCount:TXrUInt32;
                                                           const aMaxHandMeshVertexCount:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 supportsHandTrackingMesh:=aSupportsHandTrackingMesh;
 maxHandMeshIndexCount:=aMaxHandMeshIndexCount;
 maxHandMeshVertexCount:=aMaxHandMeshVertexCount;
end;

constructor TXrHandPoseTypeInfoMSFT.Create(const aHandPoseType:TXrHandPoseTypeMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 handPoseType:=aHandPoseType;
end;

constructor TXrSecondaryViewConfigurationSessionBeginInfoMSFT.Create(const aViewConfigurationCount:TXrUInt32;
                                                                     const aEnabledViewConfigurationTypes:PXrViewConfigurationType);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationCount:=aViewConfigurationCount;
 enabledViewConfigurationTypes:=aEnabledViewConfigurationTypes;
end;

constructor TXrSecondaryViewConfigurationStateMSFT.Create(const aViewConfigurationType:TXrViewConfigurationType;
                                                          const aActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationType:=aViewConfigurationType;
 active:=aActive;
end;

constructor TXrSecondaryViewConfigurationFrameStateMSFT.Create(const aViewConfigurationCount:TXrUInt32;
                                                               const aViewConfigurationStates:PXrSecondaryViewConfigurationStateMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationCount:=aViewConfigurationCount;
 viewConfigurationStates:=aViewConfigurationStates;
end;

constructor TXrSecondaryViewConfigurationLayerInfoMSFT.Create(const aViewConfigurationType:TXrViewConfigurationType;
                                                              const aEnvironmentBlendMode:TXrEnvironmentBlendMode;
                                                              const aLayerCount:TXrUInt32;
                                                              const aLayers:PPXrCompositionLayerBaseHeader);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationType:=aViewConfigurationType;
 environmentBlendMode:=aEnvironmentBlendMode;
 layerCount:=aLayerCount;
 layers:=aLayers;
end;

constructor TXrSecondaryViewConfigurationFrameEndInfoMSFT.Create(const aViewConfigurationCount:TXrUInt32;
                                                                 const aViewConfigurationLayersInfo:PXrSecondaryViewConfigurationLayerInfoMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationCount:=aViewConfigurationCount;
 viewConfigurationLayersInfo:=aViewConfigurationLayersInfo;
end;

constructor TXrSecondaryViewConfigurationSwapchainCreateInfoMSFT.Create(const aViewConfigurationType:TXrViewConfigurationType);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 viewConfigurationType:=aViewConfigurationType;
end;

constructor TXrHolographicWindowAttachmentMSFT.Create(const aHolographicSpace:IUnknown;
                                                      const aCoreWindow:IUnknown);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 holographicSpace:=aHolographicSpace;
 coreWindow:=aCoreWindow;
end;

{$ifdef Android}
constructor TXrAndroidSurfaceSwapchainCreateInfoFB.Create(const aCreateFlags:TXrAndroidSurfaceSwapchainFlagsFB);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 createFlags:=aCreateFlags;
end;
{$endif}

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

{$ifdef Android}
constructor TXrSwapchainStateAndroidSurfaceDimensionsFB.Create(const aWidth:TXrUInt32;
                                                               const aHeight:TXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 width:=aWidth;
 height:=aHeight;
end;
{$endif}

{$ifdef EGL}
constructor TXrSwapchainStateSamplerOpenGLESFB.Create(const aMinFilter:TEGLenum;
                                                      const aMagFilter:TEGLenum;
                                                      const aWrapModeS:TEGLenum;
                                                      const aWrapModeT:TEGLenum;
                                                      const aSwizzleRed:TEGLenum;
                                                      const aSwizzleGreen:TEGLenum;
                                                      const aSwizzleBlue:TEGLenum;
                                                      const aSwizzleAlpha:TEGLenum;
                                                      const aMaxAnisotropy:TXrFloat;
                                                      const aBorderColor:TXrColor4f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 minFilter:=aMinFilter;
 magFilter:=aMagFilter;
 wrapModeS:=aWrapModeS;
 wrapModeT:=aWrapModeT;
 swizzleRed:=aSwizzleRed;
 swizzleGreen:=aSwizzleGreen;
 swizzleBlue:=aSwizzleBlue;
 swizzleAlpha:=aSwizzleAlpha;
 maxAnisotropy:=aMaxAnisotropy;
 borderColor:=aBorderColor;
end;
{$endif}

constructor TXrSwapchainStateSamplerVulkanFB.Create(const aMinFilter:TVkFilter;
                                                    const aMagFilter:TVkFilter;
                                                    const aMipmapMode:TVkSamplerMipmapMode;
                                                    const aWrapModeS:TVkSamplerAddressMode;
                                                    const aWrapModeT:TVkSamplerAddressMode;
                                                    const aSwizzleRed:TVkComponentSwizzle;
                                                    const aSwizzleGreen:TVkComponentSwizzle;
                                                    const aSwizzleBlue:TVkComponentSwizzle;
                                                    const aSwizzleAlpha:TVkComponentSwizzle;
                                                    const aMaxAnisotropy:TXrFloat;
                                                    const aBorderColor:TXrColor4f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 minFilter:=aMinFilter;
 magFilter:=aMagFilter;
 mipmapMode:=aMipmapMode;
 wrapModeS:=aWrapModeS;
 wrapModeT:=aWrapModeT;
 swizzleRed:=aSwizzleRed;
 swizzleGreen:=aSwizzleGreen;
 swizzleBlue:=aSwizzleBlue;
 swizzleAlpha:=aSwizzleAlpha;
 maxAnisotropy:=aMaxAnisotropy;
 borderColor:=aBorderColor;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

{$ifdef Android}
constructor TXrLoaderInitInfoAndroidKHR.Create(const aApplicationVM:PXrVoid;
                                               const aApplicationContext:PXrVoid);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 applicationVM:=aApplicationVM;
 applicationContext:=aApplicationContext;
end;
{$endif}

constructor TXrCompositionLayerEquirect2KHR.Create(const aLayerFlags:TXrCompositionLayerFlags;
                                                   const aSpace:TXrSpace;
                                                   const aEyeVisibility:TXrEyeVisibility;
                                                   const aSubImage:TXrSwapchainSubImage;
                                                   const aPose:TXrPosef;
                                                   const aRadius:TXrFloat;
                                                   const aCentralHorizontalAngle:TXrFloat;
                                                   const aUpperVerticalAngle:TXrFloat;
                                                   const aLowerVerticalAngle:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 layerFlags:=aLayerFlags;
 space:=aSpace;
 eyeVisibility:=aEyeVisibility;
 subImage:=aSubImage;
 pose:=aPose;
 radius:=aRadius;
 centralHorizontalAngle:=aCentralHorizontalAngle;
 upperVerticalAngle:=aUpperVerticalAngle;
 lowerVerticalAngle:=aLowerVerticalAngle;
end;

constructor TXrCompositionLayerColorScaleBiasKHR.Create(const aColorScale:TXrColor4f;
                                                        const aColorBias:TXrColor4f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 colorScale:=aColorScale;
 colorBias:=aColorBias;
end;

constructor TXrControllerModelKeyStateMSFT.Create(const aModelKey:TXrControllerModelKeyMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 modelKey:=aModelKey;
end;

constructor TXrControllerModelNodePropertiesMSFT.Create(const aParentNodeName:TXrCharString;
                                                        const aNodeName:TXrCharString);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrControllerModelNodePropertiesMSFT),#0);
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 ArrayItemCount:=length(aParentNodeName);
 if ArrayItemCount>length(parentNodeName) then begin
  ArrayItemCount:=length(parentNodeName);
 end;
 if ArrayItemCount>0 then begin
  Move(aParentNodeName[1],parentNodeName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
 ArrayItemCount:=length(aNodeName);
 if ArrayItemCount>length(nodeName) then begin
  ArrayItemCount:=length(nodeName);
 end;
 if ArrayItemCount>0 then begin
  Move(aNodeName[1],nodeName[0],ArrayItemCount*SizeOf(TXrChar));
 end;
end;

constructor TXrControllerModelPropertiesMSFT.Create(const aNodeCapacityInput:TXrUInt32;
                                                    const aNodeCountOutput:TXrUInt32;
                                                    const aNodeProperties:PXrControllerModelNodePropertiesMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 nodeCapacityInput:=aNodeCapacityInput;
 nodeCountOutput:=aNodeCountOutput;
 nodeProperties:=aNodeProperties;
end;

constructor TXrControllerModelNodeStateMSFT.Create(const aNodePose:TXrPosef);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 nodePose:=aNodePose;
end;

constructor TXrControllerModelStateMSFT.Create(const aNodeCapacityInput:TXrUInt32;
                                               const aNodeCountOutput:TXrUInt32;
                                               const aNodeStates:PXrControllerModelNodeStateMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 nodeCapacityInput:=aNodeCapacityInput;
 nodeCountOutput:=aNodeCountOutput;
 nodeStates:=aNodeStates;
end;

constructor TXrUuidMSFT.Create(const aBytes:array of TXrUInt8);
var ArrayItemCount:TXrInt32;
begin
 FillChar(self,SizeOf(TXrUuidMSFT),#0);
 ArrayItemCount:=length(aBytes);
 if ArrayItemCount>length(bytes) then begin
  ArrayItemCount:=length(bytes);
 end;
 if ArrayItemCount>0 then begin
  Move(aBytes[0],bytes[0],ArrayItemCount*SizeOf(TXrUInt8));
 end;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrSceneSphereBoundMSFT.Create(const aCenter:TXrVector3f;
                                           const aRadius:TXrFloat);
begin
 center:=aCenter;
 radius:=aRadius;
end;

constructor TXrVisualMeshComputeLodInfoMSFT.Create(const aLod:TXrMeshComputeLodMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 lod:=aLod;
end;

constructor TXrSceneOrientedBoxBoundMSFT.Create(const aPose:TXrPosef;
                                                const aExtents:TXrVector3f);
begin
 pose:=aPose;
 extents:=aExtents;
end;

constructor TXrSceneFrustumBoundMSFT.Create(const aPose:TXrPosef;
                                            const aFov:TXrFovf;
                                            const aFarDistance:TXrFloat);
begin
 pose:=aPose;
 fov:=aFov;
 farDistance:=aFarDistance;
end;

constructor TXrSceneBoundsMSFT.Create(const aSpace:TXrSpace;
                                      const aTime:TXrTime;
                                      const aSphereCount:TXrUInt32;
                                      const aSpheres:PXrSceneSphereBoundMSFT;
                                      const aBoxCount:TXrUInt32;
                                      const aBoxes:PXrSceneOrientedBoxBoundMSFT;
                                      const aFrustumCount:TXrUInt32;
                                      const aFrustums:PXrSceneFrustumBoundMSFT);
begin
 space:=aSpace;
 time:=aTime;
 sphereCount:=aSphereCount;
 spheres:=aSpheres;
 boxCount:=aBoxCount;
 boxes:=aBoxes;
 frustumCount:=aFrustumCount;
 frustums:=aFrustums;
end;

constructor TXrNewSceneComputeInfoMSFT.Create(const aRequestedFeatureCount:TXrUInt32;
                                              const aRequestedFeatures:PXrSceneComputeFeatureMSFT;
                                              const aConsistency:TXrSceneComputeConsistencyMSFT;
                                              const aBounds:TXrSceneBoundsMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 requestedFeatureCount:=aRequestedFeatureCount;
 requestedFeatures:=aRequestedFeatures;
 consistency:=aConsistency;
 bounds:=aBounds;
end;

constructor TXrSceneComponentMSFT.Create(const aComponentType:TXrSceneComponentTypeMSFT;
                                         const aId:TXrUuidMSFT;
                                         const aParentId:TXrUuidMSFT;
                                         const aUpdateTime:TXrTime);
begin
 componentType:=aComponentType;
 id:=aId;
 parentId:=aParentId;
 updateTime:=aUpdateTime;
end;

constructor TXrSceneComponentsMSFT.Create(const aComponentCapacityInput:TXrUInt32;
                                          const aComponentCountOutput:TXrUInt32;
                                          const aComponents:PXrSceneComponentMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 componentCapacityInput:=aComponentCapacityInput;
 componentCountOutput:=aComponentCountOutput;
 components:=aComponents;
end;

constructor TXrSceneComponentsGetInfoMSFT.Create(const aComponentType:TXrSceneComponentTypeMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 componentType:=aComponentType;
end;

constructor TXrSceneComponentLocationMSFT.Create(const aFlags:TXrSpaceLocationFlags;
                                                 const aPose:TXrPosef);
begin
 flags:=aFlags;
 pose:=aPose;
end;

constructor TXrSceneComponentLocationsMSFT.Create(const aLocationCount:TXrUInt32;
                                                  const aLocations:PXrSceneComponentLocationMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 locationCount:=aLocationCount;
 locations:=aLocations;
end;

constructor TXrSceneComponentsLocateInfoMSFT.Create(const aBaseSpace:TXrSpace;
                                                    const aTime:TXrTime;
                                                    const aComponentIdCount:TXrUInt32;
                                                    const aComponentIds:PXrUuidMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 baseSpace:=aBaseSpace;
 time:=aTime;
 componentIdCount:=aComponentIdCount;
 componentIds:=aComponentIds;
end;

constructor TXrSceneObjectMSFT.Create(const aObjectType:TXrSceneObjectTypeMSFT);
begin
 objectType:=aObjectType;
end;

constructor TXrSceneObjectsMSFT.Create(const aSceneObjectCount:TXrUInt32;
                                       const aSceneObjects:PXrSceneObjectMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 sceneObjectCount:=aSceneObjectCount;
 sceneObjects:=aSceneObjects;
end;

constructor TXrSceneComponentParentFilterInfoMSFT.Create(const aParentId:TXrUuidMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 parentId:=aParentId;
end;

constructor TXrSceneObjectTypesFilterInfoMSFT.Create(const aObjectTypeCount:TXrUInt32;
                                                     const aObjectTypes:PXrSceneObjectTypeMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 objectTypeCount:=aObjectTypeCount;
 objectTypes:=aObjectTypes;
end;

constructor TXrScenePlaneMSFT.Create(const aAlignment:TXrScenePlaneAlignmentTypeMSFT;
                                     const aSize:TXrExtent2Df;
                                     const aMeshBufferId:TXrUInt64;
                                     const aSupportsIndicesUint16:TXrBool32);
begin
 alignment:=aAlignment;
 size:=aSize;
 meshBufferId:=aMeshBufferId;
 supportsIndicesUint16:=aSupportsIndicesUint16;
end;

constructor TXrScenePlanesMSFT.Create(const aScenePlaneCount:TXrUInt32;
                                      const aScenePlanes:PXrScenePlaneMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 scenePlaneCount:=aScenePlaneCount;
 scenePlanes:=aScenePlanes;
end;

constructor TXrScenePlaneAlignmentFilterInfoMSFT.Create(const aAlignmentCount:TXrUInt32;
                                                        const aAlignments:PXrScenePlaneAlignmentTypeMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 alignmentCount:=aAlignmentCount;
 alignments:=aAlignments;
end;

constructor TXrSceneMeshMSFT.Create(const aMeshBufferId:TXrUInt64;
                                    const aSupportsIndicesUint16:TXrBool32);
begin
 meshBufferId:=aMeshBufferId;
 supportsIndicesUint16:=aSupportsIndicesUint16;
end;

constructor TXrSceneMeshesMSFT.Create(const aSceneMeshCount:TXrUInt32;
                                      const aSceneMeshes:PXrSceneMeshMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 sceneMeshCount:=aSceneMeshCount;
 sceneMeshes:=aSceneMeshes;
end;

constructor TXrSceneMeshBuffersGetInfoMSFT.Create(const aMeshBufferId:TXrUInt64);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 meshBufferId:=aMeshBufferId;
end;

begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
end;

constructor TXrSceneMeshVertexBufferMSFT.Create(const aVertexCapacityInput:TXrUInt32;
                                                const aVertexCountOutput:TXrUInt32;
                                                const aVertices:PXrVector3f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 vertexCapacityInput:=aVertexCapacityInput;
 vertexCountOutput:=aVertexCountOutput;
 vertices:=aVertices;
end;

constructor TXrSceneMeshIndicesUint32MSFT.Create(const aIndexCapacityInput:TXrUInt32;
                                                 const aIndexCountOutput:TXrUInt32;
                                                 const aIndices:PXrUInt32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 indexCapacityInput:=aIndexCapacityInput;
 indexCountOutput:=aIndexCountOutput;
 indices:=aIndices;
end;

constructor TXrSceneMeshIndicesUint16MSFT.Create(const aIndexCapacityInput:TXrUInt32;
                                                 const aIndexCountOutput:TXrUInt32;
                                                 const aIndices:PXrUInt16);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 indexCapacityInput:=aIndexCapacityInput;
 indexCountOutput:=aIndexCountOutput;
 indices:=aIndices;
end;

constructor TXrSerializedSceneFragmentDataGetInfoMSFT.Create(const aSceneFragmentId:TXrUuidMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 sceneFragmentId:=aSceneFragmentId;
end;

constructor TXrDeserializeSceneFragmentMSFT.Create(const aBufferSize:TXrUInt32;
                                                   const aBuffer:PXrUInt8);
begin
 bufferSize:=aBufferSize;
 buffer:=aBuffer;
end;

constructor TXrSceneDeserializeInfoMSFT.Create(const aFragmentCount:TXrUInt32;
                                               const aFragments:PXrDeserializeSceneFragmentMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 fragmentCount:=aFragmentCount;
 fragments:=aFragments;
end;

constructor TXrSystemColorSpacePropertiesFB.Create(const aColorSpace:TXrColorSpaceFB);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 colorSpace:=aColorSpace;
end;

constructor TXrCompositionLayerDepthTestVARJO.Create(const aDepthTestRangeNearZ:TXrFloat;
                                                     const aDepthTestRangeFarZ:TXrFloat);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 depthTestRangeNearZ:=aDepthTestRangeNearZ;
 depthTestRangeFarZ:=aDepthTestRangeFarZ;
end;

constructor TXrViewLocateFoveatedRenderingVARJO.Create(const aFoveatedRenderingActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 foveatedRenderingActive:=aFoveatedRenderingActive;
end;

constructor TXrFoveatedViewConfigurationViewVARJO.Create(const aFoveatedRenderingActive:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 foveatedRenderingActive:=aFoveatedRenderingActive;
end;

constructor TXrSystemFoveatedRenderingPropertiesVARJO.Create(const aSupportsFoveatedRendering:TXrBool32);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 supportsFoveatedRendering:=aSupportsFoveatedRendering;
end;

constructor TXrCompositionLayerReprojectionInfoMSFT.Create(const aReprojectionMode:TXrReprojectionModeMSFT);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 reprojectionMode:=aReprojectionMode;
end;

constructor TXrCompositionLayerReprojectionPlaneOverrideMSFT.Create(const aPosition:TXrVector3f;
                                                                    const aNormal:TXrVector3f;
                                                                    const aVelocity:TXrVector3f);
begin
 type_:=TXrStructureType(TXrInt32(0));
 next:=nil;
 position:=aPosition;
 normal:=aNormal;
 velocity:=aVelocity;
end;
{$endif}

constructor TOpenXR.Create;
begin
 inherited Create;
 FillChar(fCommands,SizeOf(TOpenXRCommands),#0);
end;

constructor TOpenXR.Create(const AOpenXRCommands:TOpenXRCommands);
begin
 inherited Create;
 fCommands:=AOpenXRCommands;
end;

destructor TOpenXR.Destroy;
begin
 inherited Destroy;
end;

function TOpenXR.GetInstanceProcAddr(instance:TXrInstance;const name:PXrChar;_function:PPFN_xrVoidFunction):TXrResult;
begin
 result:=fCommands.GetInstanceProcAddr(instance,name,_function);
end;

function TOpenXR.EnumerateApiLayerProperties(propertyCapacityInput:TXrUInt32;propertyCountOutput:PXrUInt32;properties:PXrApiLayerProperties):TXrResult;
begin
 result:=fCommands.EnumerateApiLayerProperties(propertyCapacityInput,propertyCountOutput,properties);
end;

function TOpenXR.EnumerateInstanceExtensionProperties(const layerName:PXrChar;propertyCapacityInput:TXrUInt32;propertyCountOutput:PXrUInt32;properties:PXrExtensionProperties):TXrResult;
begin
 result:=fCommands.EnumerateInstanceExtensionProperties(layerName,propertyCapacityInput,propertyCountOutput,properties);
end;

function TOpenXR.CreateInstance(const createInfo:PXrInstanceCreateInfo;instance:PXrInstance):TXrResult;
begin
 result:=fCommands.CreateInstance(createInfo,instance);
end;

function TOpenXR.DestroyInstance(instance:TXrInstance):TXrResult;
begin
 result:=fCommands.DestroyInstance(instance);
end;

function TOpenXR.ResultToString(instance:TXrInstance;value:TXrResult;buffer:TXrChar):TXrResult;
begin
 result:=fCommands.ResultToString(instance,value,buffer);
end;

function TOpenXR.StructureTypeToString(instance:TXrInstance;value:TXrStructureType;buffer:TXrChar):TXrResult;
begin
 result:=fCommands.StructureTypeToString(instance,value,buffer);
end;

function TOpenXR.GetInstanceProperties(instance:TXrInstance;instanceProperties:PXrInstanceProperties):TXrResult;
begin
 result:=fCommands.GetInstanceProperties(instance,instanceProperties);
end;

function TOpenXR.GetSystem(instance:TXrInstance;const getInfo:PXrSystemGetInfo;systemId:PXrSystemId):TXrResult;
begin
 result:=fCommands.GetSystem(instance,getInfo,systemId);
end;

function TOpenXR.GetSystemProperties(instance:TXrInstance;systemId:TXrSystemId;properties:PXrSystemProperties):TXrResult;
begin
 result:=fCommands.GetSystemProperties(instance,systemId,properties);
end;

function TOpenXR.CreateSession(instance:TXrInstance;const createInfo:PXrSessionCreateInfo;session:PXrSession):TXrResult;
begin
 result:=fCommands.CreateSession(instance,createInfo,session);
end;

function TOpenXR.DestroySession(session:TXrSession):TXrResult;
begin
 result:=fCommands.DestroySession(session);
end;

function TOpenXR.DestroySpace(space:TXrSpace):TXrResult;
begin
 result:=fCommands.DestroySpace(space);
end;

function TOpenXR.EnumerateSwapchainFormats(session:TXrSession;formatCapacityInput:TXrUInt32;formatCountOutput:PXrUInt32;formats:PXrInt64):TXrResult;
begin
 result:=fCommands.EnumerateSwapchainFormats(session,formatCapacityInput,formatCountOutput,formats);
end;

function TOpenXR.CreateSwapchain(session:TXrSession;const createInfo:PXrSwapchainCreateInfo;swapchain:PXrSwapchain):TXrResult;
begin
 result:=fCommands.CreateSwapchain(session,createInfo,swapchain);
end;

function TOpenXR.DestroySwapchain(swapchain:TXrSwapchain):TXrResult;
begin
 result:=fCommands.DestroySwapchain(swapchain);
end;

function TOpenXR.EnumerateSwapchainImages(swapchain:TXrSwapchain;imageCapacityInput:TXrUInt32;imageCountOutput:PXrUInt32;images:PXrSwapchainImageBaseHeader):TXrResult;
begin
 result:=fCommands.EnumerateSwapchainImages(swapchain,imageCapacityInput,imageCountOutput,images);
end;

function TOpenXR.AcquireSwapchainImage(swapchain:TXrSwapchain;const acquireInfo:PXrSwapchainImageAcquireInfo;index:PXrUInt32):TXrResult;
begin
 result:=fCommands.AcquireSwapchainImage(swapchain,acquireInfo,index);
end;

function TOpenXR.WaitSwapchainImage(swapchain:TXrSwapchain;const waitInfo:PXrSwapchainImageWaitInfo):TXrResult;
begin
 result:=fCommands.WaitSwapchainImage(swapchain,waitInfo);
end;

function TOpenXR.ReleaseSwapchainImage(swapchain:TXrSwapchain;const releaseInfo:PXrSwapchainImageReleaseInfo):TXrResult;
begin
 result:=fCommands.ReleaseSwapchainImage(swapchain,releaseInfo);
end;

function TOpenXR.BeginSession(session:TXrSession;const beginInfo:PXrSessionBeginInfo):TXrResult;
begin
 result:=fCommands.BeginSession(session,beginInfo);
end;

function TOpenXR.EndSession(session:TXrSession):TXrResult;
begin
 result:=fCommands.EndSession(session);
end;

function TOpenXR.RequestExitSession(session:TXrSession):TXrResult;
begin
 result:=fCommands.RequestExitSession(session);
end;

function TOpenXR.EnumerateReferenceSpaces(session:TXrSession;spaceCapacityInput:TXrUInt32;spaceCountOutput:PXrUInt32;spaces:PXrReferenceSpaceType):TXrResult;
begin
 result:=fCommands.EnumerateReferenceSpaces(session,spaceCapacityInput,spaceCountOutput,spaces);
end;

function TOpenXR.CreateReferenceSpace(session:TXrSession;const createInfo:PXrReferenceSpaceCreateInfo;space:PXrSpace):TXrResult;
begin
 result:=fCommands.CreateReferenceSpace(session,createInfo,space);
end;

function TOpenXR.CreateActionSpace(session:TXrSession;const createInfo:PXrActionSpaceCreateInfo;space:PXrSpace):TXrResult;
begin
 result:=fCommands.CreateActionSpace(session,createInfo,space);
end;

function TOpenXR.LocateSpace(space:TXrSpace;baseSpace:TXrSpace;time:TXrTime;location:PXrSpaceLocation):TXrResult;
begin
 result:=fCommands.LocateSpace(space,baseSpace,time,location);
end;

function TOpenXR.EnumerateViewConfigurations(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationTypeCapacityInput:TXrUInt32;viewConfigurationTypeCountOutput:PXrUInt32;viewConfigurationTypes:PXrViewConfigurationType):TXrResult;
begin
 result:=fCommands.EnumerateViewConfigurations(instance,systemId,viewConfigurationTypeCapacityInput,viewConfigurationTypeCountOutput,viewConfigurationTypes);
end;

function TOpenXR.EnumerateEnvironmentBlendModes(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;environmentBlendModeCapacityInput:TXrUInt32;environmentBlendModeCountOutput:PXrUInt32;environmentBlendModes:PXrEnvironmentBlendMode):TXrResult;
begin
 result:=fCommands.EnumerateEnvironmentBlendModes(instance,systemId,viewConfigurationType,environmentBlendModeCapacityInput,environmentBlendModeCountOutput,environmentBlendModes);
end;

function TOpenXR.GetViewConfigurationProperties(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;configurationProperties:PXrViewConfigurationProperties):TXrResult;
begin
 result:=fCommands.GetViewConfigurationProperties(instance,systemId,viewConfigurationType,configurationProperties);
end;

function TOpenXR.EnumerateViewConfigurationViews(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;viewCapacityInput:TXrUInt32;viewCountOutput:PXrUInt32;views:PXrViewConfigurationView):TXrResult;
begin
 result:=fCommands.EnumerateViewConfigurationViews(instance,systemId,viewConfigurationType,viewCapacityInput,viewCountOutput,views);
end;

function TOpenXR.BeginFrame(session:TXrSession;const frameBeginInfo:PXrFrameBeginInfo):TXrResult;
begin
 result:=fCommands.BeginFrame(session,frameBeginInfo);
end;

function TOpenXR.LocateViews(session:TXrSession;const viewLocateInfo:PXrViewLocateInfo;viewState:PXrViewState;viewCapacityInput:TXrUInt32;viewCountOutput:PXrUInt32;views:PXrView):TXrResult;
begin
 result:=fCommands.LocateViews(session,viewLocateInfo,viewState,viewCapacityInput,viewCountOutput,views);
end;

function TOpenXR.EndFrame(session:TXrSession;const frameEndInfo:PXrFrameEndInfo):TXrResult;
begin
 result:=fCommands.EndFrame(session,frameEndInfo);
end;

function TOpenXR.WaitFrame(session:TXrSession;const frameWaitInfo:PXrFrameWaitInfo;frameState:PXrFrameState):TXrResult;
begin
 result:=fCommands.WaitFrame(session,frameWaitInfo,frameState);
end;

function TOpenXR.ApplyHapticFeedback(session:TXrSession;const hapticActionInfo:PXrHapticActionInfo;const hapticFeedback:PXrHapticBaseHeader):TXrResult;
begin
 result:=fCommands.ApplyHapticFeedback(session,hapticActionInfo,hapticFeedback);
end;

function TOpenXR.StopHapticFeedback(session:TXrSession;const hapticActionInfo:PXrHapticActionInfo):TXrResult;
begin
 result:=fCommands.StopHapticFeedback(session,hapticActionInfo);
end;

function TOpenXR.PollEvent(instance:TXrInstance;eventData:PXrEventDataBuffer):TXrResult;
begin
 result:=fCommands.PollEvent(instance,eventData);
end;

function TOpenXR.StringToPath(instance:TXrInstance;const pathString:PXrChar;path:PXrPath):TXrResult;
begin
 result:=fCommands.StringToPath(instance,pathString,path);
end;

function TOpenXR.PathToString(instance:TXrInstance;path:TXrPath;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult;
begin
 result:=fCommands.PathToString(instance,path,bufferCapacityInput,bufferCountOutput,buffer);
end;

function TOpenXR.GetReferenceSpaceBoundsRect(session:TXrSession;referenceSpaceType:TXrReferenceSpaceType;bounds:PXrExtent2Df):TXrResult;
begin
 result:=fCommands.GetReferenceSpaceBoundsRect(session,referenceSpaceType,bounds);
end;

{$ifdef Android}
function TOpenXR.SetAndroidApplicationThreadKHR(session:TXrSession;threadType:TXrAndroidThreadTypeKHR;threadId:TXrUInt32):TXrResult;
begin
 result:=fCommands.SetAndroidApplicationThreadKHR(session,threadType,threadId);
end;
{$endif}

{$ifdef Android}
function TOpenXR.CreateSwapchainAndroidSurfaceKHR(session:TXrSession;const info:PXrSwapchainCreateInfo;swapchain:PXrSwapchain;surface:Pjobject):TXrResult;
begin
 result:=fCommands.CreateSwapchainAndroidSurfaceKHR(session,info,swapchain,surface);
end;
{$endif}

function TOpenXR.GetActionStateBoolean(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateBoolean):TXrResult;
begin
 result:=fCommands.GetActionStateBoolean(session,getInfo,state);
end;

function TOpenXR.GetActionStateFloat(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateFloat):TXrResult;
begin
 result:=fCommands.GetActionStateFloat(session,getInfo,state);
end;

function TOpenXR.GetActionStateVector2f(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStateVector2f):TXrResult;
begin
 result:=fCommands.GetActionStateVector2f(session,getInfo,state);
end;

function TOpenXR.GetActionStatePose(session:TXrSession;const getInfo:PXrActionStateGetInfo;state:PXrActionStatePose):TXrResult;
begin
 result:=fCommands.GetActionStatePose(session,getInfo,state);
end;

function TOpenXR.CreateActionSet(instance:TXrInstance;const createInfo:PXrActionSetCreateInfo;actionSet:PXrActionSet):TXrResult;
begin
 result:=fCommands.CreateActionSet(instance,createInfo,actionSet);
end;

function TOpenXR.DestroyActionSet(actionSet:TXrActionSet):TXrResult;
begin
 result:=fCommands.DestroyActionSet(actionSet);
end;

function TOpenXR.CreateAction(actionSet:TXrActionSet;const createInfo:PXrActionCreateInfo;action:PXrAction):TXrResult;
begin
 result:=fCommands.CreateAction(actionSet,createInfo,action);
end;

function TOpenXR.DestroyAction(action:TXrAction):TXrResult;
begin
 result:=fCommands.DestroyAction(action);
end;

function TOpenXR.SuggestInteractionProfileBindings(instance:TXrInstance;const suggestedBindings:PXrInteractionProfileSuggestedBinding):TXrResult;
begin
 result:=fCommands.SuggestInteractionProfileBindings(instance,suggestedBindings);
end;

function TOpenXR.AttachSessionActionSets(session:TXrSession;const attachInfo:PXrSessionActionSetsAttachInfo):TXrResult;
begin
 result:=fCommands.AttachSessionActionSets(session,attachInfo);
end;

function TOpenXR.GetCurrentInteractionProfile(session:TXrSession;topLevelUserPath:TXrPath;interactionProfile:PXrInteractionProfileState):TXrResult;
begin
 result:=fCommands.GetCurrentInteractionProfile(session,topLevelUserPath,interactionProfile);
end;

function TOpenXR.SyncActions(session:TXrSession;const syncInfo:PXrActionsSyncInfo):TXrResult;
begin
 result:=fCommands.SyncActions(session,syncInfo);
end;

function TOpenXR.EnumerateBoundSourcesForAction(session:TXrSession;const enumerateInfo:PXrBoundSourcesForActionEnumerateInfo;sourceCapacityInput:TXrUInt32;sourceCountOutput:PXrUInt32;sources:PXrPath):TXrResult;
begin
 result:=fCommands.EnumerateBoundSourcesForAction(session,enumerateInfo,sourceCapacityInput,sourceCountOutput,sources);
end;

function TOpenXR.GetInputSourceLocalizedName(session:TXrSession;const getInfo:PXrInputSourceLocalizedNameGetInfo;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult;
begin
 result:=fCommands.GetInputSourceLocalizedName(session,getInfo,bufferCapacityInput,bufferCountOutput,buffer);
end;

function TOpenXR.GetVulkanInstanceExtensionsKHR(instance:TXrInstance;systemId:TXrSystemId;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult;
begin
 result:=fCommands.GetVulkanInstanceExtensionsKHR(instance,systemId,bufferCapacityInput,bufferCountOutput,buffer);
end;

function TOpenXR.GetVulkanDeviceExtensionsKHR(instance:TXrInstance;systemId:TXrSystemId;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrChar):TXrResult;
begin
 result:=fCommands.GetVulkanDeviceExtensionsKHR(instance,systemId,bufferCapacityInput,bufferCountOutput,buffer);
end;

function TOpenXR.GetVulkanGraphicsDeviceKHR(instance:TXrInstance;systemId:TXrSystemId;vkInstance:TVkInstance;vkPhysicalDevice:PVkPhysicalDevice):TXrResult;
begin
 result:=fCommands.GetVulkanGraphicsDeviceKHR(instance,systemId,vkInstance,vkPhysicalDevice);
end;

function TOpenXR.GetOpenGLGraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsOpenGLKHR):TXrResult;
begin
 result:=fCommands.GetOpenGLGraphicsRequirementsKHR(instance,systemId,graphicsRequirements);
end;

function TOpenXR.GetOpenGLESGraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsOpenGLESKHR):TXrResult;
begin
 result:=fCommands.GetOpenGLESGraphicsRequirementsKHR(instance,systemId,graphicsRequirements);
end;

function TOpenXR.GetVulkanGraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsVulkanKHR):TXrResult;
begin
 result:=fCommands.GetVulkanGraphicsRequirementsKHR(instance,systemId,graphicsRequirements);
end;

function TOpenXR.GetD3D11GraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsD3D11KHR):TXrResult;
begin
 result:=fCommands.GetD3D11GraphicsRequirementsKHR(instance,systemId,graphicsRequirements);
end;

function TOpenXR.GetD3D12GraphicsRequirementsKHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsD3D12KHR):TXrResult;
begin
 result:=fCommands.GetD3D12GraphicsRequirementsKHR(instance,systemId,graphicsRequirements);
end;

function TOpenXR.PerfSettingsSetPerformanceLevelEXT(session:TXrSession;domain:TXrPerfSettingsDomainEXT;level:TXrPerfSettingsLevelEXT):TXrResult;
begin
 result:=fCommands.PerfSettingsSetPerformanceLevelEXT(session,domain,level);
end;

function TOpenXR.ThermalGetTemperatureTrendEXT(session:TXrSession;domain:TXrPerfSettingsDomainEXT;notificationLevel:PXrPerfSettingsNotificationLevelEXT;tempHeadroom:PXrFloat;tempSlope:PXrFloat):TXrResult;
begin
 result:=fCommands.ThermalGetTemperatureTrendEXT(session,domain,notificationLevel,tempHeadroom,tempSlope);
end;

function TOpenXR.SetDebugUtilsObjectNameEXT(instance:TXrInstance;const nameInfo:PXrDebugUtilsObjectNameInfoEXT):TXrResult;
begin
 result:=fCommands.SetDebugUtilsObjectNameEXT(instance,nameInfo);
end;

function TOpenXR.CreateDebugUtilsMessengerEXT(instance:TXrInstance;const createInfo:PXrDebugUtilsMessengerCreateInfoEXT;messenger:PXrDebugUtilsMessengerEXT):TXrResult;
begin
 result:=fCommands.CreateDebugUtilsMessengerEXT(instance,createInfo,messenger);
end;

function TOpenXR.DestroyDebugUtilsMessengerEXT(messenger:TXrDebugUtilsMessengerEXT):TXrResult;
begin
 result:=fCommands.DestroyDebugUtilsMessengerEXT(messenger);
end;

function TOpenXR.SubmitDebugUtilsMessageEXT(instance:TXrInstance;messageSeverity:TXrDebugUtilsMessageSeverityFlagsEXT;messageTypes:TXrDebugUtilsMessageTypeFlagsEXT;const callbackData:PXrDebugUtilsMessengerCallbackDataEXT):TXrResult;
begin
 result:=fCommands.SubmitDebugUtilsMessageEXT(instance,messageSeverity,messageTypes,callbackData);
end;

function TOpenXR.SessionBeginDebugUtilsLabelRegionEXT(session:TXrSession;const labelInfo:PXrDebugUtilsLabelEXT):TXrResult;
begin
 result:=fCommands.SessionBeginDebugUtilsLabelRegionEXT(session,labelInfo);
end;

function TOpenXR.SessionEndDebugUtilsLabelRegionEXT(session:TXrSession):TXrResult;
begin
 result:=fCommands.SessionEndDebugUtilsLabelRegionEXT(session);
end;

function TOpenXR.SessionInsertDebugUtilsLabelEXT(session:TXrSession;const labelInfo:PXrDebugUtilsLabelEXT):TXrResult;
begin
 result:=fCommands.SessionInsertDebugUtilsLabelEXT(session,labelInfo);
end;

{$ifdef Windows}
function TOpenXR.ConvertTimeToWin32PerformanceCounterKHR(instance:TXrInstance;time:TXrTime;performanceCounter:PLargeInteger):TXrResult;
begin
 result:=fCommands.ConvertTimeToWin32PerformanceCounterKHR(instance,time,performanceCounter);
end;
{$endif}

{$ifdef Windows}
function TOpenXR.ConvertWin32PerformanceCounterToTimeKHR(instance:TXrInstance;const performanceCounter:PLargeInteger;time:PXrTime):TXrResult;
begin
 result:=fCommands.ConvertWin32PerformanceCounterToTimeKHR(instance,performanceCounter,time);
end;
{$endif}

function TOpenXR.CreateVulkanInstanceKHR(instance:TXrInstance;const createInfo:PXrVulkanInstanceCreateInfoKHR;vulkanInstance:PVkInstance;vulkanResult:PVkResult):TXrResult;
begin
 result:=fCommands.CreateVulkanInstanceKHR(instance,createInfo,vulkanInstance,vulkanResult);
end;

function TOpenXR.CreateVulkanDeviceKHR(instance:TXrInstance;const createInfo:PXrVulkanDeviceCreateInfoKHR;vulkanDevice:PVkDevice;vulkanResult:PVkResult):TXrResult;
begin
 result:=fCommands.CreateVulkanDeviceKHR(instance,createInfo,vulkanDevice,vulkanResult);
end;

function TOpenXR.GetVulkanGraphicsDevice2KHR(instance:TXrInstance;const getInfo:PXrVulkanGraphicsDeviceGetInfoKHR;vulkanPhysicalDevice:PVkPhysicalDevice):TXrResult;
begin
 result:=fCommands.GetVulkanGraphicsDevice2KHR(instance,getInfo,vulkanPhysicalDevice);
end;

function TOpenXR.GetVulkanGraphicsRequirements2KHR(instance:TXrInstance;systemId:TXrSystemId;graphicsRequirements:PXrGraphicsRequirementsVulkanKHR):TXrResult;
begin
 result:=fCommands.GetVulkanGraphicsRequirements2KHR(instance,systemId,graphicsRequirements);
end;

function TOpenXR.ConvertTimeToTimespecTimeKHR(instance:TXrInstance;time:TXrTime;timespecTime:Ptimespec):TXrResult;
begin
 result:=fCommands.ConvertTimeToTimespecTimeKHR(instance,time,timespecTime);
end;

function TOpenXR.ConvertTimespecTimeToTimeKHR(instance:TXrInstance;const timespecTime:Ptimespec;time:PXrTime):TXrResult;
begin
 result:=fCommands.ConvertTimespecTimeToTimeKHR(instance,timespecTime,time);
end;

function TOpenXR.GetVisibilityMaskKHR(session:TXrSession;viewConfigurationType:TXrViewConfigurationType;viewIndex:TXrUInt32;visibilityMaskType:TXrVisibilityMaskTypeKHR;visibilityMask:PXrVisibilityMaskKHR):TXrResult;
begin
 result:=fCommands.GetVisibilityMaskKHR(session,viewConfigurationType,viewIndex,visibilityMaskType,visibilityMask);
end;

function TOpenXR.CreateSpatialAnchorMSFT(session:TXrSession;const createInfo:PXrSpatialAnchorCreateInfoMSFT;anchor:PXrSpatialAnchorMSFT):TXrResult;
begin
 result:=fCommands.CreateSpatialAnchorMSFT(session,createInfo,anchor);
end;

function TOpenXR.CreateSpatialAnchorSpaceMSFT(session:TXrSession;const createInfo:PXrSpatialAnchorSpaceCreateInfoMSFT;space:PXrSpace):TXrResult;
begin
 result:=fCommands.CreateSpatialAnchorSpaceMSFT(session,createInfo,space);
end;

function TOpenXR.DestroySpatialAnchorMSFT(anchor:TXrSpatialAnchorMSFT):TXrResult;
begin
 result:=fCommands.DestroySpatialAnchorMSFT(anchor);
end;

function TOpenXR.SetInputDeviceActiveEXT(session:TXrSession;interactionProfile:TXrPath;topLevelPath:TXrPath;isActive:TXrBool32):TXrResult;
begin
 result:=fCommands.SetInputDeviceActiveEXT(session,interactionProfile,topLevelPath,isActive);
end;

function TOpenXR.SetInputDeviceStateBoolEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrBool32):TXrResult;
begin
 result:=fCommands.SetInputDeviceStateBoolEXT(session,topLevelPath,inputSourcePath,state);
end;

function TOpenXR.SetInputDeviceStateFloatEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrFloat):TXrResult;
begin
 result:=fCommands.SetInputDeviceStateFloatEXT(session,topLevelPath,inputSourcePath,state);
end;

function TOpenXR.SetInputDeviceStateVector2fEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;state:TXrVector2f):TXrResult;
begin
 result:=fCommands.SetInputDeviceStateVector2fEXT(session,topLevelPath,inputSourcePath,state);
end;

function TOpenXR.SetInputDeviceLocationEXT(session:TXrSession;topLevelPath:TXrPath;inputSourcePath:TXrPath;space:TXrSpace;pose:TXrPosef):TXrResult;
begin
 result:=fCommands.SetInputDeviceLocationEXT(session,topLevelPath,inputSourcePath,space,pose);
end;

function TOpenXR.InitializeLoaderKHR(const loaderInitInfo:PXrLoaderInitInfoBaseHeaderKHR):TXrResult;
begin
 result:=fCommands.InitializeLoaderKHR(loaderInitInfo);
end;

function TOpenXR.CreateSpatialGraphNodeSpaceMSFT(session:TXrSession;const createInfo:PXrSpatialGraphNodeSpaceCreateInfoMSFT;space:PXrSpace):TXrResult;
begin
 result:=fCommands.CreateSpatialGraphNodeSpaceMSFT(session,createInfo,space);
end;

function TOpenXR.CreateHandTrackerEXT(session:TXrSession;const createInfo:PXrHandTrackerCreateInfoEXT;handTracker:PXrHandTrackerEXT):TXrResult;
begin
 result:=fCommands.CreateHandTrackerEXT(session,createInfo,handTracker);
end;

function TOpenXR.DestroyHandTrackerEXT(handTracker:TXrHandTrackerEXT):TXrResult;
begin
 result:=fCommands.DestroyHandTrackerEXT(handTracker);
end;

function TOpenXR.LocateHandJointsEXT(handTracker:TXrHandTrackerEXT;const locateInfo:PXrHandJointsLocateInfoEXT;locations:PXrHandJointLocationsEXT):TXrResult;
begin
 result:=fCommands.LocateHandJointsEXT(handTracker,locateInfo,locations);
end;

function TOpenXR.CreateHandMeshSpaceMSFT(handTracker:TXrHandTrackerEXT;const createInfo:PXrHandMeshSpaceCreateInfoMSFT;space:PXrSpace):TXrResult;
begin
 result:=fCommands.CreateHandMeshSpaceMSFT(handTracker,createInfo,space);
end;

function TOpenXR.UpdateHandMeshMSFT(handTracker:TXrHandTrackerEXT;const updateInfo:PXrHandMeshUpdateInfoMSFT;handMesh:PXrHandMeshMSFT):TXrResult;
begin
 result:=fCommands.UpdateHandMeshMSFT(handTracker,updateInfo,handMesh);
end;

function TOpenXR.GetControllerModelKeyMSFT(session:TXrSession;topLevelUserPath:TXrPath;controllerModelKeyState:PXrControllerModelKeyStateMSFT):TXrResult;
begin
 result:=fCommands.GetControllerModelKeyMSFT(session,topLevelUserPath,controllerModelKeyState);
end;

function TOpenXR.LoadControllerModelMSFT(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;bufferCapacityInput:TXrUInt32;bufferCountOutput:PXrUInt32;buffer:PXrUInt8):TXrResult;
begin
 result:=fCommands.LoadControllerModelMSFT(session,modelKey,bufferCapacityInput,bufferCountOutput,buffer);
end;

function TOpenXR.GetControllerModelPropertiesMSFT(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;properties:PXrControllerModelPropertiesMSFT):TXrResult;
begin
 result:=fCommands.GetControllerModelPropertiesMSFT(session,modelKey,properties);
end;

function TOpenXR.GetControllerModelStateMSFT(session:TXrSession;modelKey:TXrControllerModelKeyMSFT;state:PXrControllerModelStateMSFT):TXrResult;
begin
 result:=fCommands.GetControllerModelStateMSFT(session,modelKey,state);
end;

function TOpenXR.EnumerateSceneComputeFeaturesMSFT(instance:TXrInstance;systemId:TXrSystemId;featureCapacityInput:TXrUInt32;featureCountOutput:PXrUInt32;features:PXrSceneComputeFeatureMSFT):TXrResult;
begin
 result:=fCommands.EnumerateSceneComputeFeaturesMSFT(instance,systemId,featureCapacityInput,featureCountOutput,features);
end;

function TOpenXR.CreateSceneObserverMSFT(session:TXrSession;const createInfo:PXrSceneObserverCreateInfoMSFT;sceneObserver:PXrSceneObserverMSFT):TXrResult;
begin
 result:=fCommands.CreateSceneObserverMSFT(session,createInfo,sceneObserver);
end;

function TOpenXR.DestroySceneObserverMSFT(sceneObserver:TXrSceneObserverMSFT):TXrResult;
begin
 result:=fCommands.DestroySceneObserverMSFT(sceneObserver);
end;

function TOpenXR.CreateSceneMSFT(sceneObserver:TXrSceneObserverMSFT;const createInfo:PXrSceneCreateInfoMSFT;scene:PXrSceneMSFT):TXrResult;
begin
 result:=fCommands.CreateSceneMSFT(sceneObserver,createInfo,scene);
end;

function TOpenXR.DestroySceneMSFT(scene:TXrSceneMSFT):TXrResult;
begin
 result:=fCommands.DestroySceneMSFT(scene);
end;

function TOpenXR.ComputeNewSceneMSFT(sceneObserver:TXrSceneObserverMSFT;const computeInfo:PXrNewSceneComputeInfoMSFT):TXrResult;
begin
 result:=fCommands.ComputeNewSceneMSFT(sceneObserver,computeInfo);
end;

function TOpenXR.GetSceneComputeStateMSFT(sceneObserver:TXrSceneObserverMSFT;state:PXrSceneComputeStateMSFT):TXrResult;
begin
 result:=fCommands.GetSceneComputeStateMSFT(sceneObserver,state);
end;

function TOpenXR.GetSceneComponentsMSFT(scene:TXrSceneMSFT;const getInfo:PXrSceneComponentsGetInfoMSFT;components:PXrSceneComponentsMSFT):TXrResult;
begin
 result:=fCommands.GetSceneComponentsMSFT(scene,getInfo,components);
end;

function TOpenXR.LocateSceneComponentsMSFT(scene:TXrSceneMSFT;const locateInfo:PXrSceneComponentsLocateInfoMSFT;locations:PXrSceneComponentLocationsMSFT):TXrResult;
begin
 result:=fCommands.LocateSceneComponentsMSFT(scene,locateInfo,locations);
end;

function TOpenXR.GetSceneMeshBuffersMSFT(scene:TXrSceneMSFT;const getInfo:PXrSceneMeshBuffersGetInfoMSFT;buffers:PXrSceneMeshBuffersMSFT):TXrResult;
begin
 result:=fCommands.GetSceneMeshBuffersMSFT(scene,getInfo,buffers);
end;

function TOpenXR.DeserializeSceneMSFT(sceneObserver:TXrSceneObserverMSFT;const deserializeInfo:PXrSceneDeserializeInfoMSFT):TXrResult;
begin
 result:=fCommands.DeserializeSceneMSFT(sceneObserver,deserializeInfo);
end;

function TOpenXR.GetSerializedSceneFragmentDataMSFT(scene:TXrSceneMSFT;const getInfo:PXrSerializedSceneFragmentDataGetInfoMSFT;countInput:TXrUInt32;readOutput:PXrUInt32;buffer:PXrUInt8):TXrResult;
begin
 result:=fCommands.GetSerializedSceneFragmentDataMSFT(scene,getInfo,countInput,readOutput,buffer);
end;

function TOpenXR.EnumerateDisplayRefreshRatesFB(session:TXrSession;displayRefreshRateCapacityInput:TXrUInt32;displayRefreshRateCountOutput:PXrUInt32;displayRefreshRates:PXrFloat):TXrResult;
begin
 result:=fCommands.EnumerateDisplayRefreshRatesFB(session,displayRefreshRateCapacityInput,displayRefreshRateCountOutput,displayRefreshRates);
end;

function TOpenXR.GetDisplayRefreshRateFB(session:TXrSession;displayRefreshRate:PXrFloat):TXrResult;
begin
 result:=fCommands.GetDisplayRefreshRateFB(session,displayRefreshRate);
end;

function TOpenXR.RequestDisplayRefreshRateFB(session:TXrSession;displayRefreshRate:TXrFloat):TXrResult;
begin
 result:=fCommands.RequestDisplayRefreshRateFB(session,displayRefreshRate);
end;

function TOpenXR.CreateSpatialAnchorFromPerceptionAnchorMSFT(session:TXrSession;perceptionAnchor:IUnknown;anchor:PXrSpatialAnchorMSFT):TXrResult;
begin
 result:=fCommands.CreateSpatialAnchorFromPerceptionAnchorMSFT(session,perceptionAnchor,anchor);
end;

function TOpenXR.TryGetPerceptionAnchorFromSpatialAnchorMSFT(session:TXrSession;anchor:TXrSpatialAnchorMSFT;perceptionAnchor:PPIUnknown):TXrResult;
begin
 result:=fCommands.TryGetPerceptionAnchorFromSpatialAnchorMSFT(session,anchor,perceptionAnchor);
end;

function TOpenXR.UpdateSwapchainFB(swapchain:TXrSwapchain;const state:PXrSwapchainStateBaseHeaderFB):TXrResult;
begin
 result:=fCommands.UpdateSwapchainFB(swapchain,state);
end;

function TOpenXR.GetSwapchainStateFB(swapchain:TXrSwapchain;state:PXrSwapchainStateBaseHeaderFB):TXrResult;
begin
 result:=fCommands.GetSwapchainStateFB(swapchain,state);
end;

function TOpenXR.EnumerateColorSpacesFB(session:TXrSession;colorSpaceCapacityInput:TXrUInt32;colorSpaceCountOutput:PXrUInt32;colorSpaces:PXrColorSpaceFB):TXrResult;
begin
 result:=fCommands.EnumerateColorSpacesFB(session,colorSpaceCapacityInput,colorSpaceCountOutput,colorSpaces);
end;

function TOpenXR.SetColorSpaceFB(session:TXrSession;const colorspace:TXrColorSpaceFB):TXrResult;
begin
 result:=fCommands.SetColorSpaceFB(session,colorspace);
end;

function TOpenXR.SetEnvironmentDepthEstimationVARJO(session:TXrSession;enabled:TXrBool32):TXrResult;
begin
 result:=fCommands.SetEnvironmentDepthEstimationVARJO(session,enabled);
end;

function TOpenXR.EnumerateReprojectionModesMSFT(instance:TXrInstance;systemId:TXrSystemId;viewConfigurationType:TXrViewConfigurationType;modeCapacityInput:TXrUInt32;modeCountOutput:PXrUInt32;modes:PXrReprojectionModeMSFT):TXrResult;
begin
 result:=fCommands.EnumerateReprojectionModesMSFT(instance,systemId,viewConfigurationType,modeCapacityInput,modeCountOutput,modes);
end;

function TOpenXR.GetAudioOutputDeviceGuidOculus(instance:TXrInstance;buffer:Twchar_t):TXrResult;
begin
 result:=fCommands.GetAudioOutputDeviceGuidOculus(instance,buffer);
end;

function TOpenXR.GetAudioInputDeviceGuidOculus(instance:TXrInstance;buffer:Twchar_t):TXrResult;
begin
 result:=fCommands.GetAudioInputDeviceGuidOculus(instance,buffer);
end;

initialization
 xr:=TOpenXR.Create;
finalization
 xr.Free;
 if assigned(LibOpenXR) then begin
  xrFreeLibrary(LibOpenXR);
 end;
end.
