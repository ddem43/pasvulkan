(******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************)

 // this code is an adapted copy of vkxml2pas with tweaks to make it works
 // with xr.xml
 //
 // this project is conducted with delphi 10.4.2 so maybe there is problems
 // with other compilers/configurations.
 //
 // Warning : at this point, the resulting unit was not tested at all
 // so USE AT YOUR OWN RISK (as already stated above).
 //
 // some cleaning is needed to remove vk only names here
 // then, some tests are required to see if we can get something working from
 // the resulting unit


program xrxml2pas;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define CAN_INLINE}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi} 
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
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
{$ifdef Windows}
 {$define Win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define CAN_INLINE}
{$else}
 {$undef CAN_INLINE}
 {$ifdef ver180}
  {$define CAN_INLINE}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define CAN_INLINE}
   {$ifend}
  {$endif}
 {$endif}
{$endif}
{$ifdef windows}
 {$apptype console}
{$endif}
{$undef UNICODE}

uses SysUtils,Classes,Contnrs;

// On Windows, OpenXR commands use the stdcall convention
// On Android/ARMv7a, OpenXR functions use the armeabi-v7a-hard calling convention, even if the application's native code is compiled with the armeabi-v7a calling convention.
// On other platforms, use the default calling convention
const CallingConventions='{$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}';

      CommentPadding=80;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
type qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

//https://stackoverflow.com/questions/35158485/how-to-remove-repeated-spaces-from-a-string
function DeleteRepeatedSpaces(const OldText: string): string;
var
  pO,pR: PChar;
begin
  SetLength(Result,Length(OldText));
  pR := Pointer(Result);
  pO := Pointer(OldText);
  while (pO^ <> '') do begin
    pR^ := pO^;
    Inc(pR);
    if (pO^ <> ' ') then begin
      Inc(pO);
      Continue;
    end;
    repeat // Skip additional spaces
      Inc(pO);
    until (pO^ = '') or (pO^ <> ' ');
  end;
  SetLength(Result,pR-Pointer(Result));
end;


function UTF32CharToUTF8(CharValue:longword):ansistring;
var Data:array[0..{$ifdef strictutf8}3{$else}5{$endif}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef strictutf8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef strictutf8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,pansichar(@Data[0]),ResultLen);
 end;
end;

type TXMLClass=class
      public
       Previous,Next:TXMLClass;
       Core:pointer;
       constructor Create; overload; virtual;
       destructor Destroy; override;
     end;

const MaxListSize=2147483647 div SizeOf(TXMLClass);

type PEngineListClasses=^TXMLClasses;
     TXMLClasses=array[0..MaxListSize-1] of TXMLClass;

     TXMLClassList=class(TXMLClass)
      private
       InternalList:PEngineListClasses;
       InternalCount,InternalCapacity:longint;
       function GetItem(Index:longint):TXMLClass;
       procedure SetItem(Index:longint;Value:TXMLClass);
       function GetItemPointer(Index:longint):TXMLClass;
      public
       ClearWithContentDestroying:boolean;
       CapacityMinimium:longint;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear;
       procedure ClearNoFree;
       procedure ClearWithFree;
       function Add(Item:TXMLClass):longint;
       function Append(Item:TXMLClass):longint;
       function AddList(List:TXMLClassList):longint;
       function AppendList(List:TXMLClassList):longint;
       function NewClass:TXMLClass;
       procedure Insert(Index:longint;Item:TXMLClass);
       procedure Delete(Index:longint);
       procedure DeleteClass(Index:longint);
       function Remove(Item:TXMLClass):longint;
       function RemoveClass(Item:TXMLClass):longint;
       function Find(Item:TXMLClass):longint;
       function IndexOf(Item:TXMLClass):longint;
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetOptimalCapacity(TargetCapacity:longint);
       procedure SetCount(NewCount:longint);
       function Push(Item:TXMLClass):longint;
       function Pop(var Item:TXMLClass):boolean; overload;
       function Pop:TXMLClass; overload;
       function Last:TXMLClass;
       property Count:longint read InternalCount; 
       property Capacity:longint read InternalCapacity write SetCapacity;
       property Item[Index:longint]:TXMLClass read GetItem write SetItem; default;
       property Items[Index:longint]:TXMLClass read GetItem write SetItem;
       property PItems[Index:longint]:TXMLClass read GetItemPointer;
     end;

     TXMLClassLinkedList=class(TXMLClass)
      public
       ClearWithContentDestroying:boolean;
       First,Last:TXMLClass;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear;
       procedure ClearNoFree;
       procedure ClearWithFree;
       procedure Add(Item:TXMLClass);
       procedure Append(Item:TXMLClass);
       procedure AddLinkedList(List:TXMLClassLinkedList);
       procedure AppendLinkedList(List:TXMLClassLinkedList);
       procedure Remove(Item:TXMLClass);
       procedure RemoveClass(Item:TXMLClass);
       procedure Push(Item:TXMLClass);
       function Pop(var Item:TXMLClass):boolean; overload;
       function Pop:TXMLClass; overload;
       function Count:longint;
     end;

     TXMLStringTreeData=ptrint;

     PXMLStringTreeNode=^TXMLStringTreeNode;
     TXMLStringTreeNode=record
      TheChar:ansichar;
      Data:TXMLStringTreeData;
      DataExist:boolean;
      Prevoius,Next,Up,Down:PXMLStringTreeNode;
     end;

     TXMLStringTree=class
      private
       Root:PXMLStringTreeNode;
       function CreateStringTreeNode(AChar:ansichar):PXMLStringTreeNode;
       procedure DestroyStringTreeNode(Node:PXMLStringTreeNode);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       procedure DumpTree;
       procedure DumpList;
       procedure AppendTo(DestStringTree:TXMLStringTree);
       procedure Optimize(DestStringTree:TXMLStringTree);
       function Add(Content:ansistring;Data:TXMLStringTreeData;Replace:boolean=false):boolean;
       function Delete(Content:ansistring):boolean;
       function Find(Content:ansistring;var Data:TXMLStringTreeData):boolean;
       function FindEx(Content:ansistring;var Data:TXMLStringTreeData;var Len:longint):boolean;
     end;

     TXMLString={$ifdef UNICODE}widestring{$else}ansistring{$endif};
     TXMLChar={$ifdef UNICODE}widechar{$else}ansichar{$endif};

     TXMLParameter=class(TXMLClass)
      public
       Name:ansistring;
       Value:TXMLString;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLParameter); virtual;
     end;

     TXMLItemList=class;

     TXMLTag=class;

     TXMLItem=class(TXMLClass)
      public
       Items:TXMLItemList;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear; virtual;
       procedure Add(Item:TXMLItem);
       procedure Assign(From:TXMLItem); virtual;
       function FindTag(const TagName:ansistring):TXMLTag;
     end;

     TXMLItemList=class(TXMLClassList)
      private
       function GetItem(Index:longint):TXMLItem;
       procedure SetItem(Index:longint;Value:TXMLItem);
      public
       constructor Create; override;
       destructor Destroy; override;
       function NewClass:TXMLItem;
       function FindTag(const TagName:ansistring):TXMLTag;
       property Item[Index:longint]:TXMLItem read GetItem write SetItem; default;
       property Items[Index:longint]:TXMLItem read GetItem write SetItem;
     end;

     TXMLText=class(TXMLItem)
      public
       Text:TXMLString;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLCommentTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLTag=class(TXMLItem)
      public
       Name:ansistring;
       Parameter:array of TXMLParameter;
       IsAloneTag:boolean;
       constructor Create; override;
       destructor Destroy; override;
       procedure Clear; override;
       procedure Assign(From:TXMLItem); override;
       function FindParameter(ParameterName:ansistring):TXMLParameter;
       function GetParameter(ParameterName:ansistring;default:ansistring=''):ansistring;
       function AddParameter(AParameter:TXMLParameter):boolean; overload;
       function AddParameter(Name:ansistring;Value:TXMLString):boolean; overload;
       function RemoveParameter(AParameter:TXMLParameter):boolean; overload;
       function RemoveParameter(ParameterName:ansistring):boolean; overload;
     end;

     TXMLProcessTag=class(TXMLTag)
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
     end;

     TXMLScriptTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLCDataTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLDOCTYPETag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXMLExtraTag=class(TXMLItem)
      public
       Text:ansistring;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXMLItem); override;
       procedure SetText(AText:ansistring);
     end;

     TXML=class(TXMLClass)
      private
       function ReadXMLText:ansistring;
       procedure WriteXMLText(Text:ansistring);
      public
       Root:TXMLItem;
       AutomaticAloneTagDetection:boolean;
       FormatIndent:boolean;
       FormatIndentText:boolean;
       constructor Create; override;
       destructor Destroy; override;
       procedure Assign(From:TXML);
       function Parse(Stream:TStream):boolean;
       function Read(Stream:TStream):boolean;
       function Write(Stream:TStream;IdentSize:longint=2):boolean;
       property Text:ansistring read ReadXMLText write WriteXMLText;
     end;

function NextPowerOfTwo(Value:longint;const MinThreshold:longint=0):longint;
begin
 result:=(Value or MinThreshold)-1;
 result:=result or (result shr 1);
 result:=result or (result shr 2);
 result:=result or (result shr 4);
 result:=result or (result shr 8);
 result:=result or (result shr 16);
 inc(result);
end;

const EntityChars:array[1..102,1..2] of TXMLString=(('&quot;',#34),('&amp;',#38),('&apos;',''''),
                                                    ('&lt;',#60),('&gt;',#62),('&euro;',#128),('&nbsp;',#160),('&iexcl;',#161),
                                                    ('&cent;',#162),('&pound;',#163),('&curren;',#164),('&yen;',#165),
                                                    ('&brvbar;',#166),('&sect;',#167),('&uml;',#168),('&copy;',#169),
                                                    ('&ordf;',#170),('&laquo;',#171),('&not;',#172),('&shy;',#173),
                                                    ('&reg;',#174),('&macr;',#175),('&deg;',#176),('&plusmn;',#177),
                                                    ('&sup2;',#178),('&sup3;',#179),('&acute;',#180),('&micro;',#181),
                                                    ('&para;',#182),('&middot;',#183),('&cedil;',#184),('&sup1;',#185),
                                                    ('&ordm;',#186),('&raquo;',#187),('&frac14;',#188),('&frac12;',#189),
                                                    ('&frac34;',#190),('&iquest;',#191),('&Agrave;',#192),('&Aacute;',#193),
                                                    ('&Acirc;',#194),('&Atilde;',#195),('&Auml;',#196),('&Aring;',#197),
                                                    ('&AElig;',#198),('&Ccedil;',#199),('&Egrave;',#200),('&Eacute;',#201),
                                                    ('&Ecirc;',#202),('&Euml;',#203),('&Igrave;',#204),('&Iacute;',#205),
                                                    ('&Icirc;',#206),('&Iuml;',#207),('&ETH;',#208),('&Ntilde;',#209),
                                                    ('&Ograve;',#210),('&Oacute;',#211),('&Ocirc;',#212),('&Otilde;',#213),
                                                    ('&Ouml;',#214),('&times;',#215),('&Oslash;',#216),('&Ugrave;',#217),
                                                    ('&Uacute;',#218),('&Ucirc;',#219),('&Uuml;',#220),('&Yacute;',#221),
                                                    ('&THORN;',#222),('&szlig;',#223),('&agrave;',#224),('&aacute;',#225),
                                                    ('&acirc;',#226),('&atilde;',#227),('&auml;',#228),('&aring;',#229),
                                                    ('&aelig;',#230),('&ccedil;',#231),('&egrave;',#232),('&eacute;',#233),
                                                    ('&ecirc;',#234),('&euml;',#235),('&igrave;',#236),('&iacute;',#237),
                                                    ('&icirc;',#238),('&iuml;',#239),('&eth;',#240),('&ntilde;',#241),
                                                    ('&ograve;',#242),('&oacute;',#243),('&ocirc;',#244),('&otilde;',#245),
                                                    ('&ouml;',#246),('&divide;',#247),('&oslash;',#248),('&ugrave;',#249),
                                                    ('&uacute;',#250),('&ucirc;',#251),('&uuml;',#252),('&yacute;',#253),
                                                    ('&thorn;',#254),('&yuml;',#255));

type TEntitiesCharLookUpItem=record
      IsEntity:boolean;
      Entity:ansistring;
     end;

     TEntitiesCharLookUpTable=array[0..{$ifdef UNICODE}65535{$else}255{$endif}] of TEntitiesCharLookUpItem;

var EntitiesCharLookUp:TEntitiesCharLookUpTable;
    EntityStringTree:TXMLStringTree;

const EntityInitialized:boolean=false;

procedure InitializeEntites;
var EntityCounter:longint;
begin
 if not EntityInitialized then begin
  EntityInitialized:=true;
  EntityStringTree:=TXMLStringTree.Create;
  FillChar(EntitiesCharLookUp,SizeOf(TEntitiesCharLookUpTable),#0);
  for EntityCounter:=low(EntityChars) to high(EntityChars) do begin
   EntityStringTree.Add(EntityChars[EntityCounter,1],EntityCounter,true);
   with EntitiesCharLookUp[ord(EntityChars[EntityCounter,2][1])] do begin
    IsEntity:=true;
    Entity:=EntityChars[EntityCounter,1];
   end;
  end;
 end;
end;

procedure FinalizeEntites;
begin
 if assigned(EntityStringTree) then begin
  EntityStringTree.Destroy;
  EntityStringTree:=nil;
 end;
 EntityInitialized:=false;
end;

function ConvertToEntities(AString:TXMLString;IdentLevel:longint=0):ansistring;
var Counter,IdentCounter:longint;
    c:TXMLChar;
begin
 result:='';
 for Counter:=1 to length(AString) do begin
  c:=AString[Counter];
  if c=#13 then begin
   if ((Counter+1)<=length(AString)) and (AString[Counter+1]=#10) then begin
    continue;
   end;
   c:=#10;
  end;
  if EntitiesCharLookUp[ord(c)].IsEntity then begin
   result:=result+EntitiesCharLookUp[ord(c)].Entity;
  end else if (c=#9) or (c=#10) or (c=#13) or ((c>=#32) and (c<=#127)) then begin
   result:=result+c;
   if c=#10 then begin
    for IdentCounter:=1 to IdentLevel do begin
     result:=result+' ';
    end;
   end;
  end else begin
{$ifdef UNICODE}
   if c<#255 then begin
    result:=result+'&#'+INTTOSTR(ord(c))+';';
   end else begin
    result:=result+'&#x'+IntToHex(ord(c),4)+';';
   end;
{$else}
   result:=result+'&#'+INTTOSTR(byte(c))+';';
{$endif}
  end;
 end;
end;

constructor TXMLClass.Create;
begin
 inherited Create;
 Previous:=nil;
 Next:=nil;
 Core:=nil;
end;

destructor TXMLClass.Destroy;
begin
 inherited Destroy;
end;

constructor TXMLClassList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=false;
 InternalCount:=0;
 InternalCapacity:=0;
 InternalList:=nil;
 CapacityMinimium:=0;
 Clear;
end;

destructor TXMLClassList.Destroy;
begin
 Clear;
 if assigned(InternalList) and (InternalCapacity<>0) then begin
  FreeMem(InternalList);
 end;
 inherited Destroy;
end;

procedure TXMLClassList.Clear;
begin
 if ClearWithContentDestroying then begin
  ClearWithFree;
 end else begin
  ClearNoFree;
 end;
end;

procedure TXMLClassList.ClearNoFree;
begin
 SetCount(0);
end;

procedure TXMLClassList.ClearWithFree;
var Counter:longint;
begin
 for Counter:=0 to InternalCount-1 do begin
  if assigned(InternalList^[Counter]) then begin
   try
    InternalList^[Counter].Destroy;
   except
   end;
  end;
 end;
 SetCount(0);
end;

procedure TXMLClassList.SetCapacity(NewCapacity:longint);
begin
 if (InternalCapacity<>NewCapacity) and
    ((NewCapacity>=0) and (NewCapacity<MaxListSize)) then begin
  ReallocMem(InternalList,NewCapacity*SizeOf(TXMLClass));
  if InternalCapacity<NewCapacity then begin
   FillChar(InternalList^[InternalCapacity],(NewCapacity-InternalCapacity)*SizeOf(TXMLClass),#0);
  end;
  InternalCapacity:=NewCapacity;
 end;
end;

procedure TXMLClassList.SetOptimalCapacity(TargetCapacity:longint);
var CapacityMask:longint;
begin
 if (TargetCapacity>=0) and (TargetCapacity<MaxListSize) then begin
  if TargetCapacity<256 then begin
   CapacityMask:=15;
  end else if TargetCapacity<1024 then begin
   CapacityMask:=255;
  end else if TargetCapacity<4096 then begin
   CapacityMask:=1023;
  end else if TargetCapacity<16384 then begin
   CapacityMask:=4095;
  end else if TargetCapacity<65536 then begin
   CapacityMask:=16383;
  end else begin
   CapacityMask:=65535;
  end;
  SetCapacity((TargetCapacity+CapacityMask+CapacityMinimium) and not CapacityMask);
 end;
end;

procedure TXMLClassList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  SetOptimalCapacity(NewCount);
  if InternalCount<NewCount then begin
   FillChar(InternalList^[InternalCount],(NewCount-InternalCount)*SizeOf(TXMLClass),#0);
  end;
  InternalCount:=NewCount;
 end;
end;

function TXMLClassList.Add(Item:TXMLClass):longint;
begin
 result:=InternalCount;
 SetCount(result+1);
 InternalList^[result]:=Item;
end;

function TXMLClassList.Append(Item:TXMLClass):longint;
begin
 result:=Add(Item);
end;

function TXMLClassList.AddList(List:TXMLClassList):longint;
var Counter,Index:longint;
begin
 result:=-1;
 for Counter:=0 to List.Count-1 do begin
  Index:=Add(List[Counter]);
  if Counter=0 then begin
   result:=Index;
  end;
 end;
end;

function TXMLClassList.AppendList(List:TXMLClassList):longint;
begin
 result:=AddList(List);
end;

function TXMLClassList.NewClass:TXMLClass;
var Item:TXMLClass;
begin
 Item:=TXMLClass.Create;
 Add(Item);
 result:=Item;
end;

procedure TXMLClassList.Insert(Index:longint;Item:TXMLClass);
var Counter:longint;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  SetCount(InternalCount+1);
  for Counter:=InternalCount-1 downto Index do begin
   InternalList^[Counter+1]:=InternalList^[Counter];
  end;
  InternalList^[Index]:=Item;
 end else if Index=InternalCount then begin
  Add(Item);
 end else if Index>InternalCount then begin
  SetCount(Index);
  Add(Item);
 end;
end;

procedure TXMLClassList.Delete(Index:longint);
var i,j:longint;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  j:=InternalCount-1;
  i:=Index;
  Move(InternalList^[i+1],InternalList^[i],(j-i)*SizeOf(TXMLClass));
  SetCount(j);
 end;
end;

procedure TXMLClassList.DeleteClass(Index:longint);
var i,j:longint;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  j:=InternalCount-1;
  i:=Index;
  if assigned(InternalList^[i]) then begin
   InternalList^[i].Free;
   InternalList^[i]:=nil;
  end;
  Move(InternalList^[i+1],InternalList^[i],(j-i)*SizeOf(TXMLClass));
  SetCount(j);
 end;
end;

function TXMLClassList.Remove(Item:TXMLClass):longint;
var i,j,k:longint;
begin
 result:=-1;
 k:=InternalCount;
 j:=-1;
 for i:=0 to k-1 do begin
  if InternalList^[i]=Item then begin
   j:=i;
   break;
  end;
 end;
 if j>=0 then begin
  dec(k);
  Move(InternalList^[j+1],InternalList^[j],(k-j)*SizeOf(TXMLClass));
  SetCount(k);
  result:=j;
 end;
end;

function TXMLClassList.RemoveClass(Item:TXMLClass):longint;
var i,j,k:longint;
begin
 result:=-1;
 k:=InternalCount;
 j:=-1;
 for i:=0 to k-1 do begin
  if InternalList^[i]=Item then begin
   j:=i;
   break;
  end;
 end;
 if j>=0 then begin
  dec(k);
  Move(InternalList^[j+1],InternalList^[j],(k-j)*SizeOf(TXMLClass));
  SetCount(k);
  Item.Free;
  result:=j;
 end;
end;

function TXMLClassList.Find(Item:TXMLClass):longint;
var i:longint;
begin
 result:=-1;
 for i:=0 to InternalCount-1 do begin
  if InternalList^[i]=Item then begin
   result:=i;
   exit;
  end;
 end;
end;

function TXMLClassList.IndexOf(Item:TXMLClass):longint;
var i:longint;
begin
 result:=-1;
 for i:=0 to InternalCount-1 do begin
  if InternalList^[i]=Item then begin
   result:=i;
   exit;
  end;
 end;
end;

procedure TXMLClassList.Exchange(Index1,Index2:longint);
var TempPointer:TXMLClass;
begin
 if (Index1>=0) and (Index1<InternalCount) and (Index2>=0) and (Index2<InternalCount) then begin
  TempPointer:=InternalList^[Index1];
  InternalList^[Index1]:=InternalList^[Index2];
  InternalList^[Index2]:=TempPointer;
 end;
end;

function TXMLClassList.Push(Item:TXMLClass):longint;
begin
 result:=Add(Item);
end;

function TXMLClassList.Pop(var Item:TXMLClass):boolean;
begin
 result:=InternalCount>0;
 if result then begin
  Item:=InternalList^[InternalCount-1];
  Delete(InternalCount-1);
 end;
end;

function TXMLClassList.Pop:TXMLClass;
begin
 if InternalCount>0 then begin
  result:=InternalList^[InternalCount-1];
  Delete(InternalCount-1);
 end else begin
  result:=nil;
 end;
end;

function TXMLClassList.Last:TXMLClass;
begin
 if InternalCount>0 then begin
  result:=InternalList^[InternalCount-1];
 end else begin
  result:=nil;
 end;
end;

function TXMLClassList.GetItem(Index:longint):TXMLClass;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  result:=InternalList^[Index];
 end else begin
  result:=nil;
 end;
end;

procedure TXMLClassList.SetItem(Index:longint;Value:TXMLClass);
begin
 if (Index>=0) and (Index<InternalCount) then begin
  InternalList^[Index]:=Value;
 end;
end;

function TXMLClassList.GetItemPointer(Index:longint):TXMLClass;
begin
 if (Index>=0) and (Index<InternalCount) then begin
  result:=@InternalList^[Index];
 end else begin
  result:=nil;
 end;
end;

constructor TXMLClassLinkedList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=false;
 ClearNoFree;
end;

destructor TXMLClassLinkedList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TXMLClassLinkedList.Clear;
begin
 if ClearWithContentDestroying then begin
  ClearWithFree;
 end else begin
  ClearNoFree;
 end;
end;

procedure TXMLClassLinkedList.ClearNoFree;
var Current,Next:TXMLClass;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current.Next;
  Remove(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TXMLClassLinkedList.ClearWithFree;
var Current,Next:TXMLClass;
begin
 Current:=First;
 while assigned(Current) do begin
  Next:=Current.Next;
  RemoveClass(Current);
  Current:=Next;
 end;
 First:=nil;
 Last:=nil;
end;

procedure TXMLClassLinkedList.Add(Item:TXMLClass);
begin
 Item.Next:=nil;
 if assigned(Last) then begin
  Last.Next:=Item;
  Item.Previous:=Last;
 end else begin
  Item.Previous:=nil;
  First:=Item;
 end;
 Last:=Item;
end;

procedure TXMLClassLinkedList.Append(Item:TXMLClass);
begin
 Add(Item);
end;

procedure TXMLClassLinkedList.AddLinkedList(List:TXMLClassLinkedList);
begin
 Last.Next:=List.First;
 if assigned(List.First) then begin
  List.First.Previous:=Last;
 end;
 Last:=List.Last;
 List.First:=nil;
 List.Last:=nil;
end;

procedure TXMLClassLinkedList.AppendLinkedList(List:TXMLClassLinkedList);
begin
 AddLinkedList(List);
end;

procedure TXMLClassLinkedList.Remove(Item:TXMLClass);
begin
 if assigned(Item) then begin
  if assigned(Item.Next) then begin
   Item.Next.Previous:=Item.Previous;
  end else if Last=Item then begin
   Last:=Item.Previous;
  end;
  if assigned(Item.Previous) then begin
   Item.Previous.Next:=Item.Next;
  end else if First=Item then begin
   First:=Item.Next;
  end;
  Item.Previous:=nil;
  Item.Next:=nil;
 end;
end;

procedure TXMLClassLinkedList.RemoveClass(Item:TXMLClass);
begin
 if assigned(Item) then begin
  Remove(Item);
  Item.Destroy;
 end;
end;

procedure TXMLClassLinkedList.Push(Item:TXMLClass);
begin
 Add(Item);
end;

function TXMLClassLinkedList.Pop(var Item:TXMLClass):boolean;
begin
 result:=assigned(Last);
 if result then begin
  Item:=Last;
  Remove(Last);
 end;
end;

function TXMLClassLinkedList.Pop:TXMLClass;
begin
 result:=Last;
 if assigned(Last) then begin
  Remove(Last);
 end;
end;

function TXMLClassLinkedList.Count:longint;
var Current:TXMLClass;
begin
 result:=0;
 Current:=First;
 while assigned(Current) do begin
  inc(result);
  Current:=Current.Next;
 end;
end;

constructor TXMLStringTree.Create;
begin
 inherited Create;
 Root:=nil;
 Clear;
end;

destructor TXMLStringTree.Destroy;
begin
 Clear;
 inherited Destroy;
end;

function TXMLStringTree.CreateStringTreeNode(AChar:ansichar):PXMLStringTreeNode;
begin
 GetMem(result,SizeOf(TXMLStringTreeNode));
 result^.TheChar:=AChar;
 result^.Data:=0;
 result^.DataExist:=false;
 result^.Prevoius:=nil;
 result^.Next:=nil;
 result^.Up:=nil;
 result^.Down:=nil;
end;

procedure TXMLStringTree.DestroyStringTreeNode(Node:PXMLStringTreeNode);
begin
 if assigned(Node) then begin
  DestroyStringTreeNode(Node^.Next);
  DestroyStringTreeNode(Node^.Down);
  FreeMem(Node);
 end;
end;

procedure TXMLStringTree.Clear;
begin
 DestroyStringTreeNode(Root);
 Root:=nil;
end;

procedure TXMLStringTree.DumpTree;
var Ident:longint;
 procedure DumpNode(Node:PXMLStringTreeNode);
 var SubNode:PXMLStringTreeNode;
     IdentCounter,IdentOld:longint;
 begin
  for IdentCounter:=1 to Ident do begin
   write(' ');
  end;
  write(Node^.TheChar);
  IdentOld:=Ident;
  SubNode:=Node^.Next;
  while assigned(SubNode) do begin
   write(SubNode.TheChar);
   if not assigned(SubNode^.Next) then begin
    break;
   end;
   inc(Ident);
   SubNode:=SubNode^.Next;
  end;
  writeln;
  inc(Ident);
  while assigned(SubNode) and (SubNode<>Node) do begin
   if assigned(SubNode^.Down) then begin
    DumpNode(SubNode^.Down);
   end;
   SubNode:=SubNode^.Prevoius;
   dec(Ident);
  end;
  Ident:=IdentOld;
  if assigned(Node^.Down) then begin
   DumpNode(Node^.Down);
  end;
 end;
begin
 Ident:=0;
 DumpNode(Root);
end;

procedure TXMLStringTree.DumpList;
 procedure DumpNode(Node:PXMLStringTreeNode;ParentStr:ansistring);
 begin
  if assigned(Node) then begin
   ParentStr:=ParentStr;
   if Node^.DataExist then begin
    writeln(ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Next) then begin
    DumpNode(Node^.Next,ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Down) then begin
    DumpNode(Node^.Down,ParentStr);
   end;
  end;
 end;
begin
 if assigned(Root) then begin
  DumpNode(Root,'');
 end;
end;

procedure TXMLStringTree.AppendTo(DestStringTree:TXMLStringTree);
 procedure DumpNode(Node:PXMLStringTreeNode;ParentStr:ansistring);
 begin
  if assigned(Node) then begin
   ParentStr:=ParentStr;
   if Node^.DataExist then begin
    DestStringTree.Add(ParentStr+Node^.TheChar,Node^.Data);
   end;
   if assigned(Node^.Next) then begin
    DumpNode(Node^.Next,ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Down) then begin
    DumpNode(Node^.Down,ParentStr);
   end;
  end;
 end;
begin
 if assigned(DestStringTree) and assigned(Root) then begin
  DumpNode(Root,'');
 end;
end;

procedure TXMLStringTree.Optimize(DestStringTree:TXMLStringTree);
 procedure DumpNode(Node:PXMLStringTreeNode;ParentStr:ansistring);
 begin
  if assigned(Node) then begin
   ParentStr:=ParentStr;
   if Node^.DataExist then begin
    DestStringTree.Add(ParentStr+Node^.TheChar,Node^.Data);
   end;
   if assigned(Node^.Next) then begin
    DumpNode(Node^.Next,ParentStr+Node^.TheChar);
   end;
   if assigned(Node^.Down) then begin
    DumpNode(Node^.Down,ParentStr);
   end;
  end;
 end;
begin
 if assigned(DestStringTree) then begin
  DestStringTree.Clear;
  if assigned(Root) then begin
   DumpNode(Root,'');
  end;
 end;
end;

function TXMLStringTree.Add(Content:ansistring;Data:TXMLStringTreeData;Replace:boolean=false):boolean;
var StringLength,Position,PositionCounter:longint;
    NewNode,LastNode,Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  LastNode:=nil;
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    if NodeChar=StringChar then begin
     LastNode:=Node;
     Node:=Node^.Next;
    end else begin
     while (NodeChar<StringChar) and assigned(Node^.Down) do begin
      Node:=Node^.Down;
      NodeChar:=Node^.TheChar;
     end;
     if NodeChar=StringChar then begin
      LastNode:=Node;
      Node:=Node^.Next;
     end else begin
      NewNode:=CreateStringTreeNode(StringChar);
      if NodeChar<StringChar then begin
       NewNode^.Down:=Node^.Down;
       NewNode^.Up:=Node;
       if assigned(NewNode^.Down) then begin
        NewNode^.Down^.Up:=NewNode;
       end;
       NewNode^.Prevoius:=Node^.Prevoius;
       Node^.Down:=NewNode;
      end else if NodeChar>StringChar then begin
       NewNode^.Down:=Node;
       NewNode^.Up:=Node^.Up;
       if assigned(NewNode^.Up) then begin
        NewNode^.Up^.Down:=NewNode;
       end;
       NewNode^.Prevoius:=Node^.Prevoius;
       if not assigned(NewNode^.Up) then begin
        if assigned(NewNode^.Prevoius) then begin
         NewNode^.Prevoius^.Next:=NewNode;
        end else begin
         Root:=NewNode;
        end;
       end;
       Node^.Up:=NewNode;
      end;
      LastNode:=NewNode;
      Node:=LastNode^.Next;
     end;
    end;
   end else begin
    for PositionCounter:=Position to StringLength do begin
     NewNode:=CreateStringTreeNode(Content[PositionCounter]);
     if assigned(LastNode) then begin
      NewNode^.Prevoius:=LastNode;
      LastNode^.Next:=NewNode;
      LastNode:=LastNode^.Next;
     end else begin
      if not assigned(Root) then begin
       Root:=NewNode;
       LastNode:=Root;
      end;
     end;
    end;
    break;
   end;
  end;
  if assigned(LastNode) then begin
   if Replace or not LastNode^.DataExist then begin
    LastNode^.Data:=Data;
    LastNode^.DataExist:=true;
    result:=true;
   end;
  end;
 end;
end;

function TXMLStringTree.Delete(Content:ansistring):boolean;
var StringLength,Position:longint;
    Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.DataExist then begin
      Node^.DataExist:=false;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TXMLStringTree.Find(Content:ansistring;var Data:TXMLStringTreeData):boolean;
var StringLength,Position:longint;
    Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if (Position=StringLength) and Node^.DataExist then begin
      Data:=Node^.Data;
      result:=true;
      exit;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

function TXMLStringTree.FindEx(Content:ansistring;var Data:TXMLStringTreeData;var Len:longint):boolean;
var StringLength,Position:longint;
    Node:PXMLStringTreeNode;
    StringChar,NodeChar:ansichar;
begin
 result:=false;
 Len:=0;
 StringLength:=length(Content);
 if StringLength>0 then begin
  Node:=Root;
  for Position:=1 to StringLength do begin
   StringChar:=Content[Position];
   if assigned(Node) then begin
    NodeChar:=Node^.TheChar;
    while (NodeChar<>StringChar) and assigned(Node^.Down) do begin
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    end;
    if NodeChar=StringChar then begin
     if Node^.DataExist then begin
      Len:=Position;
      Data:=Node^.Data;
      result:=true;
     end;
     Node:=Node^.Next;
    end else begin
     break;
    end;
   end else begin
    break;
   end;
  end;
 end;
end;

constructor TXMLItem.Create;
begin
 inherited Create;
 Items:=TXMLItemList.Create;
end;

destructor TXMLItem.Destroy;
begin
 Items.Destroy;
 inherited Destroy;
end;

procedure TXMLItem.Clear;
begin
 Items.Clear;
end;

procedure TXMLItem.Add(Item:TXMLItem);
begin
 Items.Add(Item);
end;

procedure TXMLItem.Assign(From:TXMLItem);
var i:longint;
    NewItem:TXMLItem;
begin
 Items.ClearWithFree;
 NewItem:=nil;
 for i:=0 to Items.Count-1 do begin
  if Items[i] is TXMLTag then begin
   NewItem:=TXMLTag.Create;
  end else if Items[i] is TXMLCommentTag then begin
   NewItem:=TXMLCommentTag.Create;
  end else if Items[i] is TXMLScriptTag then begin
   NewItem:=TXMLScriptTag.Create;
  end else if Items[i] is TXMLProcessTag then begin
   NewItem:=TXMLProcessTag.Create;
  end else if Items[i] is TXMLCDATATag then begin
   NewItem:=TXMLCDATATag.Create;
  end else if Items[i] is TXMLDOCTYPETag then begin
   NewItem:=TXMLDOCTYPETag.Create;
  end else if Items[i] is TXMLExtraTag then begin
   NewItem:=TXMLExtraTag.Create;
  end else if Items[i] is TXMLText then begin
   NewItem:=TXMLText.Create;
  end else if Items[i] is TXMLItem then begin
   NewItem:=Items[i].Create;
  end else begin
   continue;
  end;
  NewItem.Assign(Items[i]);
  Items.Add(NewItem);
 end;
end;

function TXMLItem.FindTag(const TagName:ansistring):TXMLTag;
begin
 result:=Items.FindTag(TagName);
end;

constructor TXMLItemList.Create;
begin
 inherited Create;
 ClearWithContentDestroying:=true;
//CapacityMask:=$f;
 CapacityMinimium:=0;
end;

destructor TXMLItemList.Destroy;
begin
 ClearWithFree;
 inherited Destroy;
end;

function TXMLItemList.NewClass:TXMLItem;
begin
 result:=TXMLItem.Create;
 Add(result);
end;

function TXMLItemList.GetItem(Index:longint):TXMLItem;
begin
 result:=TXMLItem(inherited Items[Index]);
end;

procedure TXMLItemList.SetItem(Index:longint;Value:TXMLItem);
begin
 inherited Items[Index]:=Value;
end;

function TXMLItemList.FindTag(const TagName:ansistring):TXMLTag;
var i:longint;
    Item:TXMLItem;
begin
 result:=nil;
 for i:=0 to Count-1 do begin
  Item:=TXMLItem(inherited Items[i]);
  if (assigned(Item) and (Item is TXMLTag)) and (TXMLTag(Item).Name=TagName) then begin
   result:=TXMLTag(Item);
   break;
  end;
 end;
end;

constructor TXMLParameter.Create;
begin
 inherited Create;
 Name:='';
 Value:='';
end;

destructor TXMLParameter.Destroy;
begin
 Name:='';
 Value:='';
 inherited Destroy;
end;

procedure TXMLParameter.Assign(From:TXMLParameter);
begin
 Name:=From.Name;
 Value:=From.Value;
end;

constructor TXMLText.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLText.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLText.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLText then begin
  Text:=TXMLText(From).Text;
 end;
end;

procedure TXMLText.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLCommentTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLCommentTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLCommentTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLCommentTag then begin
  Text:=TXMLCommentTag(From).Text;
 end;
end;

procedure TXMLCommentTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLTag.Create;
begin
 inherited Create;
 Name:='';
 Parameter:=nil;
end;

destructor TXMLTag.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TXMLTag.Clear;
var Counter:longint;
begin
 inherited Clear;
 for Counter:=0 to length(Parameter)-1 do begin
  Parameter[Counter].Free;
 end;
 SetLength(Parameter,0);
 Name:='';
end;

procedure TXMLTag.Assign(From:TXMLItem);
var Counter:longint;
begin
 inherited Assign(From);
 if From is TXMLTag then begin
  for Counter:=0 to length(Parameter)-1 do begin
   Parameter[Counter].Free;
  end;
  SetLength(Parameter,0);
  Name:=TXMLTag(From).Name;
  for Counter:=0 to length(TXMLTag(From).Parameter)-1 do begin
   AddParameter(TXMLTag(From).Parameter[Counter].Name,TXMLTag(From).Parameter[Counter].Value);
  end;
 end;
end;

function TXMLTag.FindParameter(ParameterName:ansistring):TXMLParameter;
var i:longint;
begin
 for i:=0 to length(Parameter)-1 do begin
  if Parameter[i].Name=ParameterName then begin
   result:=Parameter[i];
   exit;
  end;
 end;
 result:=nil;
end;

function TXMLTag.GetParameter(ParameterName:ansistring;default:ansistring=''):ansistring;
var i:longint;
begin
 for i:=0 to length(Parameter)-1 do begin
  if Parameter[i].Name=ParameterName then begin
   result:=Parameter[i].Value;
   exit;
  end;
 end;
 result:=default;
end;

function TXMLTag.AddParameter(AParameter:TXMLParameter):boolean;
var Index:longint;
begin
 try
  Index:=length(Parameter);
  SetLength(Parameter,Index+1);
  Parameter[Index]:=AParameter;
  result:=true;
 except
  result:=false;
 end;
end;

function TXMLTag.AddParameter(Name:ansistring;Value:TXMLString):boolean;
var AParameter:TXMLParameter;
begin
 AParameter:=TXMLParameter.Create;
 AParameter.Name:=Name;
 AParameter.Value:=Value;
 result:=AddParameter(AParameter);
end;

function TXMLTag.RemoveParameter(AParameter:TXMLParameter):boolean;
var Found,Counter:longint;
begin
 result:=false;
 try
  Found:=-1;
  for Counter:=0 to length(Parameter)-1 do begin
   if Parameter[Counter]=AParameter then begin
    Found:=Counter;
    break;
   end;
  end;
  if Found>=0 then begin
   for Counter:=Found to length(Parameter)-2 do begin
    Parameter[Counter]:=Parameter[Counter+1];
   end;
   SetLength(Parameter,length(Parameter)-1);
   AParameter.Destroy;
   result:=true;
  end;
 except
 end;
end;

function TXMLTag.RemoveParameter(ParameterName:ansistring):boolean;
begin
 result:=RemoveParameter(FindParameter(ParameterName));
end;

constructor TXMLProcessTag.Create;
begin
 inherited Create;
end;

destructor TXMLProcessTag.Destroy;
begin
 inherited Destroy;
end;

procedure TXMLProcessTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
end;

constructor TXMLScriptTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLScriptTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLScriptTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLScriptTag then begin
  Text:=TXMLScriptTag(From).Text;
 end;
end;

procedure TXMLScriptTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLCDataTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLCDataTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLCDataTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLCDataTag then begin
  Text:=TXMLCDataTag(From).Text;
 end;
end;

procedure TXMLCDataTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLDOCTYPETag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLDOCTYPETag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLDOCTYPETag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLDOCTYPETag then begin
  Text:=TXMLDOCTYPETag(From).Text;
 end;
end;

procedure TXMLDOCTYPETag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXMLExtraTag.Create;
begin
 inherited Create;
 Text:='';
end;

destructor TXMLExtraTag.Destroy;
begin
 Text:='';
 inherited Destroy;
end;

procedure TXMLExtraTag.Assign(From:TXMLItem);
begin
 inherited Assign(From);
 if From is TXMLExtraTag then begin
  Text:=TXMLExtraTag(From).Text;
 end;
end;

procedure TXMLExtraTag.SetText(AText:ansistring);
begin
 Text:=AText;
end;

constructor TXML.Create;
begin
 inherited Create;
 InitializeEntites;
 Root:=TXMLItem.Create;
 AutomaticAloneTagDetection:=true;
 FormatIndent:=true;
 FormatIndentText:=false;
end;

destructor TXML.Destroy;
begin
 Root.Free;
 inherited Destroy;
end;

procedure TXML.Assign(From:TXML);
begin
 Root.Assign(From.Root);
 AutomaticAloneTagDetection:=From.AutomaticAloneTagDetection;
 FormatIndent:=From.FormatIndent;
 FormatIndentText:=From.FormatIndentText;
end;

function TXML.Parse(Stream:TStream):boolean;
const NameCanBeginWithCharSet:set of ansichar=['A'..'Z','a'..'z','_'];
      NameCanContainCharSet:set of ansichar=['A'..'Z','a'..'z','0'..'9','.',':','_','-'];
      BlankCharSet:set of ansichar=[#0..#$20];//[#$9,#$A,#$D,#$20];
type TEncoding=(etASCII,etUTF8,etUTF16);
var Errors:boolean;
    CurrentChar:ansichar;
    StreamEOF:boolean;
    Encoding:TEncoding;

 function IsEOF:boolean;
 begin
  result:=StreamEOF or (Stream.Position>Stream.Size);
 end;

 function IsEOFOrErrors:boolean;
 begin
  result:=IsEOF or Errors;
 end;

 function NextChar:ansichar;
 begin
  if Stream.Read(CurrentChar,SizeOf(ansichar))<>SizeOf(ansichar) then begin
   StreamEOF:=true;
   CurrentChar:=#0;
  end;
  result:=CurrentChar;
//system.Write(result);
 end;

 procedure SkipBlank;
 begin
  while (CurrentChar in BlankCharSet) and not IsEOFOrErrors do begin
   NextChar;
  end;
 end;

 function GetName:ansistring;
 var i:longint;
 begin
  result:='';
  i:=0;
  if (CurrentChar in NameCanBeginWithCharSet) and not IsEOFOrErrors then begin
   while (CurrentChar in NameCanContainCharSet) and not IsEOFOrErrors do begin
    inc(i);
    if (i+1)>length(result) then begin
     SetLength(result,NextPowerOfTwo(i+1));
    end;
    result[i]:=CurrentChar;
    NextChar;
   end;
  end;
  SetLength(result,i);
 end;

 function ExpectToken(const S:ansistring):boolean; overload;
 var i:longint;
 begin
  result:=true;
  for i:=1 to length(S) do begin
   if S[i]<>CurrentChar then begin
    result:=false;
    break;
   end;
   NextChar;
  end;
 end;

 function ExpectToken(const c:ansichar):boolean; overload;
 begin
  result:=false;
  if c=CurrentChar then begin
   result:=true;
   NextChar;
  end;
 end;

 function GetUntil(var Content:ansistring;const TerminateToken:ansistring):boolean;
 var i,j,OldPosition:longint;
     OldEOF:boolean;
     OldChar:ansichar;
 begin
  result:=false;
  j:=0;
  Content:='';
  while not IsEOFOrErrors do begin
   if (length(TerminateToken)>0) and (TerminateToken[1]=CurrentChar) and (((Stream.Size-Stream.Position)+1)>=length(TerminateToken)) then begin
    OldPosition:=Stream.Position;
    OldEOF:=StreamEOF;
    OldChar:=CurrentChar;
    for i:=1 to length(TerminateToken) do begin
     if TerminateToken[i]=CurrentChar then begin
      if i=length(TerminateToken) then begin
       NextChar;
       SetLength(Content,j);
       result:=true;
       exit;
      end;
     end else begin
      break;
     end;
     NextChar;
    end;
    Stream.Seek(OldPosition,soFromBeginning);
    StreamEOF:=OldEOF;
    CurrentChar:=OldChar;
   end;
   inc(j);
   if (j+1)>length(Content) then begin
    SetLength(Content,NextPowerOfTwo(j+1));
   end;
   Content[j]:=CurrentChar;
   NextChar;
  end;
  SetLength(Content,j);
 end;

 function GetDecimalValue:longint;
 var Negitive:boolean;
 begin
  Negitive:=CurrentChar='-';
  if Negitive then begin
   NextChar;
  end else if CurrentChar='+' then begin
   NextChar;
  end;
  result:=0;
  while (CurrentChar in ['0'..'9']) and not IsEOFOrErrors do begin
   result:=(result*10)+(ord(CurrentChar)-ord('0'));
   NextChar;
  end;
  if Negitive then begin
   result:=-result;
  end;
 end;

 function GetHeximalValue:longint;
 var Negitive:boolean;
     Value:longint;
 begin
  Negitive:=CurrentChar='-';
  if Negitive then begin
   NextChar;
  end else if CurrentChar='+' then begin
   NextChar;
  end;
  result:=0;
  Value:=0;
  while not IsEOFOrErrors do begin
   case CurrentChar of
    '0'..'9':begin
     Value:=byte(CurrentChar)-ord('0');
    end;
    'A'..'F':begin
     Value:=byte(CurrentChar)-ord('A')+$a;
    end;
    'a'..'f':begin
     Value:=byte(CurrentChar)-ord('a')+$a;
    end;
    else begin
     break;
    end;
   end;
   result:=(result*16)+Value;
   NextChar;
  end;
  if Negitive then begin
   result:=-result;
  end;
 end;

 function GetEntity:TXMLString;
 var Value:longint;
     Entity:ansistring;
     c:TXMLChar;
     EntityLink:TXMLStringTreeData;
 begin
  result:='';
  if CurrentChar='&' then begin
   NextChar;
   if not IsEOF then begin
    if CurrentChar='#' then begin
     NextChar;
     if IsEOF then begin
      Errors:=true;
     end else begin
      if CurrentChar='x' then begin
       NextChar;
       Value:=GetHeximalValue;
      end else begin
       Value:=GetDecimalValue;
      end;
      if CurrentChar=';' then begin
       NextChar;
{$ifdef UNICODE}
       c:=widechar(word(Value));
{$else}
       c:=ansichar(byte(Value));
{$endif}
       result:=c;
      end else begin
       Errors:=true;
      end;
     end;
    end else begin
     Entity:='&';
     while (CurrentChar in ['a'..'z','A'..'Z','0'..'9','_']) and not IsEOFOrErrors do begin
      Entity:=Entity+CurrentChar;
      NextChar;
     end;
     if CurrentChar=';' then begin
      Entity:=Entity+CurrentChar;
      NextChar;
      if EntityStringTree.Find(Entity,EntityLink) then begin
       result:=EntityChars[EntityLink,2];
      end else begin
       result:=Entity;
      end;
     end else begin
      Errors:=true;
     end;
    end;
   end;
  end;
 end;

 function ParseTagParameterValue(TerminateChar:ansichar):TXMLString;
 var i,wc,c:longint;
 begin
  result:='';
  SkipBlank;
  i:=0;
  while (CurrentChar<>TerminateChar) and not IsEOFOrErrors do begin
   if (Encoding=etUTF8) and (ord(CurrentChar)>=$80) then begin
    wc:=ord(CurrentChar) and $3f;
    if (wc and $20)<>0 then begin
     NextChar;
     c:=ord(CurrentChar);
     if (c and $c0)<>$80 then begin
      break;
     end;
     wc:=(wc shl 6) or (c and $3f);
    end;
    NextChar;
    c:=ord(CurrentChar);
    if (c and $c0)<>$80 then begin
     break;
    end;
    wc:=(wc shl 6) or (c and $3f);
    NextChar;
    inc(i);
    if (i+1)>length(result) then begin
     SetLength(result,NextPowerOfTwo(i+1));
    end;
{$ifdef UNICODE}
    result[i]:=widechar(wc);
{$else}
    result[i]:=ansichar(wc);
{$endif}
   end else if CurrentChar='&' then begin
    SetLength(result,i);
    result:=result+GetEntity;
    i:=length(result);
   end else begin
    inc(i);
    if (i+1)>length(result) then begin
     SetLength(result,NextPowerOfTwo(i+1));
    end;
{$ifdef UNICODE}
    result[i]:=widechar(word(byte(CurrentChar)+0));
{$else}
    result[i]:=CurrentChar;
{$endif}
    NextChar;
   end;
  end;
  SetLength(result,i);
  NextChar;
 end;

 procedure ParseTagParameter(XMLTag:TXMLTag);
 var ParameterName,ParameterValue:ansistring;
     TerminateChar:ansichar;
 begin
  SkipBlank;
  while (CurrentChar in NameCanBeginWithCharSet) and not IsEOFOrErrors do begin
   ParameterName:=GetName;
   SkipBlank;
   if CurrentChar='=' then begin
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     break;
    end;
   end else begin
    Errors:=true;
    break;
   end;
   SkipBlank;
   if CurrentChar in ['''','"'] then begin
    TerminateChar:=CurrentChar;
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     break;
    end;
    ParameterValue:=ParseTagParameterValue(TerminateChar);
    if Errors then begin
     break;
    end else begin
     XMLTag.AddParameter(ParameterName,ParameterValue);
     SkipBlank;
    end;
   end else begin
    Errors:=true;
    break;
   end;
  end;
 end;

 procedure Process(ParentItem:TXMLItem;Closed:boolean);
 var FinishLevel:boolean;

  procedure ParseText;
  var Text:TXMLString;
      XMLText:TXMLText;
      i,wc,c:longint;
{$ifndef UNICODE}
      w:ansistring;
{$endif}
  begin
   SkipBlank;
   if CurrentChar='<' then begin
    exit;
   end;
   i:=0;
   Text:='';
   SetLength(Text,16);
   while (CurrentChar<>'<') and not IsEOFOrErrors do begin
    if (Encoding=etUTF8) and (ord(CurrentChar)>=$80) then begin
     wc:=ord(CurrentChar) and $3f;
     if (wc and $20)<>0 then begin
      NextChar;
      c:=ord(CurrentChar);
      if (c and $c0)<>$80 then begin
       break;
      end;
      wc:=(wc shl 6) or (c and $3f);
     end;
     NextChar;
     c:=ord(CurrentChar);
     if (c and $c0)<>$80 then begin
      break;
     end;
     wc:=(wc shl 6) or (c and $3f);
     NextChar;
{$ifdef UNICODE}
     if wc<=$d7ff then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word(wc));
     end else if wc<=$dfff then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=#$fffd;
     end else if wc<=$fffd then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word(wc));
     end else if wc<=$ffff then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=#$fffd;
     end else if wc<=$10ffff then begin
      dec(wc,$10000);
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word((wc shr 10) or $d800));
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=widechar(word((wc and $3ff) or $dc00));
     end else begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=#$fffd;
     end;
{$else}
     if wc<$80 then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=ansichar(byte(wc));
     end else begin
      w:=UTF32CharToUTF8(wc);
      if length(w)>0 then begin
       inc(i);
       if (i+length(w)+1)>length(Text) then begin
        SetLength(Text,NextPowerOfTwo(i+length(w)+1));
       end;
       Move(w[1],Text[i],length(w));
       inc(i,length(w)-1);
      end;
     end;
{$endif}
    end else if CurrentChar='&' then begin
     SetLength(Text,i);
     Text:=Text+GetEntity;
     i:=length(Text);
    end else if CurrentChar in BlankCharSet then begin
{$ifdef UNICODE}
     inc(i);
     if (i+1)>length(Text) then begin
      SetLength(Text,NextPowerOfTwo(i+1));
     end;
     Text[i]:=widechar(word(byte(CurrentChar)+0));
{$else}
     wc:=ord(CurrentChar);
     if wc<$80 then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=ansichar(byte(wc));
     end else begin
      w:=UTF32CharToUTF8(wc);
      if length(w)>0 then begin
       inc(i);
       if (i+length(w)+1)>length(Text) then begin
        SetLength(Text,NextPowerOfTwo(i+length(w)+1));
       end;
       Move(w[1],Text[i],length(w));
       inc(i,length(w)-1);
      end;
     end;
{$endif}
     SkipBlank;
    end else begin
{$ifdef UNICODE}
     inc(i);
     if (i+1)>length(Text) then begin
      SetLength(Text,NextPowerOfTwo(i+1));
     end;
     Text[i]:=widechar(word(byte(CurrentChar)+0));
{$else}
     wc:=ord(CurrentChar);
     if wc<$80 then begin
      inc(i);
      if (i+1)>length(Text) then begin
       SetLength(Text,NextPowerOfTwo(i+1));
      end;
      Text[i]:=ansichar(byte(wc));
     end else begin
      w:=UTF32CharToUTF8(wc);
      if length(w)>0 then begin
       inc(i);
       if (i+length(w)+1)>length(Text) then begin
        SetLength(Text,NextPowerOfTwo(i+length(w)+1));
       end;
       Move(w[1],Text[i],length(w));
       inc(i,length(w)-1);
      end;
     end;
{$endif}
     NextChar;
    end;
   end;
   SetLength(Text,i);
   if length(Text)<>0 then begin
    XMLText:=TXMLText.Create;
    XMLText.Text:=Text;
    ParentItem.Add(XMLText);
   end;
  end;

  procedure ParseProcessTag;
  var TagName,EncodingName:ansistring;
      XMLProcessTag:TXMLProcessTag;
  begin
   if not ExpectToken('?') then begin
    Errors:=true;
    exit;
   end;
   TagName:=GetName;
   if IsEOF or Errors then begin
    Errors:=true;
    exit;
   end;
   XMLProcessTag:=TXMLProcessTag.Create;
   XMLProcessTag.Name:=TagName;
   ParentItem.Add(XMLProcessTag);
   ParseTagParameter(XMLProcessTag);
   if not ExpectToken('?>') then begin
    Errors:=true;
    exit;
   end;
   if XMLProcessTag.Name='xml' then begin
    EncodingName:=UPPERCASE(XMLProcessTag.GetParameter('encoding','ascii'));
    if EncodingName='UTF-8' then begin
     Encoding:=etUTF8;
    end else if EncodingName='UTF-16' then begin
     Encoding:=etUTF16;
    end else begin
     Encoding:=etASCII;
    end;
   end;
  end;

  procedure ParseScriptTag;
  var XMLScriptTag:TXMLScriptTag;
  begin
   if not ExpectToken('%') then begin
    Errors:=true;
    exit;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;
   XMLScriptTag:=TXMLScriptTag.Create;
   ParentItem.Add(XMLScriptTag);
   if not GetUntil(XMLScriptTag.Text,'%>') then begin
    Errors:=true;
   end;
  end;

  procedure ParseCommentTag;
  var XMLCommentTag:TXMLCommentTag;
  begin
   if not ExpectToken('--') then begin
    Errors:=true;
    exit;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;
   XMLCommentTag:=TXMLCommentTag.Create;
   ParentItem.Add(XMLCommentTag);
   if not GetUntil(XMLCommentTag.Text,'-->') then begin
    Errors:=true;
   end;
  end;

  procedure ParseCDATATag;
  var XMLCDataTag:TXMLCDataTag;
  begin
   if not ExpectToken('[CDATA[') then begin
    Errors:=true;
    exit;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;
   XMLCDataTag:=TXMLCDataTag.Create;
   ParentItem.Add(XMLCDataTag);
   if not GetUntil(XMLCDataTag.Text,']]>') then begin
    Errors:=true;
   end;
  end;

  procedure ParseDOCTYPEOrExtraTag;
  var Content:ansistring;
      XMLDOCTYPETag:TXMLDOCTYPETag;
      XMLExtraTag:TXMLExtraTag;
  begin
   Content:='';
   if not GetUntil(Content,'>') then begin
    Errors:=true;
    exit;
   end;
   if POS('DOCTYPE',Content)=1 then begin
    XMLDOCTYPETag:=TXMLDOCTYPETag.Create;
    ParentItem.Add(XMLDOCTYPETag);
    XMLDOCTYPETag.Text:=TRIMLEFT(COPY(Content,8,length(Content)-7));
   end else begin
    XMLExtraTag:=TXMLExtraTag.Create;
    ParentItem.Add(XMLExtraTag);
    XMLExtraTag.Text:=Content;
   end;
  end;

  procedure ParseTag;
  var TagName:ansistring;
      XMLTag:TXMLTag;
      IsAloneTag:boolean;
  begin
   if CurrentChar='/' then begin
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     exit;
    end;
    TagName:='/'+GetName;
   end else begin
    TagName:=GetName;
   end;
   if IsEOFOrErrors then begin
    Errors:=true;
    exit;
   end;

   XMLTag:=TXMLTag.Create;
   XMLTag.Name:=TagName;
   ParseTagParameter(XMLTag);

   IsAloneTag:=CurrentChar='/';
   if IsAloneTag then begin
    NextChar;
    if IsEOFOrErrors then begin
     Errors:=true;
     exit;
    end;
   end;

   if CurrentChar<>'>' then begin
    Errors:=true;
    exit;
   end;
   NextChar;

   if (ParentItem<>Root) and (ParentItem is TXMLTag) and (XMLTag.Name='/'+TXMLTag(ParentItem).Name) then begin
    XMLTag.Destroy;
    FinishLevel:=true;
    Closed:=true;
   end else begin
    ParentItem.Add(XMLTag);
    if not IsAloneTag then begin
     Process(XMLTag,false);
    end;
   end;
// IsAloneTag:=false;
  end;

 begin
  FinishLevel:=false;
  while not (IsEOFOrErrors or FinishLevel) do begin
   ParseText;
   if CurrentChar='<' then begin
    NextChar;
    if not IsEOFOrErrors then begin
     if CurrentChar='/' then begin
      ParseTag;
     end else if CurrentChar='?' then begin
      ParseProcessTag;
     end else if CurrentChar='%' then begin
      ParseScriptTag;
     end else if CurrentChar='!' then begin
      NextChar;
      if not IsEOFOrErrors then begin
       if CurrentChar='-' then begin
        ParseCommentTag;
       end else if CurrentChar='[' then begin
        ParseCDATATag;
       end else begin
        ParseDOCTYPEOrExtraTag;
       end;
      end;
     end else begin
      ParseTag;
     end;
    end;
   end;
  end;
  if not Closed then begin
   Errors:=true;
  end;
 end;
begin
 Encoding:=etASCII;
 Errors:=false;
 CurrentChar:=#0;
 Root.Clear;
 StreamEOF:=false;
 Stream.Seek(0,soFromBeginning);
 NextChar;
 Process(Root,true);
 if Errors then begin
  Root.Clear;
 end;
 result:=not Errors;
end;

function TXML.Read(Stream:TStream):boolean;
begin
 result:=Parse(Stream);
end;

function TXML.Write(Stream:TStream;IdentSize:longint=2):boolean;
var IdentLevel:longint;
    Errors:boolean;
 procedure Process(Item:TXMLItem;DoIndent:boolean);
 var Line:ansistring;
     Counter:longint;
     TagWithSingleLineText,ItemsText:boolean;
  procedure WriteLineEx(Line:ansistring);
  begin
   if length(Line)>0 then begin
    if Stream.Write(Line[1],length(Line))<>length(Line) then begin
     Errors:=true;
    end;
   end;
  end;
  procedure WriteLine(Line:ansistring);
  begin
   if FormatIndent and DoIndent then begin
    Line:=Line+#10;
   end;
   if length(Line)>0 then begin
    if Stream.Write(Line[1],length(Line))<>length(Line) then begin
     Errors:=true;
    end;
   end;
  end;
 begin
  if not Errors then begin
   if assigned(Item) then begin
    inc(IdentLevel,IdentSize);
    Line:='';
    if FormatIndent and DoIndent then begin
     for Counter:=1 to IdentLevel do begin
      Line:=Line+' ';
     end;
    end;
    if Item is TXMLText then begin
     if FormatIndentText then begin
      Line:=Line+ConvertToEntities(TXMLText(Item).Text,IdentLevel);
     end else begin
      Line:=ConvertToEntities(TXMLText(Item).Text);
     end;
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLCommentTag then begin
     Line:=Line+'<!--'+TXMLCommentTag(Item).Text+'-->';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLProcessTag then begin
     Line:=Line+'<?'+TXMLProcessTag(Item).Name;
     for Counter:=0 to length(TXMLProcessTag(Item).Parameter)-1 do begin
      if assigned(TXMLProcessTag(Item).Parameter[Counter]) then begin
       Line:=Line+' '+TXMLProcessTag(Item).Parameter[Counter].Name+'="'+ConvertToEntities(TXMLProcessTag(Item).Parameter[Counter].Value)+'"';
      end;
     end;
     Line:=Line+'?>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLScriptTag then begin
     Line:=Line+'<%'+TXMLScriptTag(Item).Text+'%>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLCDataTag then begin
     Line:=Line+'<![CDATA['+TXMLCDataTag(Item).Text+']]>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLDOCTYPETag then begin
     Line:=Line+'<!DOCTYPE '+TXMLDOCTYPETag(Item).Text+'>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLExtraTag then begin
     Line:=Line+'<!'+TXMLExtraTag(Item).Text+'>';
     WriteLine(Line);
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end else if Item is TXMLTag then begin
     if AutomaticAloneTagDetection then begin
      TXMLTag(Item).IsAloneTag:=TXMLTag(Item).Items.Count=0;
     end;
     Line:=Line+'<'+TXMLTag(Item).Name;
     for Counter:=0 to length(TXMLTag(Item).Parameter)-1 do begin
      if assigned(TXMLTag(Item).Parameter[Counter]) then begin
       Line:=Line+' '+TXMLTag(Item).Parameter[Counter].Name+'="'+ConvertToEntities(TXMLTag(Item).Parameter[Counter].Value)+'"';
      end;
     end;
     if TXMLTag(Item).IsAloneTag then begin
      Line:=Line+' />';
      WriteLine(Line);
     end else begin
      TagWithSingleLineText:=false;
      if Item.Items.Count=1 then begin
       if assigned(Item.Items[0]) then begin
        if Item.Items[0] is TXMLText then begin
         if ((POS(#13,TXMLText(Item.Items[0]).Text)=0) and
             (POS(#10,TXMLText(Item.Items[0]).Text)=0)) or not FormatIndentText then begin
          TagWithSingleLineText:=true;
         end;
        end;
       end;
      end;
      ItemsText:=false;
      for Counter:=0 to Item.Items.Count-1 do begin
       if assigned(Item.Items[Counter]) then begin
        if Item.Items[Counter] is TXMLText then begin
         ItemsText:=true;
        end;
       end;
      end;
      if TagWithSingleLineText then begin
       Line:=Line+'>'+ConvertToEntities(TXMLText(Item.Items[0]).Text)+'</'+TXMLTag(Item).Name+'>';
       WriteLine(Line);
      end else if Item.Items.Count<>0 then begin
       Line:=Line+'>';
       if assigned(Item.Items[0]) and (Item.Items[0] is TXMLText) and not FormatIndentText then begin
        WriteLineEx(Line);
       end else begin
        WriteLine(Line);
       end;
       for Counter:=0 to Item.Items.Count-1 do begin
        Process(Item.Items[Counter],DoIndent and ((not ItemsText) or (FormatIndent and FormatIndentText)));
       end;
       Line:='';
       if DoIndent and ((not ItemsText) or (FormatIndent and FormatIndentText)) then begin
        for Counter:=1 to IdentLevel do begin
         Line:=Line+' ';
        end;
       end;
       Line:=Line+'</'+TXMLTag(Item).Name+'>';
       WriteLine(Line);
      end else begin
       Line:=Line+'></'+TXMLTag(Item).Name+'>';
       WriteLine(Line);
      end;
     end;
    end else begin
     for Counter:=0 to Item.Items.Count-1 do begin
      Process(Item.Items[Counter],DoIndent);
     end;
    end;
    dec(IdentLevel,IdentSize);
   end;
  end;
 end;
begin
 IdentLevel:=-(2*IdentSize);
 if Stream is TMemoryStream then begin
  TMemoryStream(Stream).Clear;
 end;
 Errors:=false;
 Process(Root,FormatIndent);
 result:=not Errors;
end;

function TXML.ReadXMLText:ansistring;
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 Write(Stream);
 if Stream.Size>0 then begin
  SetLength(result,Stream.Size);
  Stream.Seek(0,soFromBeginning);
  Stream.Read(result[1],Stream.Size);
 end else begin
  result:='';
 end;
 Stream.Destroy;
end;

procedure TXML.WriteXMLText(Text:ansistring);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 if length(Text)>0 then begin
  Stream.Write(Text[1],length(Text));
  Stream.Seek(0,soFromBeginning);
 end;
 Parse(Stream);
 Stream.Destroy;
end;

function ParseText(ParentItem:TXMLItem;const ExcludedTags:array of ansistring):ansistring;
var XMLItemIndex,Index:longint;
    XMLItem:TXMLItem;
    Found:boolean;
begin
 result:='';
 if assigned(ParentItem) then begin
  for XMLItemIndex:=0 to ParentItem.Items.Count-1 do begin
   XMLItem:=ParentItem.Items[XMLItemIndex];
   if assigned(XMLItem) then begin
    if XMLItem is TXMLText then begin
     result:=result+TXMLText(XMLItem).Text;
    end else if XMLItem is TXMLTag then begin
     Found:=false;
     for Index:=0 to length(ExcludedTags)-1 do begin
      if TXMLTag(XMLItem).Name=ExcludedTags[Index] then begin
       Found:=true;
       break;
      end;
     end;
     if not Found then begin
      if TXMLTag(XMLItem).Name='br' then begin
       result:=result+#13#10;
      end;
      result:=result+ParseText(XMLItem,ExcludedTags)+' ';
     end;
    end;
   end;
  end;
 end;
end;

function AlignPaddingString(const s:ansistring;const PaddingColumn:longint):ansistring;
begin
 result:=s;
 while length(result)<PaddingColumn do begin
  result:=result+' ';
 end;
end;

function WrapString(const InputString,LineBreak:ansistring;const MaxLineWidth:longint):ansistring;
var  i,j:longint;
     c:ansichar;
begin
 result:='';
 j:=0;
 for i:=1 to length(InputString) do begin
  c:=InputString[i];
  case c of
   #9,#32:begin
    if j>=MaxLineWidth then begin
     result:=result+LineBreak;
     j:=0;
    end else begin
     result:=result+c;
     inc(j);
    end;
   end;
   #13,#10:begin
    result:=result+c;
    j:=0;
   end;
   else begin
    result:=result+c;
    inc(j);
   end;
  end;
 end;
end;

type TVendorID=class
      public
       Name:ansistring;
       ID:longint;
       Comment:ansistring;
     end;

     TTag=class
      public
       Name:ansistring;
       Author:ansistring;
       Contact:ansistring;
     end;

     TExtensionOrFeature=class;

     TExtensionOrFeatureEnum=class
      public
       ExtensionOrFeature:TExtensionOrFeature;
       Name:ansistring;
       Value:ansistring;
       Offset:longint;
       ExtNumber:longint;
       BitPos:longint;
       Dir:ansistring;
       Extends:ansistring;
       Alias:ansistring;
     end;

     TExtensionOrFeatureType=class
      public
       Name:ansistring;
       Alias:ansistring;
     end;

     TExtensionOrFeatureCommand=class
      public
       Name:ansistring;
       Alias:ansistring;
     end;

     TExtensionOrFeature=class
      public
       Name:ansistring;
       Number:longint;
       Protect:ansistring;
       Author:ansistring;
       Contact:ansistring;
       Supported:ansistring;
       Enums:TObjectList;
       Types:TObjectList;
       Commands:TObjectList;
       constructor Create;
       destructor Destroy; override;
     end;

constructor TExtensionOrFeature.Create;
begin
 inherited Create;
 Name:='';
 Number:=0;
 Protect:='';
 Author:='';
 Contact:='';
 Supported:='';
 Enums:=TObjectList.Create(true);
 Types:=TObjectList.Create(true);
 Commands:=TObjectList.Create(true);
end;

destructor TExtensionOrFeature.Destroy;
begin
 Enums.Free;
 Types.Free;
 Commands.Free;
 inherited Destroy;
end;

var Comment:ansistring;
    VendorIDList:TObjectList;
    TagList:TObjectList;
    ExtensionsOrFeatures:TObjectList;
    ExtensionOrFeatureEnums:TStringList;
    ExtensionOrFeatureTypes:TStringList;
    ExtensionOrFeatureCommands:TStringList;
    VersionConstants:TStringList;
    BaseTypes:TStringList;
    BitMaskTypes:TStringList;
    HandleTypes:TStringList;
    ENumTypes:TStringList;
    AliasENumTypes:TStringList;
    ENumConstants:TStringList;
    ENumValues:TStringList;
    TypeDefinitionTypes:TStringList;
    TypeDefinitionConstructors:TStringList;
    CommandTypes:TStringList;
    CommandVariables:TStringList;
    AllCommandType:TStringList;
    AllCommands:TStringList;
    AllCommandClassDefinitions:TStringList;
    AllCommandClassImplementations:TStringList;

function TranslateName(Name_:ansistring):ansistring;
begin
 //reserved
 if Name_='function' then begin
   result := '_'+Name_;
 end else begin
   result := Name_;
 end;
end;

function TranslateType(Type_:ansistring;const Ptr:longint=0):ansistring;
begin
 case Ptr of
  1:begin
   if Type_='void' then begin
    result:='PXrVoid';
   end else if Type_='char' then begin
    result:='PXrChar';
   end else if Type_='float' then begin
    result:='PXrFloat';
   end else if Type_='double' then begin
    result:='PXrDouble';
   end else if Type_='int8_t' then begin
    result:='PXrInt8';
   end else if Type_='uint8_t' then begin
    result:='PXrUInt8';
   end else if Type_='int16_t' then begin
    result:='PXrInt16';
   end else if Type_='uint16_t' then begin
    result:='PXrUInt16';
   end else if (Type_='int32_t') or (Type_='int') then begin
    result:='PXrInt32';
   end else if (Type_='uint32_t') or (Type_='DWORD') then begin
    result:='PXrUInt32';
   end else if Type_='int64_t' then begin
    result:='PXrInt64';
   end else if Type_='uint64_t' then begin
    result:='PXrUInt64';
   end else if Type_='size_t' then begin
    result:='PXrSize';
   end else if Type_='XR_DEFINE_HANDLE' then begin
    result:='PXrDispatchableHandle';
   end else if Type_='XR_DEFINE_NON_DISPATCHABLE_HANDLE' then begin
    result:='PXrNonDispatchableHandle';
   end else if Type_='XR_DEFINE_ATOM' then begin
    result:='PXrAtom';
   end else if Type_='LARGE_INTEGER' then begin
    result:='PLargeInteger';
   end else if Type_='IUnknown' then begin
    result:='IUnknown';
   end else if Type_='ID3D11Device' then begin
    result:='ID3D11Device';
   end else if Type_='ID3D12Device' then begin
    result:='ID3D12Device';
   end else if Type_='ID3D12CommandQueue' then begin
    result:='ID3D12CommandQueue';
   end else if Type_='ID3D11Texture2D' then begin
    result:='ID3D11Texture2D';
   end else if Type_='ID3D12Resource' then begin
    result:='ID3D12Resource';
   end else if Type_='HINSTANCE' then begin
    result:='PXrHINSTANCE';
   end else if Type_='HWND' then begin
    result:='PXrHWND';
   end else if Type_='HMONITOR' then begin
    result:='PXrHMONITOR';
   end else if Type_='Display' then begin
    result:='PXrXLIBDisplay';
   end else if Type_='VisualID' then begin
    result:='PXrXLIBVisualID';
   end else if Type_='Window' then begin
    result:='PXrXLIBWindow';
   end else if Type_='xcb_connection_t' then begin
    result:='PXrXCBConnection';
   end else if Type_='xcb_visualid_t' then begin
    result:='PXrXCBVisualID';
   end else if Type_='xcb_window_t' then begin
    result:='PXrXCBWindow';
   end else if Type_='wl_display' then begin
    result:='PXrWaylandDisplay';
   end else if Type_='wl_surface' then begin
    result:='PXrWaylandSurface';
   end else if Type_='ANativeWindow' then begin
    result:='PXrAndroidANativeWindow';
   end else if Type_='AHardwareBuffer' then begin
    result:='PXrAndroidAHardwareBuffer';
   end else if Type_='SECURITY_ATTRIBUTES' then begin
    result:='PSecurityAttributes';
   end else if Type_='zx_handle_t' then begin
    result:='PXrFuchsiaZXHandle';
   end else if Type_='GgpStreamDescriptor' then begin
    result:='PXrGgpStreamDescriptor';
   end else if Type_='GgpFrameToken' then begin
    result:='PXrGgpFrameToken';
   end else if Type_='CAMetalLayer' then begin
    result:='PXrCAMetalLayer';
   end else if Type_='IDirectFB' then begin
    result:='PXrDirectFBIDirectFB';
   end else if Type_='IDirectFBSurface' then begin
    result:='PXrDirectFBIDirectFBSurface';
   end else begin
    result:='P'+Type_;
   end;
  end;
  2:begin
   if Type_='void' then begin
    result:='PPXrVoid';
   end else if Type_='char' then begin
    result:='PPXrChar';
   end else if Type_='float' then begin
    result:='PPXrFloat';
   end else if Type_='double' then begin
    result:='PPXrDouble';
   end else if Type_='int8_t' then begin
    result:='PPXrInt8';
   end else if Type_='uint8_t' then begin
    result:='PPXrUInt8';
   end else if Type_='int16_t' then begin
    result:='PPXrInt16';
   end else if Type_='uint16_t' then begin
    result:='PPXrUInt16';
   end else if (Type_='int32_t') or (Type_='int') then begin
    result:='PPXrInt32';
   end else if (Type_='uint32_t') or (Type_='DWORD') then begin
    result:='PPXrUInt32';
   end else if Type_='int64_t' then begin
    result:='PPXrInt64';
   end else if Type_='uint64_t' then begin
    result:='PPXrUInt64';
   end else if Type_='size_t' then begin
    result:='PPXrSize';
   end else if Type_='XR_DEFINE_HANDLE' then begin
    result:='PPXrDispatchableHandle';
   end else if Type_='XR_DEFINE_NON_DISPATCHABLE_HANDLE' then begin
    result:='PPXrNonDispatchableHandle';
   end else if Type_='IUnknown' then begin
    result:='PPIUnknown';
   end else if Type_='XR_DEFINE_ATOM' then begin
    result:='PPXrAtom';
   end else if Type_='HINSTANCE' then begin
    result:='PPXrHINSTANCE';
   end else if Type_='HWND' then begin
    result:='PPXrHWND';
   end else if Type_='HMONITOR' then begin
    result:='PPXrHMONITOR';
   end else if Type_='Display' then begin
    result:='PPXrXLIBDisplay';
   end else if Type_='VisualID' then begin
    result:='PPXrXLIBVisualID';
   end else if Type_='Window' then begin
    result:='PPXrXLIBWindow';
   end else if Type_='xcb_connection_t' then begin
    result:='PPXrXCBConnection';
   end else if Type_='xcb_visualid_t' then begin
    result:='PPXrXCBVisualID';
   end else if Type_='xcb_window_t' then begin
    result:='PPXrXCBWindow';
   end else if Type_='wl_display' then begin
    result:='PPXrWaylandDisplay';
   end else if Type_='wl_surface' then begin
    result:='PPXrWaylandSurface';
   end else if Type_='ANativeWindow' then begin
    result:='PPXrAndroidANativeWindow';
   end else if Type_='AHardwareBuffer' then begin
    result:='PPXrAndroidAHardwareBuffer';
   end else if Type_='zx_handle_t' then begin
    result:='PPXrFuchsiaZXHandle';
   end else if Type_='GgpStreamDescriptor' then begin
    result:='PPXrGgpStreamDescriptor';
   end else if Type_='GgpFrameToken' then begin
    result:='PPXrGgpFrameToken';
   end else if Type_='CAMetalLayer' then begin
    result:='PPXrCAMetalLayer';
   end else if Type_='IDirectFB' then begin
    result:='PPXrDirectFBIDirectFB';
   end else if Type_='IDirectFBSurface' then begin
    result:='PPXrDirectFBIDirectFBSurface';
   end else begin
    result:='PP'+Type_;
   end;
  end;
  else begin
   if Type_='void' then begin
    Assert(false,'TODO: Unexpected void data type');
    result:='TXrVoid';
   end else if Type_='char' then begin
    result:='TXrChar';
   end else if Type_='float' then begin
    result:='TXrFloat';
   end else if Type_='double' then begin
    result:='TXrDouble';
   end else if Type_='int8_t' then begin
    result:='TXrInt8';
   end else if Type_='uint8_t' then begin
    result:='TXrUInt8';
   end else if Type_='int16_t' then begin
    result:='TXrInt16';
   end else if Type_='uint16_t' then begin
    result:='TXrUInt16';
   end else if (Type_='int32_t') or (Type_='int') then begin
    result:='TXrInt32';
   end else if (Type_='uint32_t') or (Type_='DWORD') then begin
    result:='TXrUInt32';
   end else if Type_='int64_t' then begin
    result:='TXrInt64';
   end else if Type_='uint64_t' then begin
    result:='TXrUInt64';
   end else if Type_='size_t' then begin
    result:='TXrSize';
   end else if Type_='wchar_t' then begin
    result:='TXrWidechar';
   end else if Type_='XR_DEFINE_ATOM' then begin
    result:='TXrAtom';
   end else if Type_='HDC' then begin
    result:='TXrHDC';
   end else if Type_='HGLRC' then begin
    result:='TXrHGLRC';
   end else if Type_='XR_DEFINE_HANDLE' then begin
    result:='TXrDispatchableHandle';
   end else if Type_='XR_DEFINE_NON_DISPATCHABLE_HANDLE' then begin
    result:='TXrNonDispatchableHandle';
   end else if Type_='HINSTANCE' then begin
    result:='TXrHINSTANCE';
   end else if Type_='HWND' then begin
    result:='TXrHWND';
   end else if Type_='HMONITOR' then begin
    result:='TXrHMONITOR';
   end else if Type_='Display' then begin
    result:='TXrXLIBDisplay';
   end else if Type_='VisualID' then begin
    result:='TXrXLIBVisualID';
   end else if Type_='Window' then begin
    result:='TXrXLIBWindow';
   end else if Type_='xcb_connection_t' then begin
    result:='TXrXCBConnection';
   end else if Type_='xcb_visualid_t' then begin
    result:='TXrXCBVisualID';
   end else if Type_='xcb_window_t' then begin
    result:='TXrXCBWindow';
   end else if Type_='wl_display' then begin
    result:='TXrWaylandDisplay';
   end else if Type_='wl_surface' then begin
    result:='TXrWaylandSurface';
   end else if Type_='ANativeWindow' then begin
    result:='TXrAndroidANativeWindow';
   end else if Type_='AHardwareBuffer' then begin
    result:='TXrAndroidAHardwareBuffer';
   end else if Type_='LPCWSTR' then begin
    result:='PWideChar';
   end else if Type_='zx_handle_t' then begin
    result:='TXrFuchsiaZXHandle';
   end else if Type_='GgpStreamDescriptor' then begin
    result:='TXrGgpStreamDescriptor';
   end else if Type_='GgpFrameToken' then begin
    result:='TXrGgpFrameToken';
   end else if Type_='CAMetalLayer' then begin
    result:='TXrCAMetalLayer';
   end else if Type_='IDirectFB' then begin
    result:='TXrDirectFBIDirectFB';
   end else if Type_='IDirectFBSurface' then begin
    result:='TXrDirectFBIDirectFBSurface';
   end else if length(Type_)>0 then begin
    result:='T'+Type_;
   end else begin
    result:='TXrNonDefinedType';
   end;
  end;
 end;
end;

procedure ParseValidityTag(Tag:TXMLTag;const StringList:TStringList);
var i:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
begin
 if assigned(Tag) then begin
  for i:=0 to Tag.Items.Count-1 do begin
   ChildItem:=Tag.Items[i];
   if ChildItem is TXMLTag then begin
    ChildTag:=TXMLTag(ChildItem);
    if ChildTag.Name='usage' then begin
     StringList.Add(ParseText(ChildTag,['']));
    end;
   end;
  end;
 end;
end;

procedure ParseCommentTag(Tag:TXMLTag);
begin
 Comment:=ParseText(Tag,['']);
end;

procedure ParseExtensionsTag(Tag:TXMLTag);
var i,j,k:longint;
    ChildItem,ChildChildItem,ChildChildChildItem:TXMLItem;
    ChildTag,ChildChildTag,ChildChildChildTag:TXMLTag;
    Extension:TExtensionOrFeature;
    ExtensionOrFeatureEnum:TExtensionOrFeatureEnum;
    ExtensionOrFeatureType:TExtensionOrFeatureType;
    ExtensionOrFeatureCommand:TExtensionOrFeatureCommand;
begin
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='extension' then begin
    Extension:=TExtensionOrFeature.Create;
    ExtensionsOrFeatures.Add(Extension);
    Extension.Name:=ChildTag.GetParameter('name','');
    Extension.Number:=StrToIntDef(ChildTag.GetParameter('number','0'),0);
    Extension.Protect:=ChildTag.GetParameter('protect','');
    Extension.Author:=ChildTag.GetParameter('author','');
    Extension.Contact:=ChildTag.GetParameter('contact','');
    Extension.Supported:=ChildTag.GetParameter('supported','');
    for j:=0 to ChildTag.Items.Count-1 do begin
     ChildChildItem:=ChildTag.Items[j];
     if ChildChildItem is TXMLTag then begin
      ChildChildTag:=TXMLTag(ChildChildItem);
      if ChildChildTag.Name='require' then begin
       for k:=0 to ChildChildTag.Items.Count-1 do begin
        ChildChildChildItem:=ChildChildTag.Items[k];
        if ChildChildChildItem is TXMLTag then begin
         ChildChildChildTag:=TXMLTag(ChildChildChildItem);
         if ChildChildChildTag.Name='enum' then begin
          ExtensionOrFeatureEnum:=TExtensionOrFeatureEnum.Create;
          Extension.Enums.Add(ExtensionOrFeatureEnum);
          ExtensionOrFeatureEnum.ExtensionOrFeature:=Extension;
          ExtensionOrFeatureEnum.Name:=ChildChildChildTag.GetParameter('name','');
          ExtensionOrFeatureEnum.Value:=ChildChildChildTag.GetParameter('value','');
          ExtensionOrFeatureEnum.Offset:=StrToIntDef(ChildChildChildTag.GetParameter('offset','-1'),-1);
          ExtensionOrFeatureEnum.ExtNumber:=StrToIntDef(ChildChildChildTag.GetParameter('extnumber','-1'),-1);
          ExtensionOrFeatureEnum.BitPos:=StrToIntDef(ChildChildChildTag.GetParameter('bitpos','-1'),-1);
          ExtensionOrFeatureEnum.Dir:=ChildChildChildTag.GetParameter('dir','');
          ExtensionOrFeatureEnum.Extends:=ChildChildChildTag.GetParameter('extends','');
          ExtensionOrFeatureEnum.Alias:=ChildChildChildTag.GetParameter('alias','');
          ExtensionOrFeatureEnums.AddObject(ExtensionOrFeatureEnum.Name,ExtensionOrFeatureEnum);
         end else if ChildChildChildTag.Name='type' then begin
          ExtensionOrFeatureType:=TExtensionOrFeatureType.Create;
          Extension.Types.Add(ExtensionOrFeatureType);
          ExtensionOrFeatureType.Name:=ChildChildChildTag.GetParameter('name','');
          ExtensionOrFeatureType.Alias:=ChildChildChildTag.GetParameter('alias','');
          ExtensionOrFeatureTypes.AddObject(ExtensionOrFeatureType.Name,ExtensionOrFeatureType);
         end else if ChildChildChildTag.Name='command' then begin
          ExtensionOrFeatureCommand:=TExtensionOrFeatureCommand.Create;
          Extension.Commands.Add(ExtensionOrFeatureCommand);
          ExtensionOrFeatureCommand.Name:=ChildChildChildTag.GetParameter('name','');
          ExtensionOrFeatureCommand.Alias:=ChildChildChildTag.GetParameter('alias','');
          ExtensionOrFeatureCommands.AddObject(ExtensionOrFeatureCommand.Name,ExtensionOrFeatureCommand);
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
end;

function ParseFeatureTags(Tag:TXMLTag):longint;
var i,j,k:longint;
    ChildItem,ChildChildItem,ChildChildChildItem:TXMLItem;
    ChildTag,ChildChildTag,ChildChildChildTag:TXMLTag;
    Feature:TExtensionOrFeature;
    ExtensionOrFeatureEnum:TExtensionOrFeatureEnum;
    ExtensionOrFeatureType:TExtensionOrFeatureType;
    ExtensionOrFeatureCommand:TExtensionOrFeatureCommand;
begin
 result:=0;
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='feature' then begin
    inc(result);
    Feature:=TExtensionOrFeature.Create;
    ExtensionsOrFeatures.Add(Feature);
    Feature.Name:=ChildTag.GetParameter('name','');
    Feature.Number:=StrToIntDef(ChildTag.GetParameter('number','0'),0);
    Feature.Protect:=ChildTag.GetParameter('protect','');
    Feature.Author:=ChildTag.GetParameter('author','');
    Feature.Contact:=ChildTag.GetParameter('contact','');
    Feature.Supported:=ChildTag.GetParameter('supported','');
    for j:=0 to ChildTag.Items.Count-1 do begin
     ChildChildItem:=ChildTag.Items[j];
     if ChildChildItem is TXMLTag then begin
      ChildChildTag:=TXMLTag(ChildChildItem);
      if ChildChildTag.Name='require' then begin
       for k:=0 to ChildChildTag.Items.Count-1 do begin
        ChildChildChildItem:=ChildChildTag.Items[k];
        if ChildChildChildItem is TXMLTag then begin
         ChildChildChildTag:=TXMLTag(ChildChildChildItem);
         if ChildChildChildTag.Name='enum' then begin
          ExtensionOrFeatureEnum:=TExtensionOrFeatureEnum.Create;
          Feature.Enums.Add(ExtensionOrFeatureEnum);
          ExtensionOrFeatureEnum.ExtensionOrFeature:=Feature;
          ExtensionOrFeatureEnum.Name:=ChildChildChildTag.GetParameter('name','');
          ExtensionOrFeatureEnum.Value:=ChildChildChildTag.GetParameter('value','');
          ExtensionOrFeatureEnum.Offset:=StrToIntDef(ChildChildChildTag.GetParameter('offset','-1'),-1);
          ExtensionOrFeatureEnum.ExtNumber:=StrToIntDef(ChildChildChildTag.GetParameter('extnumber','-1'),-1);
          ExtensionOrFeatureEnum.BitPos:=StrToIntDef(ChildChildChildTag.GetParameter('bitpos','-1'),-1);
          ExtensionOrFeatureEnum.Dir:=ChildChildChildTag.GetParameter('dir','');
          ExtensionOrFeatureEnum.Extends:=ChildChildChildTag.GetParameter('extends','');
          ExtensionOrFeatureEnum.Alias:=ChildChildChildTag.GetParameter('alias','');

          ExtensionOrFeatureEnums.AddObject(ExtensionOrFeatureEnum.Name,ExtensionOrFeatureEnum);
         end else if ChildChildChildTag.Name='type' then begin
          ExtensionOrFeatureType:=TExtensionOrFeatureType.Create;
          Feature.Types.Add(ExtensionOrFeatureType);
          ExtensionOrFeatureType.Name:=ChildChildChildTag.GetParameter('name','');
          ExtensionOrFeatureType.Alias:=ChildChildChildTag.GetParameter('alias','');
          ExtensionOrFeatureTypes.AddObject(ExtensionOrFeatureType.Name,ExtensionOrFeatureType);
         end else if ChildChildChildTag.Name='command' then begin
          ExtensionOrFeatureCommand:=TExtensionOrFeatureCommand.Create;
          Feature.Commands.Add(ExtensionOrFeatureCommand);
          ExtensionOrFeatureCommand.Name:=ChildChildChildTag.GetParameter('name','');
          ExtensionOrFeatureCommand.Alias:=ChildChildChildTag.GetParameter('alias','');
          ExtensionOrFeatureCommands.AddObject(ExtensionOrFeatureCommand.Name,ExtensionOrFeatureCommand);
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure ProcessExtensions;
var i:longint;
    ExtensionOrFeature:TExtensionOrFeature;
    ExtensionOrFeatureEnum:TExtensionOrFeatureEnum;
    ExtensionOrFeatureEnumValue:string;
begin
 for i:=0 to ExtensionOrFeatureEnums.Count-1 do begin
  ExtensionOrFeatureEnum:=TExtensionOrFeatureEnum(ExtensionOrFeatureEnums.Objects[i]);
  ExtensionOrFeature:=ExtensionOrFeatureEnum.ExtensionOrFeature;
  if length(ExtensionOrFeatureEnum.Name)>0 then begin
   ExtensionOrFeatureEnumValue:=EnumValues.Values[ExtensionOrFeatureEnum.Value];
   if length(ExtensionOrFeatureEnumValue)>0 then begin
    ExtensionOrFeatureEnum.Value:=ExtensionOrFeatureEnumValue;
   end;
   if length(ExtensionOrFeatureEnum.Extends)=0 then begin
    if length(ExtensionOrFeatureEnum.Alias)<>0 then begin
     ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'='+ExtensionOrFeatureEnum.Alias+';');
    end else if length(ExtensionOrFeatureEnum.Value)<>0 then begin
     if pos('"',ExtensionOrFeatureEnum.Value)=0 then begin
      ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'='+ExtensionOrFeatureEnum.Value+';');
     end else begin
      ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'='+StringReplace(ExtensionOrFeatureEnum.Value,'"','''',[rfReplaceAll])+';');
     end;
    end else if ExtensionOrFeatureEnum.Offset>=0 then begin
     if ExtensionOrFeatureEnum.ExtNumber>0 then begin
      ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'='+ExtensionOrFeatureEnum.Dir+IntToStr(1000000000+((ExtensionOrFeatureEnum.ExtNumber-1)*1000)+ExtensionOrFeatureEnum.Offset)+';');
     end else begin
      ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'='+ExtensionOrFeatureEnum.Dir+IntToStr(1000000000+((ExtensionOrFeature.Number-1)*1000)+ExtensionOrFeatureEnum.Offset)+';');
     end; 
    end else if ExtensionOrFeatureEnum.BitPos>=0 then begin
     if ExtensionOrFeatureEnum.BitPos=31 then begin
      ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'=TXrInt32('+ExtensionOrFeatureEnum.Dir+'$'+IntToHex(longword(1) shl ExtensionOrFeatureEnum.BitPos,8)+');');
     end else begin
      ENumConstants.Add('      '+ExtensionOrFeatureEnum.Name+'='+ExtensionOrFeatureEnum.Dir+'$'+IntToHex(longword(1) shl ExtensionOrFeatureEnum.BitPos,8)+';');
     end;
    end;
   end;
  end;
 end;
end;

procedure ParseVendorIDsTag(Tag:TXMLTag);
var i:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
    VendorID:TVendorID;
begin
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='vendorid' then begin
    VendorID:=TVendorID.Create;
    VendorIDList.Add(VendorID);
    VendorID.Name:=ChildTag.GetParameter('name','');
    VendorID.ID:=StrToIntDef(StringReplace(ChildTag.GetParameter('id','0'),'0x','$',[rfReplaceAll]),0);
    VendorID.Comment:=ChildTag.GetParameter('comment','');
   end;
  end;
 end;
 writeln(VendorIDList.Count,' vendor IDs found ! ');
end;

procedure ParseTagsTag(Tag:TXMLTag);
var i:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
    ATag:TTag;
begin
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='tag' then begin
    ATag:=TTag.Create;
    TagList.Add(ATag);
    ATag.Name:=ChildTag.GetParameter('name','');
    ATag.Author:=ChildTag.GetParameter('author','');
    ATag.Contact:=ChildTag.GetParameter('contact','');
   end;
  end;
 end;
 writeln(TagList.Count,' tags found ! ');
end;

procedure ParseTypesTag(Tag:TXMLTag);
type PTypeDefinitionKind=^TTypeDefinitionKind;
     TTypeDefinitionKind=(tdkUNKNOWN,tdkSTRUCT,tdkUNION,tdkFUNCPOINTER,tdkALIAS);
     PTypeDefinitionMember=^TTypeDefinitionMember;
     TTypeDefinitionMember=record
      Name:ansistring;
      Type_:ansistring;
      Values:ansistring;
      ArraySizeInt:longint;
      ArraySizeStr:ansistring;
      Comment:ansistring;
      TypeDefinitionIndex:longint;
      Ptr:longint;
      Constant:longbool;
     end;
     TTypeDefinitionMembers=array of TTypeDefinitionMember;
     PTypeDefinition=^TTypeDefinition;
     TTypeDefinition=record
      Kind:TTypeDefinitionKind;
      Name:ansistring;
      Comment:ansistring;
      Members:TTypeDefinitionMembers;
      CountMembers:longint;
      Define:ansistring;
      Type_:ansistring;
      Alias:ansistring;
      Ptr:longint;
      ValidityStringList:TStringList;
     end;
     TTypeDefinitions=array of TTypeDefinition;
     TPTypeDefinitions=array of PTypeDefinition;
var i,j,k,ArraySize,CountTypeDefinitions,VersionMajor,VersionMinor,VersionPatch:longint;
    ChildItem,ChildChildItem,NextChildChildItem:TXMLItem;
    ChildTag,ChildChildTag:TXMLTag;
    Category,Type_,Name,Text,NextText,ArraySizeStr,Comment,ParameterLine,ParameterName,CodeParameterLine,
    Alias:ansistring;
    TypeDefinitions:TTypeDefinitions;
    SortedTypeDefinitions:TPTypeDefinitions;
    TypeDefinition:PTypeDefinition;
    TypeDefinitionMember:PTypeDefinitionMember;
    TypeDefinitionList:TStringList;
    ValidityStringList:TStringList;
    RecordConstructorStringList:TStringList;
    RecordConstructorCodeStringList:TStringList;
    RecordConstructorCodeBlockStringList:TStringList;
    Ptr,Constant,HasArray:boolean;
 procedure ResolveTypeDefinitionDependencies;
  function HaveDependencyOnTypeDefinition(const TypeDefinition,OtherTypeDefinition:PTypeDefinition):boolean;
  var i:longint;
  begin
   result:=TypeDefinition^.Alias=OtherTypeDefinition^.Name;
   if not result then begin
    for i:=0 to TypeDefinition^.CountMembers-1 do begin
     if TypeDefinition^.Members[i].Type_=OtherTypeDefinition^.Name then begin
      result:=true;
      break;
     end;
    end;
   end;
  end;
  function HaveCircularDependencyWithTypeDefinition(const TypeDefinition,OtherTypeDefinition:PTypeDefinition):boolean;
  var VisitedList,StackList:TList;
      Index,TypeDefinitionIndex:longint;
      CurrentTypeDefinition,RequiredTypeDefinition:PTypeDefinition;
  begin
   result:=false;
   if assigned(OtherTypeDefinition) then begin
    VisitedList:=TList.Create;
    try
     StackList:=TList.Create;
     try
      StackList.Add(OtherTypeDefinition);
      while (StackList.Count>0) and not result do begin
       CurrentTypeDefinition:=StackList.Items[StackList.Count-1];
       StackList.Delete(StackList.Count-1);
       VisitedList.Add(CurrentTypeDefinition);
       for Index:=0 to CurrentTypeDefinition.CountMembers-1 do begin
        RequiredTypeDefinition:=nil;
        if CurrentTypeDefinition^.Members[Index].Ptr=0 then begin
         TypeDefinitionIndex:=TypeDefinitionList.IndexOf(CurrentTypeDefinition^.Members[Index].Type_);
         if TypeDefinitionIndex>=0 then begin
          RequiredTypeDefinition:=pointer(TypeDefinitionList.Objects[TypeDefinitionIndex]);
         end;
        end;
        if assigned(RequiredTypeDefinition) then begin
         if RequiredTypeDefinition=TypeDefinition then begin
          result:=true;
          break;
         end else if VisitedList.IndexOf(RequiredTypeDefinition)<0 then begin
          StackList.Add(RequiredTypeDefinition);
         end;
        end;
       end;
      end;
     finally
      StackList.Free;
     end;
    finally
     VisitedList.Free;
    end;
   end;
  end;
 var Index,OtherIndex,TypeDefinitionIndex:longint;
     Done,Stop:boolean;
     TypeDefinition,OtherTypeDefinition:PTypeDefinition;
 begin
  // Resolve dependencies with "stable" topological sorting a la naive bubble sort with a bad
  // execution time (but that fact does not matter at so few SortedTypeDefinitions), and not with Kahn's or
  // Tarjan's algorithms, because the result must be in a stable sort order
  repeat
   Done:=true;
   for Index:=0 to CountTypeDefinitions-1 do begin
    TypeDefinition:=SortedTypeDefinitions[Index];
    for OtherIndex:=0 to Index-1 do begin
     OtherTypeDefinition:=SortedTypeDefinitions[OtherIndex];
     if HaveDependencyOnTypeDefinition(OtherTypeDefinition,TypeDefinition) then begin
      if HaveCircularDependencyWithTypeDefinition(OtherTypeDefinition,TypeDefinition) then begin
       raise Exception.Create(TypeDefinition^.Name+' have circular dependency with '+OtherTypeDefinition^.Name);
      end else begin
       SortedTypeDefinitions[OtherIndex]:=TypeDefinition;
       SortedTypeDefinitions[Index]:=OtherTypeDefinition;
       Done:=false;
       break;
      end;
     end;
    end;
    if not Done then begin
     break;
    end;
   end;
  until Done;
 end;
 function MemberComment(s:ansistring):ansistring;
 begin
  s:=StringReplace(s,#13#10,' ',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,#13,' ',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,#10,' ',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,#9,' ',[rfReplaceAll,rfIgnoreCase]);
  while pos('  ',s)>0 do begin
   s:=StringReplace(s,'  ',' ',[rfReplaceAll,rfIgnoreCase]);
  end;
  s:=WrapString(s,sLineBreak,512);
  s:=trim(s);
  if length(s)>0 then begin
   if (pos(#13,s)>0) or (pos(#10,s)>0) then begin
    s:=StringReplace(s,'{','(',[rfReplaceAll,rfIgnoreCase]);
    s:=StringReplace(s,'}',')',[rfReplaceAll,rfIgnoreCase]);
    result:=' {< '+s+' }';
   end else begin
    result:=' //< '+s;
   end;
  end else begin
   result:='';
  end;
 end;
 function ParamMemberComment(s:ansistring):ansistring;
 begin
  s:=StringReplace(s,#13#10,' ',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,#13,' ',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,#10,' ',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,#9,' ',[rfReplaceAll,rfIgnoreCase]);
  while pos('  ',s)>0 do begin
   s:=StringReplace(s,'  ',' ',[rfReplaceAll,rfIgnoreCase]);
  end;
  s:=WrapString(s,sLineBreak,512);
  s:=trim(s);
  if length(s)>0 then begin
   result:='{< '+s+' }';
  end else begin
   result:='';
  end;
 end;
begin
 TypeDefinitions:=nil;
 CountTypeDefinitions:=0;
 SortedTypeDefinitions:=nil;
 TypeDefinitionList:=TStringList.Create;
 try
  SetLength(TypeDefinitions,Tag.Items.Count);
  for i:=0 to Tag.Items.Count-1 do begin
   ChildItem:=Tag.Items[i];
   if ChildItem is TXMLTag then begin
    ChildTag:=TXMLTag(ChildItem);
    if ChildTag.Name='type' then begin
     Alias:=ChildTag.GetParameter('alias');
     if length(Alias)>0 then begin
      Name:=ChildTag.GetParameter('name');
      Category:=ChildTag.GetParameter('category');
      if Category='basetype' then begin
       BaseTypes.Add('     PP'+Name+'=PP'+Alias+';');
       BaseTypes.Add('     P'+Name+'=P'+Alias+';');
       BaseTypes.Add('     T'+Name+'=T'+Alias+';');
       BaseTypes.Add('');
      end else if Category='bitmask' then begin
       BitMaskTypes.Add('     PP'+Name+'=PP'+Alias+';');
       BitMaskTypes.Add('     P'+Name+'=P'+Alias+';');
       BitMaskTypes.Add('     T'+Name+'=T'+Alias+';');
       BitMaskTypes.Add('');
      end else if Category='handle' then begin
       HandleTypes.Add('     PP'+Name+'=PP'+Alias+';');
       HandleTypes.Add('     P'+Name+'=P'+Alias+';');
       HandleTypes.Add('     T'+Name+'=T'+Alias+';');
       HandleTypes.Add('');
      end else if Category='enum' then begin
       AliasEnumTypes.Add('     PP'+Name+'=PP'+Alias+';');
       AliasEnumTypes.Add('     P'+Name+'=P'+Alias+';');
       AliasEnumTypes.Add('     T'+Name+'=T'+Alias+';');
       AliasEnumTypes.Add('');
      end else if (Category='struct') or (Category='union') then begin
       TypeDefinition:=@TypeDefinitions[CountTypeDefinitions];
       inc(CountTypeDefinitions);
       TypeDefinition^.Kind:=tdkALIAS;
       TypeDefinition^.Name:=Name;
       TypeDefinition^.Comment:=ChildTag.GetParameter('comment');
       TypeDefinition^.Members:=nil;
       TypeDefinition^.Define:='';
       TypeDefinition^.Type_:='';
       TypeDefinition^.Alias:=Alias;
       TypeDefinition^.Ptr:=0;
      end else begin
       Assert(false,Category);
      end;
     end else begin
      ValidityStringList:=TStringList.Create;
      try
       ParseValidityTag(ChildTag.FindTag('validity'),ValidityStringList);
       Category:=ChildTag.GetParameter('category');
       if Category='include' then begin
       end else if Category='define' then begin
        Name:=ParseText(ChildTag.FindTag('name'),['']);
        if (pos('XR_API_VERSION',Name)=1) or (pos('XR_HEADER_VERSION_COMPLETE',Name)=1) then begin
         VersionMajor:=1;
         VersionMinor:=0;
         VersionPatch:=0;
         Text:=ParseText(ChildTag,['']);
         if length(Text)>0 then begin
          j:=pos('(',Text);
          if j>0 then begin
           delete(Text,1,j);
           j:=pos(')',Text);
           if j>0 then begin
            Text:=copy(Text,1,j-1);
            j:=pos(',',Text);
            if j>0 then begin
             VersionMajor:=StrToIntDef(trim(copy(Text,1,j-1)),0);
             delete(Text,1,j);
             Text:=trim(Text);
             j:=pos(',',Text);
             if j>0 then begin
              VersionMinor:=StrToIntDef(trim(copy(Text,1,j-1)),0);
              delete(Text,1,j);
              Text:=trim(Text);
              VersionPatch:=StrToIntDef(Text,0);
             end;
            end;
           end;
          end;
         end;
         if Name='XR_HEADER_VERSION_COMPLETE' then begin
          VersionConstants.Add('      '+Name+'=('+IntToStr(VersionMajor)+' shl 22) or ('+IntToStr(VersionMinor)+' shl 12) or (XR_HEADER_VERSION shl 0);');
         end else begin
          VersionConstants.Add('      '+Name+'=('+IntToStr(VersionMajor)+' shl 22) or ('+IntToStr(VersionMinor)+' shl 12) or ('+IntToStr(VersionPatch)+' shl 0);');
         end;
         VersionConstants.Add('');
        end else if pos('XR_HEADER_VERSION',Name)=1 then begin
         Text:=ParseText(ChildTag,['']);
         j:=pos(Name,Text);
         if j>0 then begin
          Delete(Text,1,(j+length(Name))-1);
          Text:=trim(Text);
          VersionConstants.Add('      '+Name+'='+Text+';');
          VersionConstants.Add('');
         end;
        end;
       end else if Category='basetype' then begin
        Type_:=ParseText(ChildTag.FindTag('type'),['']);
        Name:=ParseText(ChildTag.FindTag('name'),['']);
        BaseTypes.Add('     PP'+Name+'=^P'+Name+';');
        BaseTypes.Add('     P'+Name+'=^T'+Name+';');
        BaseTypes.Add('     T'+Name+'='+TranslateType(Type_,0)+';');
        BaseTypes.Add('');
       end else if Category='bitmask' then begin
        Type_:=ParseText(ChildTag.FindTag('type'),['']);
        Name:=ParseText(ChildTag.FindTag('name'),['']);
        BitMaskTypes.Add('     PP'+Name+'=^P'+Name+';');
        BitMaskTypes.Add('     P'+Name+'=^T'+Name+';');
        BitMaskTypes.Add('     T'+Name+'='+TranslateType(Type_,0)+';');
        BitMaskTypes.Add('');
       end else if Category='handle' then begin
        Type_:=ParseText(ChildTag.FindTag('type'),['']);
        Name:=ParseText(ChildTag.FindTag('name'),['']);
        HandleTypes.Add('     PP'+Name+'=^P'+Name+';');
        HandleTypes.Add('     P'+Name+'=^T'+Name+';');
        HandleTypes.Add('     T'+Name+'='+TranslateType(Type_,0)+';');
        HandleTypes.Add('');
       end else if Category='enum' then begin
        Name:=ChildTag.GetParameter('name');
       end else if Category='funcpointer' then begin
        TypeDefinition:=@TypeDefinitions[CountTypeDefinitions];
        inc(CountTypeDefinitions);
        TypeDefinition^.Kind:=tdkFUNCPOINTER;
        TypeDefinition^.Name:='';
        TypeDefinition^.Comment:=ChildTag.GetParameter('comment');
        TypeDefinition^.Members:=nil;
        TypeDefinition^.Define:='';
        TypeDefinition^.Type_:='';
        TypeDefinition^.Alias:='';
        TypeDefinition^.Ptr:=0;
        Name:='';
        Type_:='';
        Text:='';
        Ptr:=false;
        Constant:=false;
        for j:=0 to ChildTag.Items.Count-1 do begin
         ChildChildItem:=ChildTag.Items[j];
         if ChildChildItem is TXMLTag then begin
          ChildChildTag:=TXMLTag(ChildChildItem);
          if ChildChildTag.Name='name' then begin
           TypeDefinition^.Name:=ParseText(ChildChildTag,['']);
           if pos('void*',Text)>0 then begin
            TypeDefinition^.Ptr:=1;
            TypeDefinition^.Type_:='void';
           end else begin
            Text:=trim(StringReplace(Text,'typedef','',[rfReplaceAll]));
            Text:=trim(StringReplace(Text,'XRAPI_PTR','',[rfReplaceAll]));
            Text:=trim(StringReplace(Text,'(','',[rfReplaceAll]));
            Text:=trim(StringReplace(Text,'*','',[rfReplaceAll]));
            TypeDefinition^.Type_:=Text;
           end;
          end else if ChildChildTag.Name='type' then begin
           Type_:=ParseText(ChildChildTag,['']);
          end;
         end else if ChildChildItem is TXMLText then begin
          Text:=TXMLText(ChildChildItem).Text;
          if length(TypeDefinition^.Name)>0 then begin
           while length(Text)>0 do begin
            NextText:='';
            if trim(Text)='const' then begin
             Constant:=true;
            end else if length(Type_)>0 then begin
             if length(TypeDefinition^.Members)<TypeDefinition^.CountMembers+1 then begin
              SetLength(TypeDefinition^.Members,(TypeDefinition^.CountMembers+1)*2);
             end;
             TypeDefinitionMember:=@TypeDefinition^.Members[TypeDefinition^.CountMembers];
             inc(TypeDefinition^.CountMembers);
             TypeDefinitionMember^.Type_:=Type_;
             k:=pos(',',Text);
             if k>0 then begin
              NextText:=trim(copy(Text,k+1,(length(Text)-k)+1));
              Text:=trim(copy(Text,1,k-1));
             end;
             Text:=trim(StringReplace(Text,');','',[rfReplaceAll]));
             if pos('*',Text)>0 then begin
              for k:=1 to length(Text) do begin
               if Text[k]='*' then begin
                inc(TypeDefinitionMember^.Ptr);
               end;
              end;
              Text:=trim(StringReplace(Text,'*','',[rfReplaceAll]));
             end;
             TypeDefinitionMember^.Constant:=Constant;
             Constant:=false;
             if Text='object' then begin
              Text:='object_';
             end else if Text='set' then begin
              Text:='set_';
             end else if Text='unit' then begin
              Text:='unit_';
             end;
             TypeDefinitionMember^.Name:=Text;
             TypeDefinitionMember^.ArraySizeInt:=0;
             TypeDefinitionMember^.ArraySizeStr:='';
             TypeDefinitionMember^.Comment:='';
             Type_:='';
            end;
            Text:=NextText;
           end;
          end;
         end;
        end;
        SetLength(TypeDefinition^.Members,TypeDefinition^.CountMembers);
       end else if (Category='struct') or (Category='union') then begin
        Name:=ChildTag.GetParameter('name');
        TypeDefinition:=@TypeDefinitions[CountTypeDefinitions];
        inc(CountTypeDefinitions);
        if Category='union' then begin
         TypeDefinition^.Kind:=tdkUNION;
        end else begin
         TypeDefinition^.Kind:=tdkSTRUCT;
        end;
        TypeDefinition^.Name:=Name;
        TypeDefinition^.Comment:=ChildTag.GetParameter('comment');
        TypeDefinition^.Members:=nil;
        TypeDefinition^.Define:='';
        if (pos('IOS',UpperCase(Name))>0) and (pos('MXR',Name)>0) then begin
         TypeDefinition^.Define:='MoltenXR_IOS';
        end else if (pos('MACOS',UpperCase(Name))>0) and (pos('MXR',Name)>0) then begin
         TypeDefinition^.Define:='MoltenXR_MacOS';
        end else if (pos('MXR',Name)>0) or (pos('MOLTENXR',UpperCase(Name))>0) then begin
         TypeDefinition^.Define:='MoltenXR';
        end else if (pos('ANDROID',Name)>0) or (pos('Android',Name)>0) then begin
         TypeDefinition^.Define:='Android';
        end else if (pos('GraphicsBindingEGL',Name)>0) or (pos('OpenGLESFB',Name)>0)  then begin
         TypeDefinition^.Define:='EGL';
        end;
        SetLength(TypeDefinition^.Members,ChildTag.Items.Count);
        TypeDefinition^.CountMembers:=0;
        for j:=0 to ChildTag.Items.Count-1 do begin
         ChildChildItem:=ChildTag.Items[j];
         if ChildChildItem is TXMLTag then begin
          ChildChildTag:=TXMLTag(ChildChildItem);
          if ChildChildTag.Name='member' then begin
           Comment:='';
           if (j+1)<ChildTag.Items.Count then begin
            k:=j+1;
            while k<ChildTag.Items.Count do begin
             NextChildChildItem:=ChildTag.Items[k];
             if NextChildChildItem is TXMLCommentTag then begin
              Comment:=trim(TXMLCommentTag(NextChildChildItem).Text);
              break;
             end else if NextChildChildItem is TXMLText then begin
              inc(k);
             end else begin
              break;
             end;
            end;
           end;
           Name:=ParseText(ChildChildTag.FindTag('name'),['']);
           ArraySizeStr:=ParseText(ChildChildTag.FindTag('enum'),['']);
           k:=pos('[',Name);
           ArraySize:=-1;
           if k>0 then begin
            Text:=copy(Name,k+1,length(Name)-k);
            Name:=copy(Name,1,k-1);
            k:=pos(']',Text);
            if k>0 then begin
             ArraySize:=StrToIntDef(copy(Text,1,k-1),1);
            end;
           end;
           if ArraySize<0 then begin
            Text:=ParseText(ChildChildTag,['comment']);
            k:=pos('[',Text);
            if k>0 then begin
             Delete(Text,1,k);
             k:=pos(']',Text);
             if k>0 then begin
              Text:=trim(copy(Text,1,k-1));
              ArraySize:=StrToIntDef(Text,-1);
              if ArraySize<0 then begin
               ArraySizeStr:=Text;
              end;
             end;
            end;
           end;
           if Name='type' then begin
            Name:='type_';
           end else if Name='hinstance' then begin
            Name:='hinstance_';
           end else if Name='hwnd' then begin
            Name:='hwnd_';
           end else if Name='hmonitor' then begin
            Name:='hmonitor_';
           end else if Name='object' then begin
            Name:='object_';
           end else if Name='set' then begin
            Name:='set_';
           end else if Name='unit' then begin
            Name:='unit_';
           end;
           TypeDefinitionMember:=@TypeDefinition^.Members[TypeDefinition^.CountMembers];
           inc(TypeDefinition^.CountMembers);
           TypeDefinitionMember^.Name:=Name;
           TypeDefinitionMember^.ArraySizeInt:=ArraySize;
           Type_:=ParseText(ChildChildTag.FindTag('type'),['']);
           TypeDefinitionMember^.Type_:=Type_;
           TypeDefinitionMember^.Values:=ChildChildTag.GetParameter('values');
           TypeDefinitionMember^.ArraySizeStr:=ArraySizeStr;
           TypeDefinitionMember^.Comment:=trim(ChildChildTag.GetParameter('comment')+' '+Comment);
 {         k:=0;
           while k<ValidityStringList.Count do begin
            if pos('pname:'+Name,ValidityStringList[k])>0 then begin
             TypeDefinitionMember^.Comment:=trim(TypeDefinitionMember^.Comment+' '+StringReplace(ValidityStringList[k],'pname:','',[rfReplaceAll]));
            end;
            inc(k);
           end;{}
           TypeDefinitionMember^.TypeDefinitionIndex:=-1;
           TypeDefinitionMember^.Ptr:=0;
           Text:=ParseText(ChildChildTag,['']);
           for k:=1 to length(Text) do begin
            if Text[k]='*' then begin
             inc(TypeDefinitionMember^.Ptr);
            end;
           end;
           if (Type_='HWND') or (Type_='HMONITOR') or (Type_='HINSTANCE') or (Type_='SECURITY_ATTRIBUTES') then begin
            TypeDefinition^.Define:='Windows';
           end else if Type_='RROutput' then begin
            TypeDefinition^.Define:='RandR';
           end else if (Type_='Display') or (Type_='VisualID') or (Type_='Window') then begin
            TypeDefinition^.Define:='XLIB';
           end else if (Type_='xcb_connection_t') or (Type_='xcb_visualid_t') or (Type_='xcb_window_t') then begin
            TypeDefinition^.Define:='XCB';
           end else if (Type_='wl_display') or (Type_='wl_surface') then begin
            TypeDefinition^.Define:='Wayland';
           end else if (Type_='ANativeWindow') or (Type_='AHardwareBuffer') then begin
            TypeDefinition^.Define:='Android';
           end else if (Type_='zx_handle_t') or (pos('FUCHSIA',UpperCase(Type_))>0) then begin
            TypeDefinition^.Define:='Fuchsia';
           end else if (Type_='IDirectFB') or (Type_='IDirectFBSurface') or (pos('DIRECTFB',UpperCase(Type_))>0) then begin
            TypeDefinition^.Define:='DirectFB';
           end;
          end;
         end;
        end;
        SetLength(TypeDefinition^.Members,TypeDefinition^.CountMembers);
        TypeDefinition^.ValidityStringList:=ValidityStringList;
        ValidityStringList:=nil;
       end;
      finally
       FreeAndNil(ValidityStringList);
      end;
     end;
    end;
   end;
  end;
  SetLength(TypeDefinitions,CountTypeDefinitions);
  SetLength(SortedTypeDefinitions,CountTypeDefinitions);
  for i:=0 to CountTypeDefinitions-1 do begin
   SortedTypeDefinitions[i]:=@TypeDefinitions[i];
   TypeDefinitionList.AddObject(TypeDefinitions[i].Name,pointer(SortedTypeDefinitions[i]));
  end;
  ResolveTypeDefinitionDependencies;
  for i:=0 to CountTypeDefinitions-1 do begin
   TypeDefinition:=SortedTypeDefinitions[i];
   if length(TypeDefinition^.Define)>0 then begin
    TypeDefinitionTypes.Add('{$ifdef '+TypeDefinition^.Define+'}');
   end;
   if assigned(TypeDefinition^.ValidityStringList) and (TypeDefinition^.ValidityStringList.Count>0) then begin
    TypeDefinition^.ValidityStringList.Text:=StringReplace(TypeDefinition^.ValidityStringList.Text,'pname:','',[rfReplaceAll]);
    TypeDefinition^.ValidityStringList.Text:=StringReplace(TypeDefinition^.ValidityStringList.Text,'sname:','T',[rfReplaceAll]);
    TypeDefinition^.ValidityStringList.Text:=StringReplace(TypeDefinition^.ValidityStringList.Text,'ename:','T',[rfReplaceAll]);
    TypeDefinition^.ValidityStringList.Text:=StringReplace(TypeDefinition^.ValidityStringList.Text,'fname:','',[rfReplaceAll]);
    TypeDefinition^.ValidityStringList.Text:=WrapString(TypeDefinition^.ValidityStringList.Text,sLineBreak,512);
    for j:=0 to TypeDefinition^.ValidityStringList.Count-1 do begin
     TypeDefinitionTypes.Add('     // '+TypeDefinition^.ValidityStringList.Strings[j]);
    end;
   end;
   TypeDefinitionTypes.Add('     PP'+TypeDefinition^.Name+'=^P'+TypeDefinition^.Name+';');
   TypeDefinitionTypes.Add('     P'+TypeDefinition^.Name+'=^T'+TypeDefinition^.Name+';');
   case TypeDefinition^.Kind of
    tdkSTRUCT:begin
     TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=record');
     TypeDefinitionTypes.Add('{$ifdef HAS_ADVANCED_RECORDS}');
     TypeDefinitionTypes.Add('      public');
     TypeDefinitionTypes.Add('{$endif}');
     RecordConstructorStringList:=TStringList.Create;
     RecordConstructorCodeStringList:=TStringList.Create;
     RecordConstructorCodeBlockStringList:=TStringList.Create;
     HasArray:=false;
     try
      for j:=0 to TypeDefinition^.CountMembers-1 do begin
       Assert(length(TypeDefinition^.Members[j].Name)>0);
       if (TypeDefinition^.Members[j].Type_='XrStructureType') and
          (length(TypeDefinition^.Members[j].Comment)=0) and
          (length(TypeDefinition^.Members[j].Values)>0) and
          (copy(TypeDefinition^.Members[j].Values,1,length('XR_STRUCTURE_TYPE_'))='XR_STRUCTURE_TYPE_') then begin
        TypeDefinition^.Members[j].Comment:='Must be '+TypeDefinition^.Members[j].Values;
       end;
       ParameterName:='a'+UpCase(TypeDefinition^.Members[j].Name[1])+copy(TypeDefinition^.Members[j].Name,2,length(TypeDefinition^.Members[j].Name)-1);
       ParameterLine:=ParameterName+':';
       if length(TypeDefinition^.Members[j].ArraySizeStr)>0 then begin
        TypeDefinitionTypes.Add('       '+TypeDefinition^.Members[j].Name+':array[0..'+TypeDefinition^.Members[j].ArraySizeStr+'-1] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';'+MemberComment(TypeDefinition^.Members[j].Comment));
        if TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)='TXrChar' then begin
         ParameterLine:=ParameterLine+'TXrCharString';
        end else begin
         ParameterLine:=ParameterLine+'array of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr);
        end;
       end else if TypeDefinition^.Members[j].ArraySizeInt>=0 then begin
        TypeDefinitionTypes.Add('       '+TypeDefinition^.Members[j].Name+':array[0..'+IntToStr(TypeDefinition^.Members[j].ArraySizeInt-1)+'] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';'+MemberComment(TypeDefinition^.Members[j].Comment));
        if TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)='TXrChar' then begin
         ParameterLine:=ParameterLine+'TXrCharString';
        end else begin
         ParameterLine:=ParameterLine+'array of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr);
        end;
       end else begin
        TypeDefinitionTypes.Add('       '+TypeDefinition^.Members[j].Name+':'+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';'+MemberComment(TypeDefinition^.Members[j].Comment));
        if (TypeDefinition^.Members[j].Type_<>'XrStructureType') and (TypeDefinition^.Members[j].Name<>'next') then begin
         ParameterLine:=ParameterLine+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr);
        end;
       end;
       if (length(TypeDefinition^.Members[j].ArraySizeStr)>0) or (TypeDefinition^.Members[j].ArraySizeInt>=0) then begin
        HasArray:=true;
        if TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)='TXrChar' then begin
         RecordConstructorCodeBlockStringList.Add(' ArrayItemCount:=length('+ParameterName+');');
         RecordConstructorCodeBlockStringList.Add(' if ArrayItemCount>length('+TypeDefinition^.Members[j].Name+') then begin');
         RecordConstructorCodeBlockStringList.Add('  ArrayItemCount:=length('+TypeDefinition^.Members[j].Name+');');
         RecordConstructorCodeBlockStringList.Add(' end;');
         RecordConstructorCodeBlockStringList.Add(' if ArrayItemCount>0 then begin');
         RecordConstructorCodeBlockStringList.Add('  Move('+ParameterName+'[1],'+TypeDefinition^.Members[j].Name+'[0],ArrayItemCount*SizeOf('+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+'));');
         RecordConstructorCodeBlockStringList.Add(' end;');
        end else begin
         RecordConstructorCodeBlockStringList.Add(' ArrayItemCount:=length('+ParameterName+');');
         RecordConstructorCodeBlockStringList.Add(' if ArrayItemCount>length('+TypeDefinition^.Members[j].Name+') then begin');
         RecordConstructorCodeBlockStringList.Add('  ArrayItemCount:=length('+TypeDefinition^.Members[j].Name+');');
         RecordConstructorCodeBlockStringList.Add(' end;');
         RecordConstructorCodeBlockStringList.Add(' if ArrayItemCount>0 then begin');
         RecordConstructorCodeBlockStringList.Add('  Move('+ParameterName+'[0],'+TypeDefinition^.Members[j].Name+'[0],ArrayItemCount*SizeOf('+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+'));');
         RecordConstructorCodeBlockStringList.Add(' end;');
        end;
       end else begin
        if (TypeDefinition^.Members[j].Type_<>'XrStructureType') and (TypeDefinition^.Members[j].Name<>'next') then begin
         RecordConstructorCodeBlockStringList.Add(' '+TypeDefinition^.Members[j].Name+':='+ParameterName+';');
        end else if TypeDefinition^.Members[j].Type_='XrStructureType' then begin
         ParameterName:=TypeDefinition^.Members[j].Comment;
         k:=pos('XR_STRUCTURE_TYPE_',ParameterName);
         if k>0 then begin
          Delete(ParameterName,1,k-1);
          for k:=1 to length(ParameterName) do begin
           if not (ParameterName[k] in ['A'..'Z','a'..'z','_','0'..'9']) then begin
            ParameterName:=copy(ParameterName,1,k-1);
            break;
           end;
          end;
          RecordConstructorCodeBlockStringList.Add(' '+TypeDefinition^.Members[j].Name+':='+ParameterName+';');
         end else if (length(TypeDefinition^.Members[j].Values)>0) and
                     (copy(TypeDefinition^.Members[j].Values,1,length('XR_STRUCTURE_TYPE_'))='XR_STRUCTURE_TYPE_') then begin
          RecordConstructorCodeBlockStringList.Add(' '+TypeDefinition^.Members[j].Name+':='+TypeDefinition^.Members[j].Values+';');
         end else if TypeDefinition^.Members[j].Name='type_' then begin
          RecordConstructorCodeBlockStringList.Add(' '+TypeDefinition^.Members[j].Name+':=TXrStructureType(TXrInt32(0));');
         end else begin
          Assert(false);
         end;
        end else if TypeDefinition^.Members[j].Name='next' then begin
         RecordConstructorCodeBlockStringList.Add(' '+TypeDefinition^.Members[j].Name+':=nil;');
        end;
       end;
       if (TypeDefinition^.Members[j].Type_<>'XrStructureType') and (TypeDefinition^.Members[j].Name<>'next') then begin
        if RecordConstructorStringList.Count=0 then begin
         CodeParameterLine:='constructor T'+TypeDefinition^.Name+'.Create(const '+ParameterLine;
         ParameterLine:='       constructor Create(const '+ParameterLine;
        end else begin
         CodeParameterLine:=AlignPaddingString('',21+length(TypeDefinition^.Name))+'const '+ParameterLine;
         ParameterLine:='                          const '+ParameterLine;
        end;
        if (j+1)<TypeDefinition^.CountMembers then begin
         CodeParameterLine:=CodeParameterLine+';';
         ParameterLine:=ParameterLine+';'+MemberComment(TypeDefinition^.Members[j].Comment);
        end else begin
         CodeParameterLine:=CodeParameterLine+');';
         ParameterLine:=ParameterLine+');'+MemberComment(TypeDefinition^.Members[j].Comment);
        end;
        if (TypeDefinition^.Name<>'XrBaseInStructure') and
           (TypeDefinition^.Name<>'XrBaseOutStructure') and
           (TypeDefinition^.Name<>'XrSwapchainImageBaseHeader') and
           (TypeDefinition^.Name<>'XrSwapchainImageAcquireInfo') and
           (TypeDefinition^.Name<>'XrSwapchainImageReleaseInfo') and
           (TypeDefinition^.Name<>'XrFrameBeginInfo') and
           (TypeDefinition^.Name<>'XrFrameWaitInfo') and
           (TypeDefinition^.Name<>'XrHapticBaseHeader') and
           (TypeDefinition^.Name<>'XrEventDataBaseHeader') and
           (TypeDefinition^.Name<>'XrBindingModificationBaseHeaderKHR') and
           (TypeDefinition^.Name<>'XrSwapchainStateBaseHeaderFB') and
           (TypeDefinition^.Name<>'XrLoaderInitInfoBaseHeaderKHR') and
           (TypeDefinition^.Name<>'XrSceneObserverCreateInfoMSFT') and
           (TypeDefinition^.Name<>'XrSceneCreateInfoMSFT') and
           (TypeDefinition^.Name<>'XrSceneMeshBuffersMSFT') then begin
         RecordConstructorCodeStringList.Add(CodeParameterLine);
         RecordConstructorStringList.Add(ParameterLine);
        end;
       end;
      end;
      if (TypeDefinition^.Name<>'XrBaseInStructure') and
         (TypeDefinition^.Name<>'XrBaseOutStructure') and
         (TypeDefinition^.Name<>'XrSwapchainImageBaseHeader') and
         (TypeDefinition^.Name<>'XrSwapchainImageAcquireInfo') and
         (TypeDefinition^.Name<>'XrSwapchainImageReleaseInfo') and
         (TypeDefinition^.Name<>'XrFrameBeginInfo') and
         (TypeDefinition^.Name<>'XrFrameWaitInfo') and
         (TypeDefinition^.Name<>'XrHapticBaseHeader') and
         (TypeDefinition^.Name<>'XrEventDataBaseHeader') and
         (TypeDefinition^.Name<>'XrBindingModificationBaseHeaderKHR') and
         (TypeDefinition^.Name<>'XrSwapchainStateBaseHeaderFB') and
         (TypeDefinition^.Name<>'XrLoaderInitInfoBaseHeaderKHR') and
         (TypeDefinition^.Name<>'XrSceneObserverCreateInfoMSFT') and
         (TypeDefinition^.Name<>'XrSceneCreateInfoMSFT') and
         (TypeDefinition^.Name<>'XrSceneMeshBuffersMSFT') then begin
       if HasArray then begin
        RecordConstructorCodeStringList.Add('var ArrayItemCount:TXrInt32;');
       end;
       RecordConstructorCodeStringList.Add('begin');
       if HasArray then begin
        RecordConstructorCodeStringList.Add(' FillChar(self,SizeOf(T'+TypeDefinition^.Name+'),#0);');
       end;
       RecordConstructorCodeStringList.AddStrings(RecordConstructorCodeBlockStringList);
       RecordConstructorCodeStringList.Add('end;');
       if TypeDefinitionConstructors.Count>0 then begin
        TypeDefinitionConstructors.Add('');
       end;
       if length(TypeDefinition^.Define)>0 then begin
        TypeDefinitionConstructors.Add('{$ifdef '+TypeDefinition^.Define+'}');
       end;
       TypeDefinitionConstructors.AddStrings(RecordConstructorCodeStringList);
       if length(TypeDefinition^.Define)>0 then begin
        TypeDefinitionConstructors.Add('{$endif}');
       end;
      end;
      TypeDefinitionTypes.Add('{$ifdef HAS_ADVANCED_RECORDS}');
      if RecordConstructorStringList.Count>0 then begin
       TypeDefinitionTypes.AddStrings(RecordConstructorStringList);
      end else begin
//     TypeDefinitionTypes.Add('       function Create:T'+TypeDefinition^.Name+';');
      end;
      TypeDefinitionTypes.Add('{$endif}');
      TypeDefinitionTypes.Add('     end;');
     finally
      RecordConstructorStringList.Free;
      RecordConstructorCodeStringList.Free;
      RecordConstructorCodeBlockStringList.Free;
     end;
    end;
    tdkUNION:begin
     TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=record');
     TypeDefinitionTypes.Add('      case longint of');
     for j:=0 to TypeDefinition^.CountMembers-1 do begin
      TypeDefinitionTypes.Add('       '+IntToStr(j)+':(');
      if length(TypeDefinition^.Members[j].ArraySizeStr)>0 then begin
       TypeDefinitionTypes.Add('        '+TypeDefinition^.Members[j].Name+':array[0..'+TypeDefinition^.Members[j].ArraySizeStr+'-1] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';'+MemberComment(TypeDefinition^.Members[j].Comment));
      end else if TypeDefinition^.Members[j].ArraySizeInt>=0 then begin
       TypeDefinitionTypes.Add('        '+TypeDefinition^.Members[j].Name+':array[0..'+IntToStr(TypeDefinition^.Members[j].ArraySizeInt-1)+'] of '+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';'+MemberComment(TypeDefinition^.Members[j].Comment));
      end else begin
       TypeDefinitionTypes.Add('        '+TypeDefinition^.Members[j].Name+':'+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr)+';'+MemberComment(TypeDefinition^.Members[j].Comment));
      end;
      TypeDefinitionTypes.Add('       );');
     end;
     TypeDefinitionTypes.Add('     end;');
    end;
    tdkFUNCPOINTER:begin
     Text:='';
     for j:=0 to TypeDefinition^.CountMembers-1 do begin
      TypeDefinitionMember:=@TypeDefinition^.Members[j];
      if TypeDefinitionMember^.Constant then begin
       Text:=Text+'const ';
      end;
      Text:=Text+TypeDefinitionMember^.Name+':'+TranslateType(TypeDefinition^.Members[j].Type_,TypeDefinition^.Members[j].Ptr);
      if (j+1)<TypeDefinition^.CountMembers then begin
       Text:=Text+';';
      end;
      if length(TypeDefinition^.Members[j].Comment)>0 then begin
       Text:=Text+ParamMemberComment(TypeDefinition^.Members[j].Comment);
//     Text:=Text+#13#10;
      end;
     end;
     if (TypeDefinition^.Type_='void') and (TypeDefinition^.Ptr=0) then begin
      TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=procedure('+Text+'); '+CallingConventions);
     end else begin
      TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=function('+Text+'):'+TranslateType(TypeDefinition^.Type_,TypeDefinition^.Ptr)+'; '+CallingConventions);
     end;
    end;
    tdkALIAS:begin
     TypeDefinitionTypes.Add('     T'+TypeDefinition^.Name+'=T'+TypeDefinition^.Alias+';');
    end;
   end;
   if length(TypeDefinition^.Define)>0 then begin
    TypeDefinitionTypes.Add('{$endif}');
   end;
   TypeDefinitionTypes.Add('');
   FreeAndNil(TypeDefinition^.ValidityStringList);
  end;
 finally
  SetLength(TypeDefinitions,0);
  SetLength(SortedTypeDefinitions,0);
  TypeDefinitionList.Free;
 end;
end;

procedure ParseEnumsTag(Tag:TXMLTag);
type PValueItem=^TValueItem;
     TValueItem=record
      Name:ansistring;
      ValueStr:ansistring;
      ValueInt64:int64;
      Comment:ansistring;
      Alias:ansistring;
      IsExtended:boolean;
     end;              
     TValueItems=array of TValueItem;
var i,j,lv,hv,CountValueItems:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
    Type_,Name,NameEnum,Line,Comment,Expand,Value,LowValue,HighValue:ansistring;
    Values:TStringList;
    v:int64;
    OK:boolean;
    ValueItems:TValueItems;
    ValueItem:PValueItem;
    TempValueItem:TValueItem;
    ExtensionOrFeatureEnum:TExtensionOrFeatureEnum;
    Extension:TExtensionOrFeature;
begin
 ValueItems:=nil;
 try
  Name:=Tag.GetParameter('name','');
  Type_:=Tag.GetParameter('type','');
  Expand:=Tag.GetParameter('expand','');
  LowValue:='unknown';
  HighValue:='unknown';
  Values:=TStringList.Create;
  if Name='XrVendorId' then begin
   if length(Expand)=0 then begin
    Expand:='XR_VENDOR_ID';
   end;
  end;
  try
   j:=-1;
   SetLength(ValueItems,Tag.Items.Count);
   CountValueItems:=0;
   for i:=0 to Tag.Items.Count-1 do begin
    ChildItem:=Tag.Items[i];
    if ChildItem is TXMLTag then begin
     ChildTag:=TXMLTag(ChildItem);
     if (ChildTag.Name='enum') or (ChildTag.Name='unused') then begin
      ValueItem:=@ValueItems[CountValueItems];
      ValueItem^.IsExtended:=false;
      inc(CountValueItems);
      if ChildTag.Name='unused' then begin
       ValueItem^.Name:=Expand+'_UNUSED_START';
      end else begin
       ValueItem^.Name:=ChildTag.GetParameter('name');
      end;
      begin
       ValueItem^.Alias:=ChildTag.GetParameter('alias','');
      end;
      begin
       if length(ValueItem^.Alias)>0 then begin
        ValueItem^.ValueStr:=ValueItem^.Alias;
       end else begin
        Value:=ChildTag.GetParameter('value','');
        if Value='(~0U)' then begin
         ValueItem^.ValueStr:='TXrUInt32($ffffffff)';
        end else if Value='(~0U-1)' then begin
         ValueItem^.ValueStr:='TXrUInt32($fffffffe)';
        end else if Value='(~0U-2)' then begin
         ValueItem^.ValueStr:='TXrUInt32($fffffffd)';
        end else if Value='(~0ULL)' then begin
         ValueItem^.ValueStr:='TXrUInt64($ffffffffffffffff)';
        end else if Value='(~0ULL-1)' then begin
         ValueItem^.ValueStr:='TXrUInt64($fffffffffffffffe)';
        end else if Value='(~0ULL-2)' then begin
         ValueItem^.ValueStr:='TXrUInt64($fffffffffffffffd)';
        end else if (length(Value)>0) and ((pos('.',Value)>0) or ((pos('f',Value)=length(Value)) and (pos('x',Value)=0))) then begin
         ValueItem^.ValueStr:=StringReplace(Value,'f','',[]);
        end else if length(Value)>0 then begin
         ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('value','0'),0));
        end else begin
         if ChildTag.Name='unused' then begin
          ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('start','0'),0));
         end else if Type_='bitmask' then begin
          ValueItem^.ValueStr:='$'+IntToHex(longword(1) shl StrToIntDef(ChildTag.GetParameter('bitpos','0'),0),8);
         end else if Type_='enum' then begin
          ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('value','0'),0));
         end else begin
          ValueItem^.ValueStr:=IntToStr(StrToIntDef(ChildTag.GetParameter('value','0'),0));
         end;
        end;
       end;
      end;
      ValueItem^.ValueInt64:=StrToIntDef(ValueItem^.ValueStr,0);
      ValueItem^.Comment:=ChildTag.GetParameter('comment','');
     end;
    end;
   end;
   SetLength(ValueItems,CountValueItems);
   if length(Expand)>0 then begin
    v:=$7fffffffffffffff;
    lv:=0;
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     if ValueItem^.ValueInt64<v then begin
      v:=ValueItem^.ValueInt64;
      lv:=i;
     end;
    end;
    v:=-$7fffffffffffffff;
    hv:=0;
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     if ValueItem^.ValueInt64>v then begin
      v:=ValueItem^.ValueInt64;
      hv:=i;
     end;
    end;
    SetLength(ValueItems,CountValueItems+4);
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_BEGIN_RANGE';
     ValueItem^.ValueStr:=ValueItems[lv].ValueStr;
     ValueItem^.ValueInt64:=ValueItems[lv].ValueInt64;
     ValueItem^.Comment:=ValueItems[lv].Name;
     ValueItem^.IsExtended:=false;
    end;
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_END_RANGE';
     ValueItem^.ValueStr:=ValueItems[hv].ValueStr;
     ValueItem^.ValueInt64:=ValueItems[hv].ValueInt64;
     ValueItem^.Comment:=ValueItems[hv].Name;
     ValueItem^.IsExtended:=false;
    end;
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_RANGE_SIZE';
     ValueItem^.ValueStr:=IntToStr((ValueItems[hv].ValueInt64-ValueItems[lv].ValueInt64)+1);
     ValueItem^.ValueInt64:=(ValueItems[hv].ValueInt64-ValueItems[lv].ValueInt64)+1;
     ValueItem^.Comment:='('+ValueItems[hv].Name+'-'+ValueItems[lv].Name+')+1';
     ValueItem^.IsExtended:=false;
    end;
    begin
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.Name:=Expand+'_MAX_ENUM';
     ValueItem^.ValueStr:='$7fffffff';
     ValueItem^.ValueInt64:=$7fffffff;
     ValueItem^.Comment:='';
     ValueItem^.IsExtended:=false;
    end;
   end;
   for i:=0 to ExtensionOrFeatureEnums.Count-1 do begin
    ExtensionOrFeatureEnum:=TExtensionOrFeatureEnum(ExtensionOrFeatureEnums.Objects[i]);
    Extension:=ExtensionOrFeatureEnum.ExtensionOrFeature;
    if (length(ExtensionOrFeatureEnum.Extends)>0) and (ExtensionOrFeatureEnum.Extends=Name) then begin
     if length(ValueItems)<(CountValueItems+1) then begin
      SetLength(ValueItems,(CountValueItems+1)*2);
     end;
     ValueItem:=@ValueItems[CountValueItems];
     inc(CountValueItems);
     ValueItem^.IsExtended:=true;
     ValueItem^.Name:=ExtensionOrFeatureEnum.Name;
     if length(ExtensionOrFeatureEnum.Alias)<>0 then begin
      ValueItem^.ValueStr:=ExtensionOrFeatureEnum.Alias;
      ValueItem^.Alias:=ExtensionOrFeatureEnum.Alias;
     end else if length(ExtensionOrFeatureEnum.Value)<>0 then begin
      ValueItem^.ValueStr:=ExtensionOrFeatureEnum.Value;
     end else if ExtensionOrFeatureEnum.Offset>=0 then begin
      if ExtensionOrFeatureEnum.ExtNumber>0 then begin
       ValueItem^.ValueStr:=ExtensionOrFeatureEnum.Dir+IntToStr(1000000000+((ExtensionOrFeatureEnum.ExtNumber-1)*1000)+ExtensionOrFeatureEnum.Offset);
      end else begin
       ValueItem^.ValueStr:=ExtensionOrFeatureEnum.Dir+IntToStr(1000000000+((Extension.Number-1)*1000)+ExtensionOrFeatureEnum.Offset);
      end;
     end else if ExtensionOrFeatureEnum.BitPos>=0 then begin
      if ExtensionOrFeatureEnum.BitPos=31 then begin
       ValueItem^.ValueStr:=ExtensionOrFeatureEnum.Dir+'TXrInt32($'+IntToHex(longword(1) shl ExtensionOrFeatureEnum.BitPos,8)+')';
      end else begin
       ValueItem^.ValueStr:=ExtensionOrFeatureEnum.Dir+'$'+IntToHex(longword(1) shl ExtensionOrFeatureEnum.BitPos,8);
      end;
     end;
     ValueItem^.ValueInt64:=StrToIntDef(ValueItem^.ValueStr,0);
     ValueItem^.Comment:='';
    end;
   end;
   for i:=0 to CountValueItems-1 do begin
    for j:=0 to ExtensionOrFeatureEnums.Count-1 do begin
     ExtensionOrFeatureEnum:=TExtensionOrFeatureEnum(ExtensionOrFeatureEnums.Objects[j]);
     Extension:=ExtensionOrFeatureEnum.ExtensionOrFeature;
     if (length(ExtensionOrFeatureEnum.Extends)=0) and (ExtensionOrFeatureEnum.Value=ValueItems[i].Name) then begin
      if length(ValueItems)<(CountValueItems+1) then begin
       SetLength(ValueItems,(CountValueItems+1)*2);
      end;
      ValueItem:=@ValueItems[CountValueItems];
      inc(CountValueItems);
      ValueItem^:=ValueItems[i];
      ValueItem^.Name:=ExtensionOrFeatureEnum.Name;
      ValueItem^.IsExtended:=false;
      ExtensionOrFeatureEnum.Name:='';
      break;
     end;
    end;
   end;
   for i:=CountValueItems-1 downto 0 do begin
    if ValueItems[i].IsExtended then begin
     OK:=true;
     for j:=0 to CountValueItems-1 do begin
      if (ValueItems[j].Name=ValueItems[i].Name) and (i<>j) then begin
       OK:=false;
       break;
      end;
     end;
     if not OK then begin
      for j:=i to CountValueItems-2 do begin
       ValueItems[j]:=ValueItems[j+1];
      end;
      dec(CountValueItems);
     end;
    end;
   end;
   SetLength(ValueItems,CountValueItems);
   if (Type_='enum') or (Type_='bitmask') then begin
    ENumTypes.Add('     PP'+Name+'=^P'+Name+';');
    ENumTypes.Add('     P'+Name+'=^T'+Name+';');
    ENumTypes.Add('     T'+Name+'=');
    ENumTypes.Add('      (');
    i:=0;
    while i<(CountValueItems-1) do begin
     if (ValueItems[i].Alias>ValueItems[i+1].Alias) or
        ((ValueItems[i].Alias=ValueItems[i+1].Alias) and
         (ValueItems[i].ValueInt64>ValueItems[i+1].ValueInt64)) then begin
      TempValueItem:=ValueItems[i];
      ValueItems[i]:=ValueItems[i+1];
      ValueItems[i+1]:=TempValueItem;
      if i>0 then begin
       dec(i);
      end else begin
       inc(i);
      end;
     end else begin
      inc(i);
     end;
    end;
    begin
     // Bruteforce quick&dirty the-programmer-is-tired topological sort, don't try this at home, kids! :-)
     repeat
      OK:=true;
      for i:=0 to CountValueItems-2 do begin
       for j:=i+1 to CountValueItems-1 do begin
        if ValueItems[i].ValueStr=ValueItems[j].Name then begin
         TempValueItem:=ValueItems[i];
         ValueItems[i]:=ValueItems[i+1];
         ValueItems[i+1]:=TempValueItem;
         OK:=false;
         break;
        end;
       end;
      end;
     until OK;
    end;
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     ENumValues.Add(ValueItem^.Name+ENumValues.NameValueSeparator+ValueItem^.ValueStr);
     if length(ValueItem^.Comment)>0 then begin

      ValueItem^.Comment := DeleteRepeatedSpaces(ValueItem^.Comment);
      // CRLF added in comment ?
      ValueItem^.Comment := stringreplace( ValueItem^.Comment, #13#10, '',[rfReplaceAll]);

      if (i+1)<CountValueItems then begin
       ENumTypes.Add(AlignPaddingString('       '+ValueItem^.Name+'='+ValueItem^.ValueStr+',',CommentPadding)+' //< '+ValueItem^.Comment);
      end else begin
       ENumTypes.Add(AlignPaddingString('       '+ValueItem^.Name+'='+ValueItem^.ValueStr,CommentPadding)+' //< '+ValueItem^.Comment);
      end;
     end else begin
      if (i+1)<CountValueItems then begin
       ENumTypes.Add('       '+ValueItem^.Name+'='+ValueItem^.ValueStr+',');
      end else begin
       ENumTypes.Add('       '+ValueItem^.Name+'='+ValueItem^.ValueStr);
      end;
     end;
    end;
    if CountValueItems=0 then begin
     ENumTypes.Add('       T'+Name+'DummyValue=0');
    end;
    ENumTypes.Add('      );');
    ENumTypes.Add('');
   end else begin
    for i:=0 to CountValueItems-1 do begin
     ValueItem:=@ValueItems[i];
     ENumValues.Add(ValueItem^.Name+ENumValues.NameValueSeparator+ValueItem^.ValueStr);
     if length(ValueItem^.Comment)>0 then begin
      ENumConstants.Add(AlignPaddingString('      '+ValueItem^.Name+'='+ValueItem^.ValueStr+';',CommentPadding)+' //< '+ValueItem^.Comment);
     end else begin
      ENumConstants.Add('      '+ValueItem^.Name+'='+ValueItem^.ValueStr+';');
     end;
    end;
   end;
  finally
   Values.Free;
  end;
 finally
  SetLength(ValueItems,0);
 end;
end;

procedure ParseCommandsTag(Tag:TXMLTag);
var i,j,k,ArraySize,CountTypeDefinitions:longint;
    ChildItem,ChildChildItem:TXMLItem;
    ChildTag,ChildChildTag:TXMLTag;
    ProtoName,ProtoType,ParamName,ParamType,Text,Line,Parameters,Define,Alias,AliasName:ansistring;
    ProtoPtr,ParamPtr:longint;
    ValidityStringList:TStringList;
begin
 AllCommandType.Add('     PPOpenXRCommands=^POpenXRCommands;');
 AllCommandType.Add('     POpenXRCommands=^TOpenXRCommands;');
 AllCommandType.Add('     TOpenXRCommands=record');
 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='command' then begin
    Alias:=ChildTag.GetParameter('alias','');
    AliasName:='';
    if length(Alias)>0 then begin
     AliasName:=ChildTag.GetParameter('name','');
     ChildTag:=nil;
     for k:=0 to Tag.Items.Count-1 do begin
      ChildItem:=Tag.Items[k];
      if ChildItem is TXMLTag then begin
       ChildTag:=TXMLTag(ChildItem);
       if ChildTag.Name='command' then begin
        ProtoName:='';
        for j:=0 to ChildTag.Items.Count-1 do begin
         ChildChildItem:=ChildTag.Items[j];
         if ChildChildItem is TXMLTag then begin
          ChildChildTag:=TXMLTag(ChildChildItem);
          if ChildChildTag.Name='proto' then begin
           ProtoName:=ParseText(ChildChildTag.FindTag('name'),['']);
           break;
          end;
         end;
        end;
        if ProtoName=Alias then begin
         break;
        end else begin
         ChildTag:=nil;
        end;
       end;
      end;
     end;
    end;
    if assigned(ChildTag) then begin
     ValidityStringList:=TStringList.Create;
     try
      ParseValidityTag(ChildTag.FindTag('validity'),ValidityStringList);
      ProtoName:='';
      ProtoType:='';
      ProtoPtr:=0;
      ParamName:='';
      ParamType:='';
      ParamPtr:=0;
      Line:='';
      Parameters:='';
      Define:='';
      for j:=0 to ChildTag.Items.Count-1 do begin
       ChildChildItem:=ChildTag.Items[j];
       if ChildChildItem is TXMLTag then begin
        ChildChildTag:=TXMLTag(ChildChildItem);
        if ChildChildTag.Name='proto' then begin
         if length(AliasName)>0 then begin
          ProtoName:=AliasName;
         end else begin
          ProtoName:=ParseText(ChildChildTag.FindTag('name'),['']);
         end;
         if pos('Android',ProtoName)>0 then begin
           define := 'Android';
         end;
         ProtoType:=ParseText(ChildChildTag.FindTag('type'),['']);
         ProtoPtr:=0;
         Text:=ParseText(ChildChildTag,['']);
         for k:=1 to length(Text)-1 do begin
          if Text[k]='*' then begin
           inc(ProtoPtr);
          end;
         end;
        end else if ChildChildTag.Name='param' then begin
         ParamName:=TranslateName(ParseText(ChildChildTag.FindTag('name'),['']));
         ParamType:=ParseText(ChildChildTag.FindTag('type'),['']);
         Text:=ParseText(ChildChildTag,['']);
         ParamPtr:=0;
         for k:=1 to length(Text)-1 do begin
          if Text[k]='*' then begin
           inc(ParamPtr);
          end;
         end;
         if length(Line)>0 then begin
          Line:=Line+';';
         end;
         if ParamName='type' then begin
          ParamName:='type_';
         end else if ParamName='object' then begin
          ParamName:='object_';
         end else if ParamName='set' then begin
          ParamName:='set_';
         end else if ParamName='unit' then begin
          ParamName:='unit_';
         end;
         if pos('const ',trim(Text))=1 then begin
          Line:=Line+'const ';
         end;
         Line:=Line+ParamName+':'+TranslateType(ParamType,ParamPtr);
         if length(Parameters)>0 then begin
          Parameters:=Parameters+',';
         end;
         Parameters:=Parameters+ParamName;
         if (ParamType='HWND') or (ParamType='HMONITOR') or (ParamType='HINSTANCE') or (pos('Win32',ProtoName)>0) then begin
          Define:='Windows';
         // not sure if 'android' only
         end else if (ParamType='timespec')  then begin
          Define:='Android';
         end else if ParamType='RROutput' then begin
          Define:='RandR';
         end else if (ParamType='Display') or (ParamType='VisualID') or (ParamType='Window') or (pos('Xlib',ParamType)>0) then begin
          Define:='XLIB';
         end else if (ParamType='xcb_connection_t') or (ParamType='xcb_visualid_t') or (ParamType='xcb_window_t') or (pos('Xcb',ParamType)>0) then begin
          Define:='XCB';
         end else if (ParamType='wl_display') or (ParamType='wl_surface') or (pos('Wayland',ParamType)>0) then begin
          Define:='Wayland';
         end else if (pos('IOS',UpperCase(ParamType))>0) and (pos('MXR',ParamType)>0) then begin
          Define:='MoltenXR_IOS';
         end else if (pos('MACOS',UpperCase(ParamType))>0) and (pos('MXR',ParamType)>0) then begin
          Define:='MoltenXR_MacOS';
         end else if (pos('MXR',ParamType)>0) or (pos('MOLTENXR',UpperCase(ParamType))>0) then begin
          Define:='MoltenXR';
         end else if (ParamType='ANativeWindow') or (ParamType='AHardwareBuffer') or (pos('Android',ParamType)>0) or (pos('ANDROID',ParamType)>0) then begin
          Define:='Android';
         end else if (ParamType='zx_handle_t') or (pos('FUCHSIA',UpperCase(ParamType))>0) then begin
          Define:='Fuchsia';
         end else if (ParamType='IDirectFB') or (ParamType='IDirectFBSurface') or (pos('DIRECTFB',UpperCase(ParamType))>0) then begin
          Define:='DirectFB';
         end;
        end;
       end;
      end;
      if length(Define)>0 then begin
       CommandTypes.Add('{$ifdef '+Define+'}');
       CommandVariables.Add('{$ifdef '+Define+'}');
       AllCommandType.Add('{$ifdef '+Define+'}');
       AllCommandClassDefinitions.Add('{$ifdef '+Define+'}');
       AllCommandClassImplementations.Add('{$ifdef '+Define+'}');
      end;
      if assigned(ValidityStringList) and (ValidityStringList.Count>0) then begin
       ValidityStringList.Text:=StringReplace(ValidityStringList.Text,'pname:','',[rfReplaceAll]);
       ValidityStringList.Text:=StringReplace(ValidityStringList.Text,'sname:','T',[rfReplaceAll]);
       ValidityStringList.Text:=StringReplace(ValidityStringList.Text,'ename:','T',[rfReplaceAll]);
       ValidityStringList.Text:=StringReplace(ValidityStringList.Text,'fname:','',[rfReplaceAll]);
       ValidityStringList.Text:=WrapString(ValidityStringList.Text,sLineBreak,512);
       for j:=0 to ValidityStringList.Count-1 do begin
        CommandTypes.Add('     // '+ValidityStringList.Strings[j]);
        CommandVariables.Add('    // '+ValidityStringList.Strings[j]);
        AllCommandType.Add('      // '+ValidityStringList.Strings[j]);
        AllCommandClassDefinitions.Add('       // '+ValidityStringList.Strings[j]);
       end;
      end;
      if (ProtoType='void') and (ProtoPtr=0) then begin
       CommandTypes.Add('     T'+ProtoName+'=procedure('+Line+'); '+CallingConventions);
      end else begin
       CommandTypes.Add('     T'+ProtoName+'=function('+Line+'):'+TranslateType(ProtoType,ProtoPtr)+'; '+CallingConventions);
      end;
      CommandVariables.Add('    '+ProtoName+':T'+ProtoName+'=nil;');
      AllCommandType.Add('      '+copy(ProtoName,3,length(ProtoName)-2)+':T'+ProtoName+';');
      if (ProtoType='void') and (ProtoPtr=0) then begin
       AllCommandClassDefinitions.Add('       procedure '+copy(ProtoName,3,length(ProtoName)-2)+'('+Line+'); virtual;');
       AllCommandClassImplementations.Add('procedure TOpenXR.'+copy(ProtoName,3,length(ProtoName)-2)+'('+Line+');');
       AllCommandClassImplementations.Add('begin');
       AllCommandClassImplementations.Add(' fCommands.'+copy(ProtoName,3,length(ProtoName)-2)+'('+Parameters+');');
       AllCommandClassImplementations.Add('end;');
      end else begin
       AllCommandClassDefinitions.Add('       function '+copy(ProtoName,3,length(ProtoName)-2)+'('+Line+'):'+TranslateType(ProtoType,ProtoPtr)+'; virtual;');
       AllCommandClassImplementations.Add('function TOpenXR.'+copy(ProtoName,3,length(ProtoName)-2)+'('+Line+'):'+TranslateType(ProtoType,ProtoPtr)+';');
       AllCommandClassImplementations.Add('begin');
       AllCommandClassImplementations.Add(' result:=fCommands.'+copy(ProtoName,3,length(ProtoName)-2)+'('+Parameters+');');
       AllCommandClassImplementations.Add('end;');
      end;
      AllCommands.Add(ProtoName+'='+Define);
      if length(Define)>0 then begin
       CommandTypes.Add('{$endif}');
       CommandVariables.Add('{$endif}');
       AllCommandType.Add('{$endif}');
       AllCommandClassDefinitions.Add('{$endif}');
       AllCommandClassImplementations.Add('{$endif}');
      end;
      CommandTypes.Add('');
      CommandVariables.Add('');
      AllCommandType.Add('');
      AllCommandClassDefinitions.Add('');
      AllCommandClassImplementations.Add('');
     finally
      ValidityStringList.Free;
     end;
    end;
   end;
  end;
 end;
 AllCommandType.Add('     end;');
end;

procedure ParseRegistryTag(Tag:TXMLTag);
var i,j:longint;
    ChildItem:TXMLItem;
    ChildTag:TXMLTag;
begin

 write('Searching for registry/comment tag . . . ');
 ChildTag:=Tag.FindTag('comment');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseCommentTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/extensions tag . . . ');
 ChildTag:=Tag.FindTag('extensions');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseExtensionsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/feature tags . . . ');
 j:=ParseFeatureTags(Tag);
 if j>0 then begin
  writeln(j,' feature tags found!');
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/vendorids tag . . . ');
 ChildTag:=Tag.FindTag('vendorids');
 if assigned(ChildTag) then begin
  ParseVendorIDsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/tags tag . . . ');
 ChildTag:=Tag.FindTag('tags');
 if assigned(ChildTag) then begin
  ParseTagsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 write('Searching for registry/types tag . . . ');
 ChildTag:=Tag.FindTag('types');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseTypesTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

 for i:=0 to Tag.Items.Count-1 do begin
  ChildItem:=Tag.Items[i];
  if ChildItem is TXMLTag then begin
   ChildTag:=TXMLTag(ChildItem);
   if ChildTag.Name='enums' then begin
    ParseEnumsTag(ChildTag);
   end;
  end;
 end;

 write('Searching for registry/commands tag . . . ');
 ChildTag:=Tag.FindTag('commands');
 if assigned(ChildTag) then begin
  writeln('found!');
  ParseCommandsTag(ChildTag);
 end else begin
  writeln('not found!');
 end;

end;

var XRXMLFileStream:TMemoryStream;
    XRXML:TXML;
    RegistryTag:TXMLTag;
    OutputPAS:TStringList;

var i,j:longint;
    ExtensionOrFeatureEnum:TExtensionOrFeatureEnum;
    s,s2:ansistring;
begin
 Comment:='';
 VendorIDList:=TObjectList.Create(true);
 TagList:=TObjectList.Create(true);
 ExtensionsOrFeatures:=TObjectList.Create(true);
 ExtensionOrFeatureEnums:=TStringList.Create;
 ExtensionOrFeatureTypes:=TStringList.Create;
 ExtensionOrFeatureCommands:=TStringList.Create;
 VersionConstants:=TStringList.Create;
 BaseTypes:=TStringList.Create;
 BitMaskTypes:=TStringList.Create;
 HandleTypes:=TStringList.Create;
 ENumTypes:=TStringList.Create;
 AliasENumTypes:=TStringList.Create;
 ENumConstants:=TStringList.Create;
 ENumValues:=TStringList.Create;
 TypeDefinitionTypes:=TStringList.Create;
 TypeDefinitionConstructors:=TStringList.Create;
 CommandTypes:=TStringList.Create;
 CommandVariables:=TStringList.Create;
 AllCommandType:=TStringList.Create;
 AllCommands:=TStringList.Create;
 AllCommandClassDefinitions:=TStringList.Create;
 AllCommandClassImplementations:=TStringList.Create;

 InitializeEntites;
 try

  XRXMLFileStream:=TMemoryStream.Create;
  try

   write('Loading "xr.xml" . . . ');

   try
    XRXMLFileStream.LoadFromFile('xr.xml');
   except
    writeln('Error!');
    raise;
   end;

   if XRXMLFileStream.Seek(0,soBeginning)=0 then begin

    writeln('OK!');

    XRXML:=TXML.Create;
    try

     write('Parsing "xr.xml" . . . ');
     if XRXML.Parse(XRXMLFileStream) then begin

      writeln('OK!');

      write('Searching for registry tag . . . ');
      RegistryTag:=XRXML.Root.FindTag('registry');
      if assigned(RegistryTag) then begin
       writeln('found!');
       ParseRegistryTag(RegistryTag);
      end else begin
       writeln('not found!');
      end;

     end else begin
      writeln('Error!');
     end;

    finally
     XRXML.Free;
    end;

   end else begin
    writeln('Error!');
   end;

  finally
   XRXMLFileStream.Free;
  end;

  write('Generating "OpenXR.pas" . . . ');
  ProcessExtensions;
  OutputPAS:=TStringList.Create;
  try
   OutputPAS.Add('(*');
   OutputPAS.Add('** Copyright (c) 2015-2016 The Khronos Group Inc.');
   OutputPAS.Add('** Copyright (c) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de, the pascal headers)');
   OutputPAS.Add('**');
   OutputPAS.Add('** Permission is hereby granted, free of charge, to any person obtaining a');
   OutputPAS.Add('** copy of this software and/or associated documentation files (the');
   OutputPAS.Add('** "Materials"), to deal in the Materials without restriction, including');
   OutputPAS.Add('** without limitation the rights to use, copy, modify, merge, publish,');
   OutputPAS.Add('** distribute, sublicense, and/or sell copies of the Materials, and to');
   OutputPAS.Add('** permit persons to whom the Materials are furnished to do so, subject to');
   OutputPAS.Add('** the following conditions:');
   OutputPAS.Add('**');
   OutputPAS.Add('** The above copyright notice and this permission notice shall be included');
   OutputPAS.Add('** in all copies or substantial portions of the Materials.');
   OutputPAS.Add('**');
   OutputPAS.Add('** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,');
   OutputPAS.Add('** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF');
   OutputPAS.Add('** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.');
   OutputPAS.Add('** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY');
   OutputPAS.Add('** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,');
   OutputPAS.Add('** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE');
   OutputPAS.Add('** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.');
   OutputPAS.Add('*)');
   OutputPAS.Add('(*');
   OutputPAS.Add('** This header is generated from the Khronos OpenXR XML API Registry.');
   OutputPAS.Add('**');
   OutputPAS.Add('*)');
   OutputPAS.Add('unit OpenXR;');
   OutputPAS.Add('{$ifdef fpc}');
   OutputPAS.Add(' {$mode delphi}');
   OutputPAS.Add(' {$z4}');
   OutputPAS.Add(' {$packrecords c}');
   OutputPAS.Add(' {$define CAN_INLINE}');
   OutputPAS.Add(' {$define HAS_ADVANCED_RECORDS}');
   OutputPAS.Add(' {$notes off}');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' {$z4}');
   OutputPAS.Add(' {$undef CAN_INLINE}');
   OutputPAS.Add(' {$undef HAS_ADVANCED_RECORDS}');
   OutputPAS.Add(' {$ifdef conditionalexpressions}');
   OutputPAS.Add('  {$if CompilerVersion>=24.0}');
   OutputPAS.Add('   {$legacyifend on}');
   OutputPAS.Add('  {$ifend}');
   OutputPAS.Add('  {$if CompilerVersion>=18.0}');
   OutputPAS.Add('   {$define CAN_INLINE}');
   OutputPAS.Add('   {$define HAS_ADVANCED_RECORDS}');
   OutputPAS.Add('  {$ifend}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef Win32}');
   OutputPAS.Add(' {$define Windows}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef Win64}');
   OutputPAS.Add(' {$define Windows}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef WinCE}');
   OutputPAS.Add(' {$define Windows}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$if defined(Android)}');
   OutputPAS.Add(' {$define XR_USE_PLATFORM_ANDROID_KHR}');
   OutputPAS.Add('{$elseif defined(Windows)}');
   OutputPAS.Add(' {$define XR_USE_PLATFORM_WIN32_KHR}');
   OutputPAS.Add('{$elseif defined(Unix) or defined(Linux)}');
   OutputPAS.Add(' {$ifdef WAYLAND}');
   OutputPAS.Add('  {$define XR_USE_PLATFORM_WAYLAND_KHR}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add(' {$ifdef XCB}');
   OutputPAS.Add('  {$define XR_USE_PLATFORM_XCB_KHR}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add(' {$ifdef XLIB}');
   OutputPAS.Add('  {$define XR_USE_PLATFORM_XLIB_KHR}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add('{$ifend}');
   OutputPAS.Add('');
   OutputPAS.Add('interface');
   OutputPAS.Add('');
   OutputPAS.Add('uses {$if defined(Windows)}');
   OutputPAS.Add('      Windows,');
   OutputPAS.Add('      Winapi.D3DCommon,');
   OutputPAS.Add('      Winapi.D3D11,');
   OutputPAS.Add('      Winapi.D3D12,');
   OutputPAS.Add('      Vulkan,');
   OutputPAS.Add('     {$elseif defined(Unix)}');
   OutputPAS.Add('      BaseUnix,UnixType,dl,');
   OutputPAS.Add('     {$ifend}');
   OutputPAS.Add('     {$if defined(XLIB) and defined(OpenXRUseXLIBUnits)}x,xlib,{$ifend}');
   OutputPAS.Add('     {$if defined(XCB) and defined(OpenXRUseXCBUnits)}xcb,{$ifend}');
   OutputPAS.Add('     {$if defined(Wayland) and defined(OpenXRUseWaylandUnits)}Wayland,{$ifend}');
   OutputPAS.Add('     {$if defined(Android) and defined(OpenXRUseAndroidUnits)}Android,{$ifend}');
   OutputPAS.Add('     {$if defined(Fuchsia) and defined(OpenXRUseFuchsiaUnits)}Fuchsia,{$ifend}');
   OutputPAS.Add('     {$if defined(DirectFB) and defined(OpenXRUseDirectFBUnits)}DirectFB,{$ifend}');
   OutputPAS.Add('     SysUtils;');
   OutputPAS.Add('');
   OutputPAS.Add('const XR_DEFAULT_LIB_NAME={$ifdef Windows}''openxr_loader.dll''{$else}{$ifdef Android}''openxr_loader.so''{$else}{$ifdef Unix}''openxr_loader.so.1''{$else}''openxr_loader''{$endif}{$endif}{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('type PPXrInt8=^PXrInt8;');
   OutputPAS.Add('     PXrInt8=^TXrInt8;');
   OutputPAS.Add('     TXrInt8={$ifdef FPC}Int8{$else}ShortInt{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrUInt8=^PXrUInt8;');
   OutputPAS.Add('     PXrUInt8=^TXrUInt8;');
   OutputPAS.Add('     TXrUInt8={$ifdef FPC}UInt8{$else}Byte{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrInt16=^PXrInt16;');
   OutputPAS.Add('     PXrInt16=^TXrInt16;');
   OutputPAS.Add('     TXrInt16={$ifdef FPC}Int16{$else}SmallInt{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrUInt16=^PXrUInt16;');
   OutputPAS.Add('     PXrUInt16=^TXrUInt16;');
   OutputPAS.Add('     TXrUInt16={$ifdef FPC}UInt16{$else}Word{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrInt32=^PXrInt32;');
   OutputPAS.Add('     PXrInt32=^TXrInt32;');
   OutputPAS.Add('     TXrInt32={$ifdef FPC}Int32{$else}LongInt{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrUInt32=^PXrUInt32;');
   OutputPAS.Add('     PXrUInt32=^TXrUInt32;');
   OutputPAS.Add('     TXrUInt32={$ifdef FPC}UInt32{$else}LongWord{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrInt64=^PXrInt64;');
   OutputPAS.Add('     PXrInt64=^TXrInt64;');
   OutputPAS.Add('     TXrInt64=Int64;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrUInt64=^PXrUInt64;');
   OutputPAS.Add('     PXrUInt64=^TXrUInt64;');
   OutputPAS.Add('     TXrUInt64=UInt64;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrChar=^PXrChar;');
   OutputPAS.Add('     PXrChar=PAnsiChar;');
   OutputPAS.Add('     TXrChar=AnsiChar;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrWideChar=^PXrWideChar;');
   OutputPAS.Add('     PXrWideChar=PWideChar;');
   OutputPAS.Add('     TXrWideChar=WideChar;');
   OutputPAS.Add('');


   OutputPAS.Add('     PPXrPointer=^PXrPointer;');
   OutputPAS.Add('     PXrPointer=^TXrPointer;');
   OutputPAS.Add('     TXrPointer=Pointer;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrVoid=^PXrVoid;');
   OutputPAS.Add('     PXrVoid=Pointer;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrHalfFloat=^PXrHalfFloat;');
   OutputPAS.Add('     PXrHalfFloat=^TXrHalfFloat;');
   OutputPAS.Add('     TXrHalfFloat=TXrUInt16;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrFloat=^PXrFloat;');
   OutputPAS.Add('     PXrFloat=^TXrFloat;');
   OutputPAS.Add('     TXrFloat=Single;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrDouble=^PXrDouble;');
   OutputPAS.Add('     PXrDouble=^TXrDouble;');
   OutputPAS.Add('     TXrDouble=Double;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrPtrUInt=^PXrPtrUInt;');
   OutputPAS.Add('     PPXrPtrInt=^PXrPtrInt;');
   OutputPAS.Add('     PXrPtrUInt=^TXrPtrUInt;');
   OutputPAS.Add('     PXrPtrInt=^TXrPtrInt;');
   OutputPAS.Add('{$ifdef fpc}');
   OutputPAS.Add('     TXrPtrUInt=PtrUInt;');
   OutputPAS.Add('     TXrPtrInt=PtrInt;');
   OutputPAS.Add(' {$undef OldDelphi}');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' {$ifdef conditionalexpressions}');
   OutputPAS.Add('  {$if CompilerVersion>=23.0}');
   OutputPAS.Add('   {$undef OldDelphi}');
   OutputPAS.Add('     TXrPtrUInt=NativeUInt;');
   OutputPAS.Add('     TXrPtrInt=NativeInt;');
   OutputPAS.Add('  {$else}');
   OutputPAS.Add('   {$define OldDelphi}');
   OutputPAS.Add('  {$ifend}');
   OutputPAS.Add(' {$else}');
   OutputPAS.Add('  {$define OldDelphi}');
   OutputPAS.Add(' {$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$ifdef OldDelphi}');
   OutputPAS.Add('{$ifdef cpu64}');
   OutputPAS.Add('     TXrPtrUInt=TXrUInt64;');
   OutputPAS.Add('     TXrPtrInt=TXrInt64;');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('     TXrPtrUInt=TXrUInt32;');
   OutputPAS.Add('     TXrPtrInt=TXrInt32;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrSizeUInt=^PXrSizeUInt;');
   OutputPAS.Add('     PXrSizeUInt=^TXrSizeUInt;');
   OutputPAS.Add('     TXrSizeUInt=TXrPtrUInt;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrSizeInt=^PXrSizeInt;');
   OutputPAS.Add('     PXrSizeInt=^TXrSizeInt;');
   OutputPAS.Add('     TXrSizeInt=TXrPtrInt;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrSize=^PXrSizeUInt;');
   OutputPAS.Add('     PXrSize=^TXrSizeUInt;');
   OutputPAS.Add('     TXrSize=TXrPtrUInt;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrPtrDiff=^PXrPtrDiff;');
   OutputPAS.Add('     PXrPtrDiff=^TXrPtrDiff;');
   OutputPAS.Add('     TXrPtrDiff=TXrPtrInt;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrCharString=^PXrCharString;');
   OutputPAS.Add('     PXrCharString=^TXrCharString;');
   OutputPAS.Add('     TXrCharString=AnsiString;');
   OutputPAS.Add('');
   OutputPAS.Add('{$ifdef Android}');
   OutputPAS.Add('     PPXrAndroidANativeWindow=^PXrAndroidANativeWindow;');
   OutputPAS.Add('     PXrAndroidANativeWindow={$ifdef OpenXRUseAndroidUnits}PANativeWindow{$else}TXrPointer{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrAndroidAHardwareBuffer=^PXrAndroidAHardwareBuffer;');
   OutputPAS.Add('     PXrAndroidAHardwareBuffer={$ifdef OpenXRUseAndroidUnits}PAHardwareBuffer{$else}TXrPointer{$endif};');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('{$ifdef Fuchsia}');
   OutputPAS.Add('     PPXrFuchsiaZXHandle=^PXrFuchsiaZXHandle;');
   OutputPAS.Add('     PXrFuchsiaZXHandle=^TXrFuchsiaZXHandle;');
   OutputPAS.Add('     TXrFuchsiaZXHandle={$ifdef OpenXRUseFuchsiaUnits}Tzx_handle_t{$else}TXrSizeUInt{$endif};');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('{$ifdef DirectFB}');
   OutputPAS.Add('     PPXrDirectFBIDirectFB=^PXrDirectFBIDirectFB;');
   OutputPAS.Add('     PXrDirectFBIDirectFB=^TXrDirectFBIDirectFB;');
   OutputPAS.Add('     TXrDirectFBIDirectFB={$ifdef OpenXRUseDirectFBUnits}IDirectFB{$else}TXrSizeUInt{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrDirectFBIDirectFBSurface=^PXrDirectFBIDirectFBSurface;');
   OutputPAS.Add('     PXrDirectFBIDirectFBSurface=^TXrDirectFBIDirectFBSurface;');
   OutputPAS.Add('     TXrDirectFBIDirectFBSurface={$ifdef OpenXRUseDirectFBUnits}IDirectFBSurface{$else}TXrSizeUInt{$endif};');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('{$ifdef Wayland}');
   OutputPAS.Add('     PPXrWaylandDisplay=^PXrWaylandDisplay;');
   OutputPAS.Add('     PXrWaylandDisplay={$ifdef OpenXRUseWaylandUnits}Pwl_display{$else}TXrPointer{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrWaylandSurface=^PXrWaylandSurface;');
   OutputPAS.Add('     PXrWaylandSurface={$ifdef OpenXRUseWaylandUnits}Pwl_surface{$else}TXrPointer{$endif};');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('{$ifdef XCB}');
   OutputPAS.Add('     PPXrXCBConnection=^PXrXCBConnection;');
   OutputPAS.Add('     PXrXCBConnection={$ifdef OpenXRUseXCBUnits}Pxcb_connection_t{$else}TXrPointer{$endif};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrXCBVisualID=^PXrXCBVisualID;');
   OutputPAS.Add('     PXrXCBVisualID={$ifdef OpenXRUseXCBUnits}Pxcb_visualid_t{$else}^TXrXCBVisualID{$endif};');
   OutputPAS.Add('     TXrXCBVisualID={$if defined(OpenXRUseXCBUnits)}Pxcb_visualid_t{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrXCBWindow=^PXrXCBWindow;');
   OutputPAS.Add('     PXrXCBWindow={$ifdef OpenXRUseXCBUnits}Pxcb_window_t{$else}^TXrXCBWindow{$endif};');
   OutputPAS.Add('     TXrXCBWindow={$if defined(OpenXRUseXCBUnits)}Txcb_window_t{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('{$ifdef XLIB}');
   OutputPAS.Add('     PPXrXLIBDisplay=^PXrXLIBDisplay;');
   OutputPAS.Add('     PXrXLIBDisplay={$ifdef OpenXRUseXLIBUnits}PDisplay{$else}TXrPointer{$endif};');
   OutputPAS.Add('     {$ifdef OpenXRUseXLIBUnits}TXrXLIBDisplay=TDisplay;{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrXLIBVisualID=^PXrXLIBVisualID;');
   OutputPAS.Add('     PXrXLIBVisualID={$ifdef OpenXRUseXLIBUnits}PVisualID{$else}^TXrXLIBVisualID{$endif};');
   OutputPAS.Add('     TXrXLIBVisualID={$if defined(OpenXRUseXLIBUnits)}TVisualID{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrXLIBWindow=^PXrXLIBWindow;');
   OutputPAS.Add('     PXrXLIBWindow={$ifdef OpenXRUseXLIBUnits}PWindow{$else}^TXrXLIBWindow{$endif};');
   OutputPAS.Add('     TXrXLIBWindow={$if defined(OpenXRUseXLIBUnits)}TWindow{$elseif defined(CPU64)}TXrUInt64{$else}TXRUInt32{$ifend};');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('     TXrNonDefinedType=pointer;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrGgpStreamDescriptor=^PXrGgpStreamDescriptor;');
   OutputPAS.Add('     PXrGgpStreamDescriptor=^TXrGgpStreamDescriptor;');
   OutputPAS.Add('     TXrGgpStreamDescriptor=TXrNonDefinedType;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrGgpFrameToken=^PXrGgpFrameToken;');
   OutputPAS.Add('     PXrGgpFrameToken=^TXrGgpFrameToken;');
   OutputPAS.Add('     TXrGgpFrameToken=TXrNonDefinedType;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrCAMetalLayer=^PXrCAMetalLayer;');
   OutputPAS.Add('     PXrCAMetalLayer=^TXrCAMetalLayer;');
   OutputPAS.Add('     TXrCAMetalLayer=TXrNonDefinedType;');
   OutputPAS.Add('');
   OutputPAS.Add('const XR_NULL_HANDLE=0;');
   OutputPAS.Add('');
   OutputPAS.Add('      XR_NULL_INSTANCE=0;');
   OutputPAS.Add('');
   OutputPAS.AddStrings(VersionConstants);
   OutputPAS.AddStrings(ENumConstants);
   OutputPAS.Add('');
   OutputPAS.Add('type PPXrDispatchableHandle=^PXrDispatchableHandle;');
   OutputPAS.Add('     PXrDispatchableHandle=^TXrDispatchableHandle;');
   OutputPAS.Add('     TXrDispatchableHandle=TXrUInt64;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrNonDispatchableHandle=^PXrNonDispatchableHandle;');
   OutputPAS.Add('     PXrNonDispatchableHandle=^TXrNonDispatchableHandle;');
   OutputPAS.Add('     TXrNonDispatchableHandle=TXrUInt64;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrAtom=^PXrAtom;');
   OutputPAS.Add('     PXrAtom=^TXrAtom;');
   OutputPAS.Add('     TXrAtom=TXrUInt64;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrHDC=^PXrHDC;');
   OutputPAS.Add('     PXrHDC=^TXrHDC;');
   OutputPAS.Add('     TXrHDC=HDC;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrHGLRC=^PXrHGLRC;');
   OutputPAS.Add('     PXrHGLRC=^TXrHGLRC;');
   OutputPAS.Add('     TXrHGLRC=HGLRC;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrEnum=^PXrEnum;');
   OutputPAS.Add('     PXrEnum=^TXrEnum;');
   OutputPAS.Add('     TXrEnum=TXrInt32;');
   OutputPAS.Add('');
   //not sure about this one...
   OutputPAS.Add('     TPFN_vkGetInstanceProcAddr=TvkGetInstanceProcAddr;');
   OutputPAS.Add('');


   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add('     PPXrHINSTANCE=^PXrHINSTANCE;');
   OutputPAS.Add('     PXrHINSTANCE=^TXrHINSTANCE;');
   OutputPAS.Add('     TXrHINSTANCE=TXrPtrUInt;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrHWND=^PXrHWND;');
   OutputPAS.Add('     PXrHWND=^TXrHWND;');
   OutputPAS.Add('     TXrHWND=HWND;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPXrHMONITOR=^PXrHMONITOR;');
   OutputPAS.Add('     PXrHMONITOR=^TXrHMONITOR;');
   OutputPAS.Add('     TXrHMONITOR=HMONITOR;');
   OutputPAS.Add('');
   OutputPAS.Add('     PPIUnknown=^IUnknown;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('');
   OutputPAS.AddStrings(BaseTypes);
   OutputPAS.AddStrings(BitMaskTypes);
   OutputPAS.AddStrings(HandleTypes);
   OutputPAS.AddStrings(EnumTypes);
   OutputPAS.AddStrings(AliasEnumTypes);
   OutputPAS.AddStrings(TypeDefinitionTypes);
   OutputPAS.AddStrings(CommandTypes);
   OutputPAS.Add('');
   OutputPAS.AddStrings(AllCommandType);
   OutputPAS.Add('');
   OutputPAS.Add('     TOpenXR=class');
   OutputPAS.Add('      private');
   OutputPAS.Add('       fCommands:TOpenXRCommands;');
   OutputPAS.Add('      public');
   OutputPAS.Add('       constructor Create; reintroduce; overload;');
   OutputPAS.Add('       constructor Create(const AOpenXRCommands:TOpenXRCommands); reintroduce; overload;');
   OutputPAS.Add('       destructor Destroy; override;');
   OutputPAS.AddStrings(AllCommandClassDefinitions);
   OutputPAS.Add('       property Commands:TOpenXRCommands read fCommands;');
   OutputPAS.Add('     end;');
   OutputPAS.Add('');
   OutputPAS.Add('var LibOpenXR:pointer=nil;');
   OutputPAS.Add('');
   OutputPAS.Add('    xr:TOpenXR=nil;');
   OutputPAS.Add('');
   OutputPAS.AddStrings(CommandVariables);
   OutputPAS.Add('');
   OutputPAS.Add('function XR_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):TXrVersion; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function XR_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function XR_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function XR_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('function xrLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function xrFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('function xrGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('function xrVoidFunctionToPointer(const VoidFunction:TPFN_xrVoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('');
   OutputPAS.Add('function LoadOpenXRLibrary(const LibraryName:string=XR_DEFAULT_LIB_NAME):boolean;');
   OutputPAS.Add('function LoadOpenXRGlobalCommands:boolean;');
   OutputPAS.Add('function LoadOpenXRInstanceCommands(const GetInstanceProcAddr:TxrGetInstanceProcAddr;const Instance:TXrInstance;out InstanceCommands:TOpenXRCommands):boolean;');
   OutputPAS.Add('');
   OutputPAS.Add('implementation');
   OutputPAS.Add('');
   OutputPAS.Add('function XR_MAKE_VERSION(const VersionMajor,VersionMinor,VersionPatch:longint):TXrVersion; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=(int64(VersionMajor) shl 48) or (VersionMinor shl 32) or (VersionPatch shl 0);');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function XR_VERSION_MAJOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=int64(Version) shr 48;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function XR_VERSION_MINOR(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=(Version shr 32) and $ffff;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function XR_VERSION_PATCH(const Version:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=(Version shr 0) and $ffffffff;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function xrLoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add(' result:={%H-}pointer(LoadLibrary(PChar(LibraryName)));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Unix}');
   OutputPAS.Add(' result:=dlopen(PChar(LibraryName),RTLD_NOW or RTLD_LAZY);');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' result:=nil;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function xrFreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=assigned(LibraryHandle);');
   OutputPAS.Add(' if result then begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add('  result:=FreeLibrary({%H-}HMODULE(LibraryHandle));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Unix}');
   OutputPAS.Add('  result:=dlclose(LibraryHandle)=0;');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('  result:=false;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add(' end;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function xrGetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add('{$ifdef Windows}');
   OutputPAS.Add(' result:=GetProcAddress({%H-}HMODULE(LibraryHandle),PChar(ProcName));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add('{$ifdef Unix}');
   OutputPAS.Add(' result:=dlsym(LibraryHandle,PChar(ProcName));');
   OutputPAS.Add('{$else}');
   OutputPAS.Add(' result:=nil;');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('{$endif}');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function xrVoidFunctionToPointer(const VoidFunction:TPFN_xrVoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}');
   OutputPAS.Add('begin');
   OutputPAS.Add(' result:=addr(VoidFunction);');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function LoadOpenXRLibrary(const LibraryName:string=XR_DEFAULT_LIB_NAME):boolean;');
   OutputPAS.Add('begin');
   OutputPAS.Add(' LibOpenXR:=xrLoadLibrary(LibraryName);');
   OutputPAS.Add(' result:=assigned(LibOpenXR);');
   OutputPAS.Add(' if result then begin');
   OutputPAS.Add('   @xrGetInstanceProcAddr:=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,''xrGetInstanceProcAddr''));');
   OutputPAS.Add('   @xr.fCommands.GetInstanceProcAddr:=addr(xrGetInstanceProcAddr);');
   OutputPAS.Add(' end;');
   OutputPAS.Add(' (*');
   OutputPAS.Add(' if result then begin');
   OutputPAS.Add('  xrGetInstanceProcAddr:=xrGetProcAddress(LibOpenXR,''xrGetInstanceProcAddr'');');
   OutputPAS.Add('  @xr.fCommands.GetInstanceProcAddr:=addr(xrGetInstanceProcAddr);');
   OutputPAS.Add('  result:=assigned(xrGetInstanceProcAddr);');
   OutputPAS.Add('  if result then begin');
   OutputPAS.Add('   xrEnumerateInstanceExtensionProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar(''xrEnumerateInstanceExtensionProperties'')));');
   OutputPAS.Add('   @xr.fCommands.EnumerateInstanceExtensionProperties:=addr(xrEnumerateInstanceExtensionProperties);');
   OutputPAS.Add('   xrEnumerateInstanceLayerProperties:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar(''xrEnumerateInstanceLayerProperties'')));');
   OutputPAS.Add('   @xr.fCommands.EnumerateInstanceLayerProperties:=addr(xrEnumerateInstanceLayerProperties);');
   OutputPAS.Add('   xrCreateInstance:=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar(''xrCreateInstance'')));');
   OutputPAS.Add('   @xr.fCommands.CreateInstance:=addr(xrCreateInstance);');
   OutputPAS.Add('   result:=assigned(xrEnumerateInstanceExtensionProperties) and');
   OutputPAS.Add('           assigned(xrEnumerateInstanceLayerProperties) and ');
   OutputPAS.Add('           assigned(xrCreateInstance);');
   OutputPAS.Add('  end;');
   OutputPAS.Add(' end;');
   OutputPAS.Add(' *)');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function LoadOpenXRGlobalCommands:boolean;');
   OutputPAS.Add('begin');
// OutputPAS.Add(' FillChar(xr.fCommands,SizeOf(TOpenXRCommands),#0);');
   OutputPAS.Add(' result:=assigned(xrGetInstanceProcAddr);');
   OutputPAS.Add(' if result then begin');
   for i:=0 to AllCommands.Count-1 do begin
    s:=AllCommands.Strings[i];
    j:=pos('=',s);
    if j>0 then begin
     s2:=copy(s,j+1,length(s)-j);
     s:=copy(s,1,j-1);
    end;
    if length(s2)>0 then begin
     OutputPAS.Add('{$ifdef '+s2+'}');
    end;
    OutputPAS.Add('  if not assigned('+s+') then begin');
    OutputPAS.Add('   @'+s+':=xrVoidFunctionToPointer(xrGetProcAddress(LibOpenXR,'''+s+'''));');
//  OutputPAS.Add('   @'+s+':=xrVoidFunctionToPointer(xrGetInstanceProcAddr(XR_NULL_INSTANCE,PXrChar('''+s+''')));');
    OutputPAS.Add('   @xr.fCommands.'+copy(s,3,length(s)-2)+':=addr('+s+');');
    OutputPAS.Add('  end;');
    if length(s2)>0 then begin
     OutputPAS.Add('{$endif}');
    end;
   end;
   OutputPAS.Add('  result:=assigned(xrCreateInstance);');
   OutputPAS.Add(' end;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('function LoadOpenXRInstanceCommands(const GetInstanceProcAddr:TxrGetInstanceProcAddr;const Instance:TXrInstance;out InstanceCommands:TOpenXRCommands):boolean;');
   OutputPAS.Add('begin');
   OutputPAS.Add(' FillChar(InstanceCommands,SizeOf(TOpenXRCommands),#0);');
   OutputPAS.Add(' result:=assigned(GetInstanceProcAddr);');
   OutputPAS.Add(' if result then begin');
   for i:=0 to AllCommands.Count-1 do begin
    s:=AllCommands.Strings[i];
    j:=pos('=',s);
    if j>0 then begin
     s2:=copy(s,j+1,length(s)-j);
     s:=copy(s,1,j-1);
    end;
    if length(s2)>0 then begin
     OutputPAS.Add('{$ifdef '+s2+'}');
    end;
    OutputPAS.Add('  xrGetInstanceProcAddr(Instance,PXrChar('''+s+'''),@InstanceCommands.'+copy(s,3,length(s)-2)+');');
    if length(s2)>0 then begin
     OutputPAS.Add('{$endif}');
    end;
   end;
   OutputPAS.Add('  if not assigned(InstanceCommands.EnumerateApiLayerProperties) then begin');
   OutputPAS.Add('   InstanceCommands.EnumerateApiLayerProperties:=addr(xrEnumerateApiLayerProperties);');
   OutputPAS.Add('  end;');
   OutputPAS.Add('  if not assigned(InstanceCommands.EnumerateInstanceExtensionProperties) then begin');
   OutputPAS.Add('   InstanceCommands.EnumerateInstanceExtensionProperties:=addr(xrEnumerateInstanceExtensionProperties);');
   OutputPAS.Add('  end;');
   OutputPAS.Add('  if not assigned(InstanceCommands.CreateInstance) then begin');
   OutputPAS.Add('   InstanceCommands.CreateInstance:=addr(xrCreateInstance);');
   OutputPAS.Add('  end;');
   OutputPAS.Add('  result:=assigned(InstanceCommands.DestroyInstance);');
   OutputPAS.Add(' end;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   if TypeDefinitionConstructors.Count>0 then begin
    OutputPAS.Add('{$ifdef HAS_ADVANCED_RECORDS}');
    OutputPAS.AddStrings(TypeDefinitionConstructors);
    OutputPAS.Add('{$endif}');
    OutputPAS.Add('');
   end;
   OutputPAS.Add('constructor TOpenXR.Create;');
   OutputPAS.Add('begin');
   OutputPAS.Add(' inherited Create;');
   OutputPAS.Add(' FillChar(fCommands,SizeOf(TOpenXRCommands),#0);');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('constructor TOpenXR.Create(const AOpenXRCommands:TOpenXRCommands);');
   OutputPAS.Add('begin');
   OutputPAS.Add(' inherited Create;');
   OutputPAS.Add(' fCommands:=AOpenXRCommands;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.Add('destructor TOpenXR.Destroy;');
   OutputPAS.Add('begin');
   OutputPAS.Add(' inherited Destroy;');
   OutputPAS.Add('end;');
   OutputPAS.Add('');
   OutputPAS.AddStrings(AllCommandClassImplementations);
   OutputPAS.Add('initialization');
   OutputPAS.Add(' xr:=TOpenXR.Create;');
   OutputPAS.Add('finalization');
   OutputPAS.Add(' xr.Free;');
   OutputPAS.Add(' if assigned(LibOpenXR) then begin');
   OutputPAS.Add('  xrFreeLibrary(LibOpenXR);');
   OutputPAS.Add(' end;');
   OutputPAS.Add('end.');
   OutputPAS.SaveToFile('OpenXR.pas');
  finally
   OutputPAS.Free;
  end;
  writeln('done!');

 finally
  FinalizeEntites;
  VendorIDList.Free;
  TagList.Free;
  ExtensionsOrFeatures.Free;
  ExtensionOrFeatureEnums.Free;
  ExtensionOrFeatureTypes.Free;
  ExtensionOrFeatureCommands.Free;
  BitMaskTypes.Free;
  VersionConstants.Free;
  BaseTypes.Free;
  HandleTypes.Free;
  ENumTypes.Free;
  AliasENumTypes.Free;
  ENumConstants.Free;
  ENumValues.Free;
  TypeDefinitionConstructors.Free;
  TypeDefinitionTypes.Free;
  CommandTypes.Free;
  CommandVariables.Free;
  AllCommandType.Free;
  AllCommands.Free;
  AllCommandClassDefinitions.Free;
  AllCommandClassImplementations.Free;
 end;

 readln;
end.


