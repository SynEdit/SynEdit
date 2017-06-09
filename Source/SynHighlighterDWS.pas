{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterDWS.pas,v 1.11 2011/12/28 09:24:20 Egg Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a DWScript syntax highlighter for SynEdit)
}

unit SynHighlighterDWS;

{$I SynEdit.Inc}

interface

uses
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SysUtils,
  Classes,
  Character;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsHereDocSingle, rsHereDocDouble,
    rsType, rsUnknown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function : TtkTokenKind of object;

type
   TAnsiStringList = class(TStringList)
     function CompareStrings(const S1, S2: string): Integer; override;
   end;

type
  TSynDWSSyn = class(TSynCustomHighlighter)
  private
    FAsmStart: Boolean;
    FRange: TRangeState;
    FCommentClose : Char;
    FIdentFuncTable: array[0..388] of TIdentFuncTableFunc;
    FKeywords: TAnsiStringList;
    FKeywordsPropertyScoped: TAnsiStringList;
    FKeywordsTypeScoped: TAnsiStringList;
    FTokenID: TtkTokenKind;
    FStringAttri: TSynHighlighterAttributes;
    FCharAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FFloatAttri: TSynHighlighterAttributes;
    FHexAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FAsmAttri: TSynHighlighterAttributes;
    FCommentAttri: TSynHighlighterAttributes;
    FDirecAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    function AltFunc: TtkTokenKind;
    function KeywordFunc: TtkTokenKind;
    function FuncAsm: TtkTokenKind;
    function FuncEnd: TtkTokenKind;
    function FuncPropertyScoped: TtkTokenKind;
    function FuncProperty: TtkTokenKind;
    function FuncTypeScoped: TtkTokenKind;
    function FuncType: TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringAposProc;
    procedure StringAposMultiProc;
    procedure StringQuoteProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
    function IsCurrentToken(const Token: UnicodeString): Boolean; override;

  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function IsIdentChar(AChar: WideChar): Boolean; override;

    procedure LoadDelphiStyle; virtual;
    // ^^^
    // This routine can be called to install a Delphi style of colors
    // and highlighting. It modifies the basic TSynDWSSyn to reproduce
    // the most recent Delphi editor highlighting.

  published
    property AsmAttri: TSynHighlighterAttributes read FAsmAttri write FAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri
      write FCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirecAttri
      write FDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri
      write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri
      write FNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read FFloatAttri
      write FFloatAttri;
    property HexAttri: TSynHighlighterAttributes read FHexAttri
      write FHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri
      write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri
      write FStringAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri
      write FCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri
      write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

const
   // if the language is case-insensitive keywords *must* be in lowercase
   cKeywords: array[1..96] of UnicodeString = (
      'abstract', 'and', 'array', 'as', 'asm',
      'begin', 'break', 'case', 'cdecl', 'class', 'const', 'constructor',
      'continue', 'deprecated', 'destructor',
      'div', 'do', 'downto', 'else', 'end', 'ensure', 'empty', 'except',
      'exit', 'export', 'exports', 'external', 'final', 'finalization',
      'finally', 'for', 'forward', 'function', 'helper', 'if',
      'implementation', 'implements', 'implies', 'in', 'inherited',
      'initialization', 'inline', 'interface', 'is', 'lambda', 'lazy', 'library',
      'message', 'method', 'mod', 'new', 'nil', 'not', 'object', 'of',
      'old', 'on', 'operator', 'or', 'overload', 'override',
      'pascal', 'partial', 'private', 'procedure', 'program', 'property',
      'protected', 'public', 'published', 'raise', 'record',
      'register', 'reintroduce', 'repeat', 'require', 'resourcestring',
      'sar', 'sealed', 'set', 'shl', 'shr', 'static', 'strict', 'step',
      'then', 'to', 'try', 'type', 'unit', 'until',
      'uses', 'var', 'virtual', 'while', 'xor'
  );
  cKeywordsPropertyScoped: array [0..4] of UnicodeString = (
      'default', 'index', 'read', 'stored', 'write'
  );
  cKeywordsTypeScoped: array [0..1] of UnicodeString = (
      'enum', 'flag'
  );

function TAnsiStringList.CompareStrings(const S1, S2: string): Integer;
begin
   Result := CompareText(S1, S2);
end;


{ TSynDWSSyn }

constructor TSynDWSSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := True; // bypass automatic lowercase, we handle it here

  FAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  FAsmAttri.Foreground := RGB(128, 0, 0);
  AddAttribute(FAsmAttri);

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Foreground := clGreen;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);

  FDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  FDirecAttri.Foreground := TColor($808000);
  FDirecAttri.Style := [fsItalic];
  AddAttribute(FDirecAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  AddAttribute(FKeyAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FNumberAttri.Foreground := clBlue;
  AddAttribute(FNumberAttri);

  FFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  FFloatAttri.Foreground := clBlue;
  AddAttribute(FFloatAttri);

  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  FHexAttri.Foreground := clBlue;
  AddAttribute(FHexAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Foreground := clBlue;
  AddAttribute(FStringAttri);

  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  FCharAttri.Foreground := clBlue;
  AddAttribute(FCharAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Foreground := clNavy;
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  FKeywords := TAnsiStringList.Create;
  FKeywordsPropertyScoped := TAnsiStringList.Create;
  FKeywordsTypeScoped := TAnsiStringList.Create;

  InitIdent;
  FRange := rsUnknown;
  FAsmStart := False;
  FDefaultFilter := SYNS_FilterDWS;
end;

// Destroy
//
destructor TSynDWSSyn.Destroy;
begin
  inherited;
  FKeywords.Free;
  FKeywordsPropertyScoped.Free;
  FKeywordsTypeScoped.Free;
end;

function TSynDWSSyn.HashKey(Str: PWideChar): Cardinal;
var
  c: Word;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    c := Ord(Str^);
    if c in [Ord('A')..Ord('Z')] then
      c := c + (Ord('a') - Ord('A'));
    Result := Result * 692 + c * 171;
    Inc(Str);
  end;
  FStringLen := Str - FToIdent;
  Result := Result mod Cardinal(Length(FIdentFuncTable));
end;

function TSynDWSSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  FToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(FIdentFuncTable) then
    Result := FIdentFuncTable[Key]
  else
    Result := tkIdentifier;
end;

procedure TSynDWSSyn.InitIdent;

   procedure SetIdentFunc(h : Integer; const func : TIdentFuncTableFunc);
   begin
      FIdentFuncTable[h] := func;
   end;

var
  i : Integer;
begin
  for i := Low(cKeywords) to High(cKeywords) do
  begin
    SetIdentFunc(HashKey(@cKeywords[i][1]), KeywordFunc);
    FKeywords.Add(cKeywords[i]);
  end;

  for i := 0 to High(cKeywordsPropertyScoped) do
  begin
    SetIdentFunc(HashKey(@cKeywordsPropertyScoped[i][1]), FuncPropertyScoped);
    FKeywordsPropertyScoped.Add(cKeywordsPropertyScoped[i]);
  end;

  for i := 0 to High(cKeywordsTypeScoped) do
  begin
    SetIdentFunc(HashKey(@cKeywordsTypeScoped[i][1]), FuncTypeScoped);
    FKeywordsTypeScoped.Add(cKeywordsTypeScoped[i]);
  end;

  for i := Low(FIdentFuncTable) to High(FIdentFuncTable) do
    if @FIdentFuncTable[i] = nil then
      FIdentFuncTable[i] := AltFunc;

  SetIdentFunc(HashKey('asm'), FuncAsm);
  SetIdentFunc(HashKey('end'), FuncEnd);
  SetIdentFunc(HashKey('property'), FuncProperty);
  SetIdentFunc(HashKey('type'), FuncType);

  FKeywords.Sorted := True;
end;

function TSynDWSSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynDWSSyn.KeywordFunc: TtkTokenKind;
var
   buf : String;
begin
   SetString(buf, FToIdent, FStringLen);
   if (FKeywords.IndexOf(buf) >= 0) and (FLine[Run - 1] <> '&') then
      Result := tkKey
   else Result := tkIdentifier
end;

function TSynDWSSyn.FuncAsm: TtkTokenKind;
begin
   if IsCurrentToken('asm') then begin
      Result := tkKey;
      FRange := rsAsm;
      FAsmStart := True;
   end else Result := KeywordFunc;
end;

function TSynDWSSyn.FuncEnd: TtkTokenKind;
begin
  if IsCurrentToken('end') then begin
    if (FLine[Run - 1] <> '&') then
    begin
      Result := tkKey;
      FRange := rsUnknown;
    end
    else
      Result := tkIdentifier;
  end else Result := KeywordFunc;
end;

function TSynDWSSyn.FuncTypeScoped: TtkTokenKind;
var
   buf: String;
begin
  SetString(buf, FToIdent, FStringLen);
  if (FRange = rsType) and (FKeywordsTypeScoped.IndexOf(buf) >= 0) then
    Result := tkKey
  else
    Result := KeywordFunc;
end;

function TSynDWSSyn.FuncType: TtkTokenKind;
begin
  if IsCurrentToken('type') then
  begin
    if (FLine[Run - 1] <> '&') then
    begin
      Result := tkKey;
      FRange := rsType;
    end
    else
      Result := tkIdentifier;
  end else Result := KeywordFunc;
end;

function TSynDWSSyn.FuncPropertyScoped: TtkTokenKind;
var
   buf: String;
begin
  SetString(buf, FToIdent, FStringLen);
  if (FRange = rsProperty) and (FKeywordsPropertyScoped.IndexOf(buf) >= 0) then
    Result := tkKey
  else
    Result := KeywordFunc;
end;

function TSynDWSSyn.FuncProperty: TtkTokenKind;
begin
  if IsCurrentToken('property') then
  begin
    Result := tkKey;
    FRange := rsProperty;
  end
  else
    Result := KeywordFunc;
end;

procedure TSynDWSSyn.AddressOpProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '@' then Inc(Run);
end;

procedure TSynDWSSyn.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '$', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  FTokenID := tkChar;
  Inc(Run);
  if FLine[run]='''' then
      StringAposMultiProc
  else begin
     while IsAsciiChar do
       Inc(Run);
  end;
end;

procedure TSynDWSSyn.BorProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if FRange in [rsDirective, rsDirectiveAsm] then
        FTokenID := tkDirec
      else
        FTokenID := tkComment;
      repeat
        if FLine[Run] = '}' then
        begin
          Inc(Run);
          if FRange in [rsBorAsm, rsDirectiveAsm] then
            FRange := rsAsm
          else
            FRange := rsUnknown;
          break;
        end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynDWSSyn.BraceOpenProc;
begin
  if (FLine[Run + 1] = '$') then
  begin
    if FRange = rsAsm then
      FRange := rsDirectiveAsm
    else
      FRange := rsDirective;
  end
  else
  begin
    if FRange = rsAsm then
      FRange := rsBorAsm
    else
      FRange := rsBor;
  end;
  BorProc;
end;

procedure TSynDWSSyn.ColonOrGreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynDWSSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynDWSSyn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynDWSSyn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkHex;
  while IsIntegerChar do
    Inc(Run);
end;

procedure TSynDWSSyn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynDWSSyn.LoadDelphiStyle;

   procedure AddKeyword(const AName : string);
   var
     I : Integer;
   begin
     I := HashKey( @AName[1] );
     FIdentFuncTable[I] := KeywordFunc;
     FKeywords.Add(AName);
   end;

   procedure RemoveKeyword(const AName : string);
   var
     I : Integer;
   begin
     I := FKeywords.IndexOf(AName);
     if I <> -1 then
       FKeywords.Delete(I);
   end;

const
  clID = clNavy;
  clString = clBlue;
  clComment = clGreen;
  cKeywordsToAdd: array[0..0] of UnicodeString = (
      'string');
  cKeywordsToRemove: array[0..1] of UnicodeString = (
      'break', 'exit');
var
  i : Integer;
begin
  // This routine can be called to install a Delphi style of colors
  // and highlighting. It modifies the basic TSynDWSSyn to reproduce
  // the most recent Delphi editor highlighting.

  // Delphi colors...
  KeyAttri.Foreground := clID;
  StringAttri.Foreground := clString;
  CommentAttri.Foreground := clComment;

  // These are keywords highlighted in Delphi but not in TSynDWSSyn ..
  for i := Low(cKeywordsToAdd) to High(cKeywordsToAdd) do
    AddKeyword(cKeywordsToAdd[i]);

  // These are keywords highlighted in TSynDWSSyn but not in Delphi...
  for i := Low(cKeywordsToRemove) to High(cKeywordsToRemove) do
    RemoveKeyword(cKeywordsToRemove[i]);
end;

procedure TSynDWSSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '=') or (FLine[Run] = '>') then
    Inc(Run);
end;

procedure TSynDWSSyn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynDWSSyn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Inc(Run);
  FTokenID := tkNumber;
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break
        else
          FTokenID := tkFloat;
      'e', 'E': FTokenID := tkFloat;
      '-', '+':
        begin
          if FTokenID <> tkFloat then // arithmetic
            Break;
          if (FLine[Run - 1] <> 'e') and (FLine[Run - 1] <> 'E') then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynDWSSyn.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '.') or (FLine[Run - 1] = ')') then
    Inc(Run);
end;

procedure TSynDWSSyn.AnsiProc;
begin
  case FLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    FTokenID := tkComment;
    repeat
      if (FLine[Run] = '*') and (FLine[Run + 1] = FCommentClose) then begin
        Inc(Run, 2);
        if FRange = rsAnsiAsm then
          FRange := rsAsm
        else
          FRange := rsUnknown;
        break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynDWSSyn.RoundOpenProc;
begin
  Inc(Run);
  case FLine[Run] of
    '*':
      begin
        Inc(Run);
        if FRange = rsAsm then
          FRange := rsAnsiAsm
        else
          FRange := rsAnsi;
        FTokenID := tkComment;
        FCommentClose := ')';
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
    '.':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynDWSSyn.SemicolonProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
  if FRange in [rsProperty, rsExports] then
    FRange := rsUnknown;
end;

procedure TSynDWSSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/': begin
      FTokenID := tkComment;
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
    '*':
      begin
        Inc(Run);
        if FRange = rsAsm then
          FRange := rsAnsiAsm
        else
          FRange := rsAnsi;
        FTokenID := tkComment;
        FCommentClose := '/';
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynDWSSyn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynDWSSyn.StringAposProc;
begin
  FTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = #39 then begin
      Inc(Run);
      if FLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynDWSSyn.StringAposMultiProc;
begin
  FTokenID := tkString;
  if (Run>0) or IsLineEnd(Run+1) then
     Inc(Run);
  FRange := rsHereDocSingle;
  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = '''' then begin
      Inc(Run);
      if FLine[Run] <> '''' then begin
        FRange := rsUnknown;
        break;
      end;
    end;
    Inc(Run);
  end;
end;

procedure TSynDWSSyn.StringQuoteProc;
begin
  FTokenID := tkString;
  if FRange <> rsHereDocDouble then
  begin
    FRange := rsHereDocDouble;
    Inc(Run);
  end else
  begin
    if IsLineEnd(Run) then
    begin
      Inc(Run);
      Exit;
    end;
  end;

  while not IsLineEnd(Run) do
  begin
    if FLine[Run] = '"' then
    begin
      Inc(Run);
      if FLine[Run] <> '"' then
      begin
        FRange := rsUnknown;
        break;
      end;
    end;
    Inc(Run);
  end;
end;

procedure TSynDWSSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynDWSSyn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynDWSSyn.Next;
begin
   FAsmStart := False;
   FTokenPos := Run;
   case FRange of
      rsAnsi, rsAnsiAsm:
         AnsiProc;
      rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
         BorProc;
      rsHereDocSingle:
         StringAposMultiProc;
      rsHereDocDouble:
         StringQuoteProc;
   else
      case FLine[Run] of
         #0: NullProc;
         #10: LFProc;
         #13: CRProc;
         #1..#9, #11, #12, #14..#32: SpaceProc;
         '#': AsciiCharProc;
         '$': IntegerProc;
         #39: StringAposProc;
         '"': StringQuoteProc;
         '0'..'9': NumberProc;
         'A'..'Z', 'a'..'z', '_': IdentProc;
         '{': BraceOpenProc;
         '}', '!', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~': begin
            case FLine[Run] of
               '(': RoundOpenProc;
               '.': PointProc;
               ';': SemicolonProc;
               '/': SlashProc;
               ':', '>': ColonOrGreaterProc;
               '<': LowerProc;
               '@': AddressOpProc;
            else
               SymbolProc;
            end;
         end;
         #$0080..#$FFFF :
            if {$IFDEF SYN_COMPILER_18_UP}Char(FLine[Run]).IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(FLine[Run]){$ENDIF} then
               IdentProc
            else UnknownProc;
      else
         UnknownProc;
      end;
   end;
   inherited;
end;

function TSynDWSSyn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDWSSyn.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynDWSSyn.GetTokenID: TtkTokenKind;
begin
  if not FAsmStart and (FRange = rsAsm)
    and not (FTokenID in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := FTokenID;
end;

function TSynDWSSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := FAsmAttri;
    tkComment: Result := FCommentAttri;
    tkDirec: Result := FDirecAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkFloat: Result := FFloatAttri;
    tkHex: Result := FHexAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkChar: Result := FCharAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynDWSSyn.GetTokenKind: Integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynDWSSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

procedure TSynDWSSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

procedure TSynDWSSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

function TSynDWSSyn.GetSampleSource: UnicodeString;
begin
  Result := '{ Syntax highlighting }'#13#10 +
             'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
             'var'#13#10 +
             '  Number, I, X: Integer;'#13#10 +
             'begin'#13#10 +
             '  Number := 123456;'#13#10 +
             '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
             '  for I := 0 to Number do'#13#10 +
             '  begin'#13#10 +
             '    Inc(X);'#13#10 +
             '    Dec(X);'#13#10 +
             '    X := X + 1.0;'#13#10 +
             '    X := X - $5E;'#13#10 +
             '  end;'#13#10 +
             '  {$R+}'#13#10 +
             '  asm'#13#10 +
             '    mov AX, 1234H'#13#10 +
             '    mov Number, AX'#13#10 +
             '  end;'#13#10 +
             '  {$R-}'#13#10 +
             'end;';
end;


class function TSynDWSSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynDWSSyn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynDWSSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterPascal;
end;

// IsCurrentToken
//
function TSynDWSSyn.IsCurrentToken(const Token: UnicodeString): Boolean;
var
   i : Integer;
   temp : PWideChar;
begin
   temp := FToIdent;
   if Length(Token) = FStringLen then begin
      Result := True;
      for i := 1 to FStringLen do begin
         if     (temp^ <> Token[i])
            and (   (temp^>'z')
                 or (UpCase(temp^)<>UpCase(Token[i])))  then begin
            Result := False;
            break;
         end;
         Inc(temp);
      end;
   end else Result := False;
end;

// IsIdentChar
//
function TSynDWSSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
   if Ord(AChar)<=$7F then
      Result := AnsiChar(AChar) in ['_', '0'..'9', 'A'..'Z', 'a'..'z']
   else
      Result := {$IFDEF SYN_COMPILER_18_UP}AChar.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(AChar){$ENDIF};
end;

class function TSynDWSSyn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPascal;
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynDWSSyn);
{$ENDIF}

end.
