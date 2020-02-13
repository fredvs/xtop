{$mode objfpc}
{$h+}

unit PtoPu;

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    Pascal Pretty-Printer object implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 This unit is based heavily on the code by

 Author:  Peter Grogono
   This program is based on a Pascal pretty-printer written by Ledgard,
   Hueras, and Singer.  See SIGPLAN Notices, Vol. 12, No. 7, July 1977,
   pages 101-105, and PP.DOC/HLP.
   This version of PP developed under Pascal/Z V4.0 or later.
   Very minor modifications for Turbo Pascal made by Willett Kempton
   March 1984 and Oct 84.  Runs under 8-bit Turbo or 16-bit Turbo.
   Toad Hall tweak, rewrite for TP 5, 28 Nov 89

The following was changed :
 - Object oriented
 - Uses streams
 - Run-time customizable.
}
{ $define debug}

interface

uses 
Classes, SysUtils;

const 

  MAXSYMBOLSIZE = 65500;
  MAXSHOWSIZE   = 40;
  MAXSTACKSIZE  = 100;
  MAXKEYLENGTH  = 15;     { The longest keywords are IMPLEMENTATION INITIALIZATION }
  DEFLINESIZE   = 100;
  DEFINDENT     = 2;

type 
  Token    = ansistring;
  FileName = string;

  TTokenScope = (tsInterface, tsImplementation);

  { Keysymbols }
  { If you add keysyms, adjust the definition of lastkey }
  keysymbol =  { keywords }
              (endsym, beginsym, ifsym, thensym, elsesym, procsym, varsym, ofsym,
               whilesym, dosym, casesym, withsym, forsym, repeatsym, untilsym,
               funcsym, labelsym, constsym, typesym, recordsym, stringsym, progsym,
    { TP and Delphi keywords}
               asmsym, trysym, finallysym, exceptsym, raisesym, classsym, objectsym,
               constructorsym, destructorsym, inheritedsym, propertysym,
               privatesym, publicsym, protectedsym, publishedsym,
               initializationsym, finalizationsym,
               inlinesym, librarysym, interfacesym, implementationsym,
               readsym, writesym, unitsym,
    { Not used for formatting }
               andsym, arrsym, divsym, downsym, filesym, gotosym, insym, modsym,
               notsym, nilsym, orsym, setsym, tosym, virtualsym, usessym,
               casevarsym, ofobjectsym,
    { other symbols }
               becomes, notequal, lessorequal, greaterorequal, delphicomment, dopencomment,
               dclosecomment, opencomment, closecomment, semicolon, colon, comma, equals,
               openparen, closeparen, period, endoffile, othersym);

  { Formatting options }
  { If you add options, adjust the definition of lastopt }
  options = (crsupp, crbefore, blinbefore,
             dindonkey, dindent, spbef,
             spaft, gobsym, inbytab, inbyindent, crafter, upper, lower, capital);

  optionset = set of options;
  keysymset = set of keysymbol;

  tableentry = record
    selected: optionset;
    dindsym: keysymset;
    terminators: keysymset
  end;

  { Character identification }

  charname = (letter, digit, space, quote, endofline,
              filemark, otherchar);

  charinfo = record
    Name: charname;
    Value: char
  end;

  symbol = record
    Name: keysymbol;
    Value: Token;
    IsKeyWord: Boolean;
    length, spacesbefore, crsbefore: integer;
  end;

  symbolinfo = ^ symbol;

  stackentry = record
    indentsymbol: keysymbol;
    prevmargin: integer;
  end;

  symbolstack = array [1..MAXSTACKSIZE] of stackentry;

const 
  FirstOpt      = crsupp;
  LastOpt       = capital;  { Adjust this if you add options }
  FirstKey      = endsym;
  LastKey       = othersym; { Adjust this if you add options }
  LastFormatsym = usessym;

type 
  tableptr      = ^tableentry;
  optiontable   = array [Ttokenscope, keysymbol] of tableptr;
  OEntriesTable = array [keysymbol] of string[15];
  ONamesTable   = array [Options] of string[15];
  KeywordTable  = array [endsym..lastFormatsym] of string[MAXKEYLENGTH];
  SpecialChar   = array [1..2] of char;
  dblcharset    = set of endsym..othersym;
  DblCharTable  = array [becomes..dclosecomment] of SpecialChar;
  SglCharTable  = array [opencomment..period] of char;

  TVerboseEvent = procedure (Sender: TObject; const Msg: string) of object;

  { TPrettyPrinter }

  TPrettyPrinter = class(TObject)
    private 
      FTokenScope: TTokenScope;
{$ifdef debug}
      GobbleLevel : Integer;
{$endif debug}
      PreviousSymbol: keysymbol;
      RecordLevel: integer;
      ClassSeen, ObjectSeen: Boolean;
      LastStruct: KeySymbol;
      CRPending: Boolean;
      currchar, nextchar: charinfo;
      currsym, nextsym: symbolinfo;
      inlines, outlines: integer;
      stack: symbolstack;
      top, startpos, currlinepos, currmargin: integer;
      option: OptionTable;
      FOnVerbose: TVerboseEvent;
      FirstWordStackPos, FirstWordPos, FLineSize, FIndent: integer;
      ins, outs, cfgs: TStream;
      procedure Verbose(const Msg: string);
      procedure GetChar;
      procedure StoreNextChar(var lngth: integer; var Value: Token);
      procedure SkipBlanks(Out spacesbefore, crsbefore: integer);
      procedure GetComment(sym: symbolinfo);
      procedure GetDoubleComment(sym: symbolinfo);
      procedure GetDelphiComment(sym: symbolinfo);
      procedure GetNumber(sym: symbolinfo);
      procedure GetCharLiteral(sym: symbolinfo);
      function char_Type: keysymbol;
      procedure GetSpecialChar(sym: symbolinfo);
      procedure GetNextSymbol(sym: symbolinfo);
      procedure GetIdentifier(sym: symbolinfo);
      procedure GetSymbol;
      procedure PopStack(Out indentsymbol: keysymbol; Out prevmargin: integer);
      procedure PushStack(indentsymbol: keysymbol; prevmargin: integer);
      procedure WriteCRs(numberofcrs: integer);
      procedure InsertCR;
      procedure InsertBlankLine;
      procedure LShiftOn(dindsym: keysymset);
      procedure LShift;
      procedure InsertSpace(var symbol: symbolinfo);
      procedure MoveLinePos(newlinepos: integer);
      procedure PrintSymbol;
      procedure PPSymbol;
      procedure Gobble(terminators: keysymset);
      procedure RShift(currmsym: keysymbol);
      procedure RShiftIndent{$ifdef debug}(currmsym: keysymbol){$endif debug};
      function ReadConfigFile: Boolean;
    public 
      constructor Create;
      function PrettyPrint: Boolean;
      property OnVerbose: TVerboseEvent read FOnVerbose write FOnVerbose;
      property LineSize: integer read FLineSize write FLineSize;
      property Indent: integer read FIndent write FIndent;    { How many characters to indent ? }
      property Source: TStream read Ins write Ins;
      property Dest: TStream read OutS write Outs;
      property Config: Tstream read cfgS write cfgs;
      property CurrentScope: TTokenScope read FTokenScope write FTokenScope;
  end;

procedure GenerateCfgFile(S: TStream);

implementation

const 
  Blank = ' ';

var 
  sets: tableptr;
  dblch: dblcharset;

const 
  Keyword: KeywordTable     = 
                              ('END', 'BEGIN', 'IF', 'THEN',
                               'ELSE', 'PROCEDURE', 'VAR', 'OF',
                               'WHILE', 'DO', 'CASE', 'WITH',
                               'FOR', 'REPEAT', 'UNTIL', 'FUNCTION',
                               'LABEL', 'CONST', 'TYPE', 'RECORD',
                               'STRING', 'PROGRAM',
                               'ASM', 'TRY', 'FINALLY', 'EXCEPT', 'RAISE', 'CLASS', 'OBJECT',
                               'CONSTRUCTOR', 'DESTRUCTOR', 'INHERITED', 'PROPERTY',
                               'PRIVATE', 'PUBLIC', 'PROTECTED', 'PUBLISHED',
                               'INITIALIZATION', 'FINALIZATION',
                               'INLINE', 'LIBRARY', 'INTERFACE', 'IMPLEMENTATION',
                               'READ', 'WRITE', 'UNIT',
    {keywords not used for formatting }
                               'AND', 'ARRAY', 'DIV', 'DOWNTO',
                               'FILE', 'GOTO', 'IN', 'MOD',
                               'NOT', 'NIL', 'OR', 'SET', 'TO', 'VIRTUAL', 'USES'
                              );

  EntryNames: OEntriesTable = 
                              ('end', 'begin', 'if', 'then', 'else', 'proc', 'var',
                               'of', 'while', 'do', 'case', 'with', 'for', 'repeat', 'until',
                               'func', 'label', 'const', 'type', 'record', 'string',
                               'prog',
                               'asm', 'try', 'finally', 'except', 'raise', 'class', 'object',
                               'constructor', 'destructor', 'inherited', 'property',
                               'private', 'public', 'protected', 'published',
                               'initialization', 'finalization',
                               'inline', 'library', 'interface', 'implementation',
                               'read', 'write', 'unit',
                               'and', 'arr', 'div', 'down', 'file', 'goto',
                               'in', 'mod', 'not', 'nil', 'or', 'set', 'to', 'virtual', 'uses',
                               'casevar', 'ofobject',
                               'becomes', 'notequal', 'lessorequal', 'greaterorequal',
                               'delphicomment',
                               'dopencomment', 'dclosecomment',
                               'opencomment', 'closecomment', 'semicolon',
                               'colon', 'comma', 'equals',
                               'openparen', 'closeparen', 'period', 'endoffile', 'other');

  OptionNames: ONamesTable  = 
                              ('crsupp', 'crbefore', 'blinbefore',
                               'dindonkey', 'dindent', 'spbef', 'spaft',
                               'gobsym', 'inbytab', 'inbyindent', 'crafter', 'upper',
                               'lower', 'capital');

  DblChar: DblCharTable     = 
                              (':=', '<>', '<=', '>=', '//', '(*', '*)');

  SglChar: SglCharTable     = 
                              ('{', '}', ';', ':', ',', '=', '(', ')', '.');



{ ---------------------------------------------------------------------
    General functions, not part of the object.
  ---------------------------------------------------------------------}

function upperStr(const s: string): string;

var 
  i: longint;
begin
  setLength(upperStr, length(s));
  for i := 1 to length(s) do
    if s[i] in ['a'..'z'] then
      upperStr[i] := char(byte(s[i]) - 32)
    else
      upperStr[i] := s[i];
end;

function LowerStr(const s: string): string;

var 
  i: longint;
begin
  setLength(LowerStr, length(s));
  for i := 1 to length(s) do
    if s[i] in ['A'..'Z'] then
      LowerStr[i] := char(byte(s[i]) + 32)
    else
      LowerStr[i] := s[i];
end;


function IntToStr(I: longint): string;

var 
  s: string;
begin
  str(I, s);
  IntToStr := s;
end;

function StrToInt(const S: string): integer;

var 
  Code: integer;
  Res: integer;
begin
  Val(S, Res, Code);
  StrToInt   := Res;
  if Code <> 0 then
    StrToInt := 0;
end;

procedure Strip(var S: string);

const 
  WhiteSpace = [#32, #9, #10, #13];

var 
  I, J: longint;
begin
  if length(s) = 0 then
    Exit;
  I := 1;
  while (S[I] in whitespace) and (I < Length(S)) do
    Inc(i);
  J := length(S);
  while (S[J] in whitespace) and (J > 1) do
    Dec(j);
  if I <= J then
    S := Copy(S, i, j - i + 1)
  else
    S := '';
end;

procedure ClassID(Value: Token; lngth: integer; var idtype: keysymbol; var IsKeyWord: Boolean);

{ Classify an identifier.  We are only interested
    in it if it is a keyword, so we use the hash table. }

var 
  Keyvalue: string[MAXKEYLENGTH];
  Sym: keysymbol;
begin
  if lngth > MAXKEYLENGTH then
    begin
      idtype    := othersym;
      IsKeyWord := False;
    end
  else
    begin
      IsKeyWord := False;
      KeyValue  := UpperStr(Value);
      sym       := endsym;
      while (not IsKeyword) and (sym <= lastformatsym) do
        begin
          iskeyword := (KeyValue = Keyword[sym]);
          if not iskeyword then
            Sym     := Succ(sym);
        end;
      if IsKeyWord then
        idtype := sym
      else
        idtype := othersym;
    end;
end; { of ClassID }

{ ---------------------------------------------------------------------
    Functions to create options and set defaults.
  ---------------------------------------------------------------------}

procedure CreateOptions(Out Option: OptionTable);

var 
  Sym: KeySymbol;
  T: TTokenScope;
begin
  for sym := endsym to othersym do
    for T := Low(TTokenScope) to High(TTokenScope) do
      begin
        new(option[T, sym]);
        option[T, sym]^.selected    := [];
        option[T, sym]^.dindsym     := [];
        option[T, sym]^.terminators := [];
      end;
end;

procedure SetTerminators(var Option: OptionTable);

var 
  T: TTokenScope;
begin
  for T := Low(TTokenScope) to High(TTokenScope) do
    begin
      option[t, casesym]^.terminators    := [ofsym];
      option[t, casevarsym]^.terminators := [ofsym];
      option[t, forsym]^.terminators     := [dosym];
      option[t, whilesym]^.terminators   := [dosym];
      option[t, withsym]^.terminators    := [dosym];
      option[t, ifsym]^.terminators      := [thensym];
      option[t, untilsym]^.terminators   := [endsym, untilsym, elsesym, semicolon];
      option[t, becomes]^.terminators    := [endsym, untilsym, elsesym, semicolon];
      option[t, openparen]^.terminators  := [closeparen];
      option[t, usessym]^.terminators    := [semicolon];
    end;
end;

procedure SetDefaultIndents(var Option: OptionTable);

var 
  T: TTokenScope;
begin
  for T := Low(TTokenScope) to High(TTokenScope) do
    begin
      option[t, recordsym]^.dindsym    := [endsym];
      option[t, funcsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
      option[t, procsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
      option[t, constsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
      option[t, typesym]^.dindsym      := [labelsym, constsym, typesym, varsym];
      option[t, varsym]^.dindsym       := [labelsym, constsym, typesym, varsym];
      option[t, beginsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
      option[t, publicsym]^.dindsym    := [endsym, protectedsym, privatesym, publicsym, publishedsym
                                          ];
      option[t, privatesym]^.dindsym   := [endsym, protectedsym, privatesym, publicsym, publishedsym
                                          ];
      option[t, protectedsym]^.dindsym := [endsym, protectedsym, privatesym, publicsym, publishedsym
                                          ];
      option[t, publishedsym]^.dindsym := [endsym, protectedsym, privatesym, publicsym, publishedsym
                                          ];
      option[t, finallysym]^.dindsym   := [trysym];
      option[t, exceptsym]^.dindsym    := [trysym];
      option[t, elsesym]^.dindsym      := [ifsym, thensym, elsesym];
      option[t, untilsym]^.dindsym     := [ifsym, thensym, elsesym, forsym, whilesym,
                                          withsym, colon, equals];
      option[t, endsym]^.dindsym       := [ifsym, thensym, elsesym, forsym, whilesym,
                                          withsym, casevarsym, colon, equals, recordsym,
                                          trysym, classsym, objectsym, protectedsym, privatesym,
                                          publicsym, publishedsym, finallysym, exceptsym];
      option[t, semicolon]^.dindsym    := [ifsym, thensym, elsesym, forsym,
                                          whilesym, withsym, colon, equals];
      option[t, comma]^.dindsym        := [ifsym, thensym, elsesym, forsym, whilesym,
                                          withsym, casevarsym, colon, equals, recordsym,
                                          trysym, classsym, objectsym, protectedsym, privatesym,
                                          publicsym, publishedsym, finallysym, exceptsym];
      option[t, implementationsym]^.dindsym := [labelsym, varsym, typesym, constsym,
                                               endsym, propertysym];
    end;
end;

procedure SetDefaults(var Option: OptionTable);

{ Sets default values for the formatting rules. }

var 
  T: TTokenScope;
begin
  for T := Low(TTokenScope) to High(TTokenScope) do
    begin
      option[t, progsym]^.selected       := [capital, blinbefore, spaft];
      option[t, unitsym]^.selected       := [capital, blinbefore, spaft];
      option[t, librarysym]^.selected    := [capital, blinbefore, spaft];
      option[t, funcsym]^.selected       := [capital, blinbefore, dindonkey, spaft];
      option[t, procsym]^.selected       := [capital, blinbefore, dindonkey, spaft];
      option[t, labelsym]^.selected      := [capital, blinbefore, spaft, inbytab];
      option[t, constsym]^.selected      := [capital, blinbefore, dindonkey, spaft, inbytab];
      option[t, typesym]^.selected       := [capital, blinbefore, dindonkey, spaft, inbytab];
      option[t, varsym]^.selected        := [capital, blinbefore, dindonkey, spaft, inbytab];
      option[t, beginsym]^.selected      := [capital, dindonkey, crbefore, crafter, inbytab];
      option[t, repeatsym]^.selected     := [capital, inbytab, crafter];
      option[t, recordsym]^.selected     := [capital, inbyIndent, crafter];
      option[t, objectsym]^.selected     := [capital, inbyIndent];
      option[t, classsym]^.selected      := [capital, inbyIndent];
      option[t, publicsym]^.selected     := [capital, crbefore, dindonkey, spaft, inbytab];
      option[t, publishedsym]^.selected  := [capital, crbefore, dindonkey, spaft, inbytab];
      option[t, protectedsym]^.selected  := [capital, crbefore, dindonkey, spaft, inbytab];
      option[t, privatesym]^.selected    := [capital, crbefore, dindonkey, spaft, inbytab];
      option[t, trysym]^.Selected        := [capital, crbefore, crafter, inbytab];
      option[t, finallysym]^.selected    := [capital, crbefore, dindent, crafter, inbytab];
      option[t, exceptsym]^.selected     := [capital, crbefore, dindent, crafter, inbytab];
      option[t, casesym]^.selected       := [capital, spaft, inbytab, gobsym, crafter];
      option[t, casevarsym]^.selected    := [capital, spaft, inbytab, gobsym, crafter];
      option[t, ofsym]^.selected         := [capital, crsupp, spbef, spaft];
      option[t, forsym]^.selected        := [capital, spaft, inbytab, gobsym, crafter];
      option[t, whilesym]^.selected      := [capital, spaft, inbytab, gobsym, crafter];
      option[t, withsym]^.selected       := [capital, spaft, inbytab, gobsym, crafter];
      option[t, dosym]^.selected         := [capital, crsupp, spbef];
      option[t, ifsym]^.selected         := [capital, spaft, inbytab, gobsym];
      option[t, implementationsym]^.selected := [capital, blinbefore, crafter, dindonkey];
      option[t, interfacesym]^.selected  := [capital, blinbefore, crafter];
      option[t, usessym]^.selected       := [capital, blinbefore, spaft];
      option[t, thensym]^.selected       := [capital];
      option[t, elsesym]^.selected       := [capital, crbefore, dindonkey, inbytab];
      option[t, endsym]^.selected        := [capital, crbefore, crafter, dindonkey, dindent];
      option[t, untilsym]^.selected      := [capital, crbefore, dindonkey, dindent, spaft,
                                            gobsym, crafter];
      option[t, becomes]^.selected       := [capital, spbef, spaft, gobsym];
      option[t, Delphicomment]^.Selected := [crafter];
      option[t, opencomment]^.selected   := [capital, crsupp];
      option[t, closecomment]^.selected  := [capital, crsupp];
      option[t, semicolon]^.selected     := [capital, crsupp, dindonkey, crafter];
      option[t, colon]^.selected         := [capital, inbytab];
      option[t, comma]^.selected         := [spaft];
      option[t, equals]^.selected        := [capital, spbef, spaft, inbytab];
      option[t, openparen]^.selected     := [capital, gobsym];
      option[t, period]^.selected        := [capital, crsupp];
    end;
  option[tsInterface, funcsym]^.selected := [capital, dindonkey, spaft];
  option[tsInterface, procsym]^.selected := [capital, dindonkey, spaft];
end;



{ ---------------------------------------------------------------------
    Stream handling routines
  ---------------------------------------------------------------------}

function ReadChar(S: TStream): char;

var 
  C: char;
begin
  repeat
    if S.Position = S.Size then
      C := #0
    else
      S.read(C, 1);
  until (C <> #13);
  ReadChar := C;
end;

function EoSLn(S: TStream): char;

const 
  WhiteSpace = [' ', #9, #13];

var 
  C: char;
begin
  repeat
    if S.Position = S.Size then
      C := #0
    else
      S.read(C, 1);
  until (not (C in WhiteSpace)) or ((C = #10));
  EoSln := C;
end;

function ReadString(S: TStream): string;

var 
  I: byte;
  Count: integer;
begin
  Result := '';
  I      := 0;
  repeat
    if ((I + 1) > Length(Result)) then
      SetLength(Result, Length(Result) + 255);
    Count := S.read(Result[I + 1], 1);
    if Count > 0 then
      Inc(I);
  until (Result[I] = #10) or (Count = 0);
  if Result[i] = #10 then
    Dec(I);
  if Result[I] = #13 then
    Dec(I);
  SetLength(Result, I);
end;

procedure WriteString(S: TStream; ST: string);
begin
  S.write(St[1], length(St));
end;

procedure WriteAnsiString(S: TStream; ST: ansistring);
begin
  S.write(St[1], length(St));
end;


procedure WriteCR(S: TStream);

const 
  Newline = System.LineEnding;
begin
  WriteString(S, Newline);
end;


procedure WriteLnString(S: TStream; ST: string);
begin
  WriteString(S, ST);
  WriteCR(S);
end;



{ ---------------------------------------------------------------------
    TPrettyPrinter object
  ---------------------------------------------------------------------}

procedure TPrettyPrinter.Verbose(const Msg: string);
begin
  if Assigned(FOnVerbose) then
    FOnVerbose(Self, Msg);
end;

procedure TPrettyPrinter.GetChar;
{ Read the next character and classify it }

var 
  Ch: char;
begin
  currchar := nextchar;
  with nextchar do
    begin
      Ch := ReadCHar(Ins);
      if Ch = #0 then
        begin
          Name  := filemark;
          Value := Blank;
        end
      else if (Ch = #10) then
             begin
               Name  := endofline;
               Value := Ch;
               Inc(inlines);
             end
      else
        begin
          Value  := Ch;
          if Ch in ['a'..'z', 'A'..'Z', '_'] then
            Name := letter
          else if Ch in ['0'..'9'] then
                 Name := digit
          else if Ch = '''' then
                 Name := quote
          else if Ch in [#13, ' ', #9] then
                 Name := space
          else
            Name := otherchar;
        end;
    end;
end; { of GetChar }

procedure TPrettyPrinter.StoreNextChar(var lngth: integer; var Value: Token);
     { Store a character in the current symbol }
begin
  GetChar;
  if lngth < MAXSYMBOLSIZE then
    begin {XXX - should there be a limit at all?}
      Inc(lngth);
      setlength(Value, lngth);
      Value[lngth] := currchar.Value;
    end;
end; { of StoreNextChar }

procedure TPrettyPrinter.SkipBlanks(out spacesbefore, crsbefore: integer);
     { Count the spaces between symbols }
begin
  spacesbefore := 0;
  crsbefore    := 0;
  while nextchar.Name in [space, endofline] do
    begin
      GetChar;
      case currchar.Name of 
        space: Inc(spacesbefore);
        endofline:
                   begin
                     Inc(crsbefore);
                     spacesbefore := 0;
                   end;
      end;  {case}
    end;
end;      { of SkipBlanks }

procedure TPrettyPrinter.GetComment(sym: symbolinfo);
          { Process comments using brace notation }
begin
  sym^.Name := opencomment;
  while not ((currchar.Value = '}') or (nextchar.Name = filemark)) do
    StoreNextChar(sym^.length, sym^.Value);
  if currchar.Value = '}' then
    sym^.Name := closecomment;
end; { of GetCommment }

procedure TPrettyPrinter.GetDoubleComment(sym: symbolinfo);
     { Process comments using parenthesis notation }
begin
  sym^.Name := dopencomment;
  while not (((currchar.Value = '*') and (nextchar.Value = ')')) or (nextchar.Name = filemark)) do
    StoreNextChar(sym^.length, sym^.Value);
  if (currchar.Value = '*') and (nextchar.Value = ')') then
    begin
      StoreNextChar(sym^.length, sym^.Value);
      sym^.Name := dclosecomment;
    end;
end; { of GetDoubleCommment }

procedure TPrettyPrinter.GetDelphiComment(sym: symbolinfo);
     { Process comments using either brace or parenthesis notation }
begin
  sym^.Name := Delphicomment;
  while not ((nextchar.Name = endofline) or (nextchar.Name = filemark)) do
    StoreNextChar(sym^.length, sym^.Value);

end; { of GetDelphiCommment }

procedure TPrettyPrinter.GetIdentifier(sym: symbolinfo);
     { Read an identifier and classify it }
begin
  while nextchar.Name in [letter, digit] do
    StoreNextChar(sym^.length, sym^.Value);
  ClassID(sym^.Value, sym^.length, sym^.Name, sym^.IsKeyWord);
  if sym^.Name in [recordsym, objectsym, classsym, casesym, endsym] then
    begin
      if sym^.Name = implementationsym then
        FTokenScope := tsImplementation;
      if sym^.Name in [recordsym, objectsym, classsym] then
        LastStruct  := sym^.Name;
      case sym^.Name of 
        RecordSym: Inc(RecordLevel);
        ClassSym: ClassSeen := True;
        objectsym:
                   if (PreviousSymbol = Ofsym) then
                     sym^.Name       := ofobjectsym
                   else
                     ObjectSeen      := True;
        casesym: if (RecordLevel > 0) and (LastStruct = recordsym) then
                   sym^.Name       := casevarsym;
        endsym: if (LastStruct = recordsym) then
                  Dec(Recordlevel);
        else
          begin
            ClassSeen  := False;
            ObjectSeen := False;
          end
      end;  {case}
    end;
  if (PreviousSymbol = ClassSym) and (sym^.Name = ofsym) then
    ClassSeen    := False;
  PreviousSymbol := sym^.Name;
end; { of GetIdentifier }

     { Read a number and store it as a string }
procedure TPrettyPrinter.GetNumber(sym: symbolinfo);
begin
  while nextchar.Name = digit do
    StoreNextChar(sym^.length, sym^.Value);
  sym^.Name := othersym;
end; { of GetNumber }

procedure TPrettyPrinter.GetCharLiteral(sym: symbolinfo);
     { Read a quoted string }
begin
  while nextchar.Name = quote do
    begin
      StoreNextChar(sym^.length, sym^.Value);
      while not (nextchar.Name in [quote, endofline, filemark]) do
        StoreNextChar(sym^.length, sym^.Value);
      if nextchar.Name = quote then
        StoreNextChar(sym^.length, sym^.Value);
    end;
  sym^.Name := othersym;
end; { of GetCharLiteral }

function TPrettyPrinter.char_Type: keysymbol;
     { Classify a character pair }

var 
  NextTwoChars: SpecialChar;
  Hit: Boolean;
  thischar: keysymbol;
begin
  NextTwoChars[1] := currchar.Value;
  NextTwoChars[2] := nextchar.Value;
  thischar := becomes;
  Hit := False;
  while not (Hit or (thischar = opencomment)) do
    if NextTwoChars = DblChar[thischar] then
      Hit := True
    else
      Inc(thischar);
  if not Hit then
    begin
      thischar := opencomment;
      while not (Hit or (PRED(thischar) = period)) do
        if currchar.Value = SglChar[thischar] then
          Hit := True
        else
          Inc(thischar);
    end;
  if Hit then
    char_Type := thischar
  else
    char_Type := othersym;
end; { of char_Type }

procedure TPrettyPrinter.GetSpecialChar(sym: symbolinfo);
     { Read special characters }
begin
  StoreNextChar(sym^.length, sym^.Value);
  sym^.Name := char_Type;
  if sym^.Name in dblch then
    StoreNextChar(sym^.length, sym^.Value);
end; { of GetSpecialChar }

procedure TPrettyPrinter.GetNextSymbol(sym: symbolinfo);
     { Read a symbol using the appropriate procedure }
begin
  case nextchar.Name of 
    letter: GetIdentifier(sym);
    digit: GetNumber(sym);
    quote: GetCharLiteral(sym);
    otherchar:
               begin
                 GetSpecialChar(sym);
                 if sym^.Name = opencomment then
                   GetComment(sym)
                 else if sym^.Name = dopencomment then
                        GetDoubleComment(sym)
                 else if sym^.Name = DelphiComment then
                        GetDelphiComment(Sym);
               end;
    filemark: sym^.Name := endoffile;
    else {:} {Turbo}
      WRITELN('Unknown character type: ', Ord(nextchar.Name));
  end;       {case}
end; { of GetNextSymbol }

procedure TprettyPrinter.GetSymbol;
{ Store the next symbol in NEXTSYM }

var 
  dummy: symbolinfo;
begin
  dummy           := currsym;
  currsym         := nextsym;
  nextsym         := dummy;
  SkipBlanks(nextsym^.spacesbefore, nextsym^.crsbefore);
  nextsym^.length := 0;
  nextsym^.IsKeyWord := False;
  if currsym^.Name = opencomment then
    GetComment(nextsym)
  else if currsym^.Name = dopencomment then
         GetDoubleComment(nextsym)
  else
    GetNextSymbol(nextsym);
end;  {of GetSymbol}

procedure TprettyPrinter.PopStack(Out indentsymbol: keysymbol; Out prevmargin: integer);
      { Manage stack of indentation symbols and margins }
begin
  if top > 0 then
    begin
      indentsymbol := stack[top].indentsymbol;
      prevmargin   := stack[top].prevmargin;
      Dec(top);
    end
  else
    begin
      indentsymbol := othersym;
      prevmargin   := 0;
    end;
end; { of PopStack }


procedure TPrettyPrinter.PushStack(indentsymbol: keysymbol; prevmargin: integer);
begin
  Inc(top);
  stack[top].indentsymbol := indentsymbol;
  stack[top].prevmargin   := prevmargin;
end; { of PushStack }

procedure TPrettyPrinter.WriteCRs(numberofcrs: integer);

var 
  i: integer;
begin
  if numberofcrs > 0 then
    begin
      for i := 1 to numberofcrs do
        WriteCr(OutS);
      Inc(outlines, numberofcrs);
      Currlinepos       := 0;
      FirstWordStackPos := -1;
    end;
end; { of WriteCRs }


procedure TPrettyPrinter.InsertCR;
begin
  if currsym^.crsbefore = 0 then
    begin
      WriteCRs(1);
      currsym^.spacesbefore := 0;
    end;
end; { of InsertCR }

procedure TPrettyPrinter.InsertBlankLine;
begin
  if currsym^.crsbefore = 0 then
    begin
      if currlinepos = 0 then
        WriteCRs(1)
      else
        WriteCRs(2);
      currsym^.spacesbefore := 0;
    end
  else if currsym^.crsbefore = 1 then
         if currlinepos > 0 then
           begin
             WriteCRs(1);
             currsym^.spacesbefore := 0;
           end;
end; { of InsertBlankLine }

procedure TPrettyPrinter.LShiftOn(dindsym: keysymset);
     { Move margin left according to stack configuration and current symbol }

var 
  indentsymbol: keysymbol;
  prevmargin: integer;
begin
{$ifdef debug}
  write('LShiftOn ',EntryNames[currsym^.name],' : ',FirstWordPos,'/',CurrMargin);
{$endif debug}
  if top > 0 then
    begin
      repeat
        PopStack(indentsymbol, prevmargin);
        if indentsymbol in dindsym then
          currmargin := prevmargin;
      until not (indentsymbol in dindsym) or (top = 0);
      if not (indentsymbol in dindsym) then
        PushStack(indentsymbol, prevmargin);
    end;
{$ifdef debug}
  Writeln('-> ',CurrMargin);
{$endif debug}
end; { of LShiftOn }

procedure TprettyPrinter.LShift;
     { Move margin left according to stack top }

var 
  indentsymbol: keysymbol;
  prevmargin: integer;
begin
{$ifdef debug}
  write('LShift ',EntryNames[currsym^.name],' : ',FirstWordPos,'/',CurrMargin);
{$endif debug}
  if top > 0 then
    begin
      PopStack(indentsymbol, prevmargin);
      currmargin := prevmargin;
     (* maybe PopStack(indentsymbol,currmargin); *)
    end;
{$ifdef debug}
  Writeln('-> ',CurrMargin);
{$endif debug}
end; { of LShift }

procedure TprettyPrinter.RShift(currmsym: keysymbol);
     { Move right, stacking margin positions }
begin
{$ifdef debug}
  write('RShift ',EntryNames[currmsym],' : ',FirstWordPos,'/',Currmargin);
{$endif debug}
  if top < MAXSTACKSIZE then
    PushStack(currmsym, currmargin);
  if startpos > currmargin then
    currmargin := startpos;
  Inc(currmargin, INDENT);
{$ifdef debug}
  Writeln(' -> ',Currmargin)
{$endif debug}
end; { of RShift }

procedure TprettyPrinter.RShiftIndent{$ifdef debug}(currmsym: keysymbol){$endif debug};
     { Move right, stacking margin positions }
begin
{$ifdef debug}
  write('RShiftIndent ',EntryNames[currmsym],' : ',FirstWordPos,'/',Currmargin);
{$endif debug}
  if (FirstWordStackPos >= 0) then
    Top := FirstWordStackPos
  else
    Top := 0;
{$ifdef debug}
  if (Top>0) then
    write(' Stackpos ',Top,' Item: ',EntryNames[Stack[Top].IndentSymbol],' Pos: ',Stack[Top].
          Prevmargin)
  else
    write(' no item on stack');
{$endif debug}
  if top < MAXSTACKSIZE then
    PushStack(othersym, FirstWordPos);
  //    IF top < MAXSTACKSIZE THEN PushStack(currmsym, currmargin);
  CurrMargin := FirstWordPos + Indent;
{$ifdef debug}
  Writeln(' -> ',Currmargin)
{$endif debug}
end; { of RShift }

procedure TPrettyPrinter.InsertSpace(var symbol: symbolinfo);
     { Insert space if room on line }
begin
  if currlinepos < LineSize then
    begin
      WriteString(OutS, Blank);
      Inc(currlinepos);
      if (symbol^.crsbefore = 0) and (symbol^.spacesbefore > 0) then
        Dec(symbol^.spacesbefore);
    end;
end; { of InsertSpace }

procedure TPrettyPrinter.MoveLinePos(newlinepos: integer);
     { Insert spaces until correct line position reached }

var 
  i: integer;
begin
  for i := SUCC(currlinepos) to newlinepos do
    WriteString(OutS, Blank);
  currlinepos := newlinepos;
end; { of MoveLinePos }

procedure TPrettyPrinter.PrintSymbol;
begin
  if (currsym^.IsKeyWord) then
    begin
      if upper in sets^.selected then
        WriteString(OutS, UpperStr(currsym^.Value))
      else if lower in sets^.selected then
             WriteString(OutS, LowerStr(currsym^.Value))
      else if capital in sets^.selected then
             begin
               WriteString(OutS, UpCase(CurrSym^.Value[1]));
               WriteString(OutS, LowerStr(Copy(CurrSym^.Value, 2, MAXSYMBOLSIZE)));
      {XXX - ?should it be length?}
             end
      else
        WriteString(OutS, Currsym^.Value);
    end
  else
    WriteAnsiString(OutS, currsym^.Value);
  startpos := currlinepos;
  Inc(currlinepos, currsym^.length);
  if (FirstWordStackPos = -1) then
    begin
      FirstWordPos      := startpos;
      FirstWordStackPos := Top;
{$ifdef debug}
      write('First word : ',currlinepos,': ',currsym^.value);
      if (FirstWordStackPos>0) then
        writeln(' [Stack: ',FirstWordStackPos,' Item: "',EntryNames[Stack[FirstWordStackPos].
                IndentSymbol],'" Pos: ',Stack[FirstWordStackPos].Prevmargin,']')
      else
        Writeln(' No stack')
{$endif debug}
    end;
end; { of PrintSymbol }

procedure TPrettyPrinter.PPSymbol;
     { Find position for symbol and then print it }

var 
  newlinepos: integer;
begin
  WriteCRs(currsym^.crsbefore);
  if ((currLinePos <> 0) and (currlinepos + currsym^.spacesbefore > currmargin)) or
     (currsym^.Name in [opencomment, closecomment, dopencomment, dclosecomment]) then
    newlinepos := currlinepos + currsym^.spacesbefore
  else
    newlinepos := currmargin;
  if newlinepos + currsym^.length > LINESIZE then
    begin {XXX - this needs to be cleaned for case of long symbol values}
      WriteCRs(1);
      if currmargin + currsym^.length <= LINESIZE then
        newlinepos := currmargin
      else if currsym^.length < LINESIZE then
             newlinepos := LINESIZE - currsym^.length
      else
        newlinepos := 0;
    end;
  MoveLinePos(newlinepos);
  PrintSymbol;
end; { of PPSymbol }

procedure TPrettyPrinter.Gobble(terminators: keysymset);
  { Print symbols which follow a formatting symbol but which do not
    affect layout }
begin
{$ifdef debug}
  Inc(GobbleLevel);
  Writeln('Gobble start ',GobbleLevel,' : ',EntryNames[currsym^.name]);
{$endif debug}
  if top < MAXSTACKSIZE then
    PushStack(currsym^.Name, currmargin);
  currmargin := currlinepos;
  while not ((nextsym^.Name in terminators) or (nextsym^.Name = endoffile)) do
    begin
      GetSymbol;
      PPSymbol;
    end;
  LShift;
{$ifdef debug}
  Writeln('Gobble end ',gobblelevel,' : ',EntryNames[nextsym^.name],' ',nextsym^.name in terminators
  );
  Dec(GobbleLevel);
{$endif debug}
end; { of Gobble }

function TPrettyPrinter.ReadConfigFile: Boolean;

type 
  TLineType = (ltNormal, ltIndent, ltGobble);

var 
  I, J: longint;

procedure SetOption(TheKey: KeySymbol; var OptionList: string);

var 
  TheOpt: Options;
  Found: Boolean;
  K: longint;
  opt: string;
  TS: TTokenScope;
begin
  repeat
    K := pos(',', optionlist);
    if k > 0 then
      begin
        opt := Copy(OptionList, 1, k - 1);
        strip(opt);
        Delete(OptionList, 1, k);
      end
    else
      opt := OptionList;
    if Length(Opt) > 0 then
      begin
        Found      := False;
        for TheOpt := firstopt to lastopt do
          begin
            found    := opt = OptionNames[Theopt];
            if found then
              break;
          end;
        if not found then
          Verbose('Unknown option on line ' + IntToStr(i) + ': ' + Opt)
        else
          for TS := Low(TTokenScope) to High(TTokenScope) do
            Option[TS, TheKey]^.Selected := Option[TS, TheKey]^.Selected + [TheOpt];
      end;
  until k = 0;
end;

function GetKeySimList(const aType: string; var OptionList: string): keysymset;

var 
  TheIndent: Keysymbol;
  Found: Boolean;
  K: longint;
  opt: string;
begin
  Result := [];
  repeat
    K    := pos(',', optionlist);
    if k > 0 then
      begin
        opt := Copy(OptionList, 1, k - 1);
        strip(opt);
        Delete(OptionList, 1, k);
      end
    else
      opt := OptionList;
    if Length(Opt) > 0 then
      begin
        Found         := False;
        for TheIndent := firstKey to lastKey do
          begin
            found       := opt = EntryNames[Theindent];
            if found then
              break;
          end;
        if not found then
          begin
            Verbose('Unknown indent ' + aType + ' on line ' + IntToStr(i) + ': ' + Opt);
            Exit;
          end;
        Include(Result, Theindent);
      end;
  until k = 0;
end;

procedure SetIndent(TheKey: KeySymbol; var OptionList: string);

var 
  TS: TTokenScope;
  Syms: KeySymSet;
begin
  Syms   := GetKeySimList('indent', OptionList);
  for TS := Low(TTokenScope) to High(TTokenScope) do
    with Option[TS, TheKey]^ do
      dindsym := dindsym + Syms;
end;

procedure SetGobble(TheKey: KeySymbol; var OptionList: string);

var 
  TS: TTokenScope;
  Syms: KeySymSet;
begin
  Syms   := GetKeySimList('gobble', OptionList);
  for TS := Low(TTokenScope) to High(TTokenScope) do
    with Option[TS, TheKey]^ do
      Terminators := Terminators + Syms;
end;

function CheckLineType(var Name: string): TLineType;
begin
  if (Name[1] = '[') and (Name[Length(Name)] = ']') then
    begin
      Name   := Copy(Name, 2, Length(Name) - 2);
      Result := ltIndent;
    end
  else if (Name[1] = '<') and (Name[Length(Name)] = '>') then
         begin
           Name   := Copy(Name, 2, Length(Name) - 2);
           Result := ltgobble;
         end
  else
    Result := ltNormal;
end;

var 
  TheKey: KeySymbol;
  Found: Boolean;
  Line, Name: string;
  L: TStringList;
  LT: TLineType;
begin
  ReadConfigFile := False;
  L := TStringList.Create;
  try
    L.LoadFromStream(CfgS);
    for I := 1 to L.Count do
      begin
        Line   := L[i - 1];
      { Strip comment }
        if pos('#', Line) <> 0 then
          Line := Copy(Line, 1, Pos('#', Line) - 1);
        if length(Line) <> 0 then
          begin
            J := Pos('=', Line);
            if J = 0 then
              verbose('Error in config file on line ' + IntToStr(i))
            else
              begin
                Line  := LowerStr(Line);
                Name  := Copy(Line, 1, j - 1);
                Delete(Line, 1, J);
          { indents or options ? }
                LT    := CheckLineType(Name);
                Strip(Name);
                found := False;
                for thekey := firstkey to lastkey do
                  begin
                    found := Name = EntryNames[thekey];
                    if Found then
                      break;
                  end;
                if not found then
                  Verbose('Unknown keyword on line ' + IntToStr(i) + ': ' + Name)
                else
                  case LT of 
                    ltIndent: SetIndent(TheKey, Line);
                    ltNormal: SetOption(TheKey, Line);
                    ltGobble: SetGobble(TheKey, Line);
                  end;
              end;
          end;
      end;
  finally
    L.Free;
end;
Verbose('Processed configfile: read ' + IntToStr(I) + ' lines');
ReadConfigFile := True;
end;

procedure GenerateCfgFile(S: TStream);

var 
  TheKey, TheIndent: KeySymbol;
  TheOpt: Options;
  Written: Boolean;
  Option: OptionTable;
begin
  CreateOptions(option);
  SetDefaults(option);
  SetDefaultIndents(option);
  for TheKey := Firstkey to lastkey do
    begin
    { Write options }
      WriteString(S, EntryNames[TheKey] + '=');
      Written    := False;
      for TheOpt := FirstOpt to LastOpt do
        if TheOpt in Option[tsInterface, TheKey]^.Selected then
          begin
            if written then
              WriteString(S, ',')
            else
              Written := True;
            writeString(S, OptionNames[TheOpt]);
          end;
      WriteCr(S);
    { Write de-indent keysyms, if any }
      if Option[tsInterface, TheKey]^.dindsym <> [] then
        begin
          WriteString(S, '[' + EntryNames[TheKey] + ']=');
          Written       := False;
          for TheIndent := FirstKey to lastkey do
            if TheIndent in Option[tsInterface, TheKey]^.dindsym then
              begin
                if written then
                  WriteString(S, ',')
                else
                  Written := True;
                WriteString(S, EntryNames[Theindent]);
              end;
          WriteCr(S);
        end;
    end;
end;

function trimMiddle(a: ansistring; lnght: integer; size: integer): string;

var 
  half: integer;
begin
  if lnght > size then
    begin
      half       := (size - 3) div 2;
      trimMiddle := copy(a, 1, half) + '...' + copy(a, lnght - half + 1, half);
    end
  else
    trimMiddle := a;
end;

function TPrettyPrinter.PrettyPrint: Boolean;
begin
  PrettyPrint := False;
  if not Assigned(Ins) or not Assigned(OutS) then
    Exit;
  if not Assigned(CfgS) then
    begin
      SetDefaults(Option);
      SetDefaultIndents(Option);
    end
  else
    ReadConfigFile;
  { Initialize variables }
  top         := 0;
  currlinepos := 0;
  currmargin  := 0;
  inlines     := 0;
  outlines    := 0;
  CrPending   := False;
  FirstWordStackPos := -1;
  RecordLevel := 0;
  GetChar;
  NEW(currsym);
  NEW(nextsym);
  GetSymbol;
  while nextsym^.Name <> endoffile do
    begin
      GetSymbol;
{$ifdef debug}
      Writeln('line in-'+IntToStr(inlines)+' out-'+IntToStr(outlines)+
      ' symbol "'+EntryNames[currsym^.name]+'" = "'+
      trimMiddle(currsym^.value,length(currsym^.value), MAXSHOWSIZE)+'"');
{$endif debug}
      sets := option[FTokenScope, currsym^.Name];
      if (CrPending and not (crsupp in sets^.selected)) or (crbefore in sets^.selected) then
        begin
          InsertCR;
          CrPending := False;
        end;
      if blinbefore in sets^.selected then
        begin
          InsertBlankLine;
          CrPending := False;
        end;
      if dindonkey in sets^.selected then
        LShiftOn(sets^.dindsym);
      if dindent in sets^.selected then
        LShift;
      if spbef in sets^.selected then
        InsertSpace(currsym);
      PPSymbol;
      if spaft in sets^.selected then
        InsertSpace(nextsym);
      if inbytab in sets^.selected then
        RShift(currsym^.Name)
      else if inbyindent in sets^.selected then
             RShiftIndent
{$ifdef debug}(currsym^.name){$endif debug}
      ;
      if gobsym in sets^.selected then
        Gobble(sets^.terminators);
      if crafter in sets^.selected then
        CrPending := True;
    end;
  if CrPending then
    WriteCRs(1);
  Verbose(IntToStr(inlines) + ' lines read, ' + IntToStr(outlines) + ' lines written.');
  PrettyPrint := True;
end;

constructor TPrettyPrinter.Create;
begin
  Indent   := DefIndent;
  LineSize := DefLineSize;
  CreateOptions(Option);
  SetTerminators(Option);
  InS      := nil;
  OutS     := nil;
  CfgS     := nil;
end;

{ ---------------------------------------------------------------------
    Unit initialization
  ---------------------------------------------------------------------}

begin
  dblch := [becomes, notequal, lessorequal, greaterorequal, opencomment];
end.
