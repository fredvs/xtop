{$mode objfpc}
{$H+}

Program PtoP;





{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by Michael Van Canneyt, member of
    the Free Pascal development team

    Pascal pretty print program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


uses 
SysUtils,
Classes,
PtoPu,
CustApp,
BufStream;

resourcestring
Version = 'Version 1.2';
ATitle = 'PToP';
Copyright = 'Copyright (c) 1999-2005 by the Free Pascal Development Team';
SErrNoInputOutput = 'No input and output file given';

Type 
  TPToP = Class(TCustomApplication)
    Private 
      Infilename,
    OutFileName,
    ConfigFile: string;
    BeVerbose: boolean;
    TheIndent,
    TheBufSize,
    TheLineSize: integer;
    Procedure Usage(ECode: word);
    Procedure GenOpts;
    Procedure ProcessOpts;
    Procedure DoVerbose(Sender: TObject; Const Msg: String);
    Public 
      Procedure DoRun;
      override;
  End;


Procedure TPToP.DoVerbose(Sender: TObject; Const Msg: String);

begin
  writeLn(StdErr,Msg);
End;

Procedure TPToP.Usage(ECode: word);

begin
  writeLn('ptop : Usage : ');
  writeLn('ptop [-v] [-i indent] [-b bufsize ][-c optsfile][-l linesize] infile outfile');
  writeLn('     converts infile to outfile.');
  writeLn('     -c : read options from optsfile');
  writeLn('     -i : Set number of indent spaces.');
  writeLn('     -l : Set maximum output linesize.');
  writeLn('     -b : Use buffers of size bufsize');
  writeLn('     -v : be verbose');
  writeLn('ptop -g ofile');
  writeLn('     generate default options file');
  writeLn('ptop -h : This help');
  halt(Ecode);
End;

Procedure TPToP.Genopts;

Var 
  S: TFileStream;

begin
  S := TFileStream.Create(ConfigFile, fmCreate);
  Try
    GeneratecfgFile(S);
  Finally
    S.Free;
End;
End;

Procedure TPToP.ProcessOpts;

Var 
  S: string;
begin
  { Set defaults }
  Infilename := '';
  OutFileName := '';
  ConfigFile := '';
  TheIndent := 2;
  TheBufSize := 255;
  TheLineSize := DefLineSize;
  BeVerbose := false;
  S := CheckOptions('icglbhv', '');
  If (S <> '') Then
    begin
      writeLn(stderr, S);
      Usage(1);
    End;
  If HasOption('h') Then usage(0);
  TheIndent := StrToIntDef(GetOptionValue('i', ''), 2);
  TheBufSize := StrToIntDef(GetOptionValue('b', ''), 255);
  TheLineSize := StrToIntDef(GetOptionValue('l', ''), DefLineSize);
  If HasOption('g') Then
    begin
      ConfigFile := GetOptionValue('g', '');
      GenOpts;
      halt(0);
    End;
  ConfigFile := GetOptionValue('c', '');
  BeVerbose := HasOption('v');
  If (ParamCount > 1) Then
    begin
      InFileName := paramstr(ParamCount - 1);
      OutFilename := Paramstr(ParamCount);
    End;
End; { Of ProcessOpts }

Procedure TPToP.DoRun;

Var 
  F,
  InS,
  OutS,
  cfgS: TSTream;
  PPrinter: TPrettyPrinter;

begin
  ProcessOpts;
  If BeVerbose Then
    begin
      writeLn(Title + ' ' + Version);
      writeLn(Copyright);
      writeLn;
    End;
  If (Length(InfileName) = 0) Or (Length(OutFileName) = 0) Then
    begin
      writeLn(stderr, SErrNoInputOutput);
      Usage(1);
    End;
  Ins := TMemoryStream.Create;
  Try
    F := TFileStream.Create(InFileName, fmOpenRead);
    Try
      Ins.CopyFrom(F, 0);
      Ins.Position := 0;
    Finally
      F.Free;
End;
OutS := TwriteBufStream.Create(TFileStream.Create(OutFileName, fmCreate));
Try
  If ConfigFile <> '' Then CfgS := TFileStream.Create(ConfigFile, fmOpenRead)
  Else CfgS := Nil;
  Try
    PPrinter := TPrettyPrinter.Create;
    Try
      PPrinter.Indent := TheIndent;
      PPrinter.LineSize := TheLineSize;
      PPrinter.Source := Ins;
      PPrinter.Dest := OutS;
      PPrinter.Config := CfgS;
      If BeVerbose Then PPrinter.OnVerbose := @DoVerbose;
      PPrinter.PrettyPrint;
    Finally
      FreeAndNil(PPrinter);
End;
Finally
  FreeAndNil(CfgS);
End;
Finally
  FreeAndNil(OutS);
End;
Finally
  FreeAndNil(Ins);
End;
Terminate;
End;

begin
  With TPToP.Create(Nil) Do
    Try
      Title := ATitle;
      StopOnException := true;
      Initialize;
      Run;
    Finally
      Free;
End;
End.
