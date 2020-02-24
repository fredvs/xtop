{$mode objfpc}
{$H+}

program jtop;

{
    This file is based on ptop an Pascal pretty print program.

}

uses
  SysUtils,
  Classes,
  jtopu,
  CustApp,
  BufStream;

resourcestring
  Version   = 'Version 1.0';
  ATitle    = 'JToP';
  Copyright = 'Copyright (c) 1999-2020 by the Free Pascal Development Team and FredvS';
  SErrNoInputOutput = 'No input and output file given';

type
  Tjtop = class(TCustomApplication)
  private
    Infilename, OutFileName, ConfigFile: string;
    BeVerbose: Boolean;
    TheIndent, TheBufSize, TheLineSize: integer;
    procedure Usage(ECode: word);
    procedure GenOpts;
    procedure ProcessOpts;
    procedure DoVerbose(Sender: TObject; const Msg: string);
  public
    procedure DoRun;
      override;
  end;

  procedure Tjtop.DoVerbose(Sender: TObject; const Msg: string);
  begin
    writeLn(StdErr, Msg);
  end;

  procedure Tjtop.Usage(ECode: word);
  begin
    writeLn('jtop : Usage : ');
    writeLn('jtop [-v] [-i indent] [-b bufsize ][-c optsfile][-l linesize] infile outfile');
    writeLn('     converts infile to outfile.');
    writeLn('     -c : read options from optsfile');
    writeLn('     -i : Set number of indent spaces.');
    writeLn('     -l : Set maximum output linesize.');
    writeLn('     -b : Use buffers of size bufsize');
    writeLn('     -v : be verbose');
    writeLn('jtop -g ofile');
    writeLn('     generate default options file');
    writeLn('jtop -h : This help');
    ExitCode := ECode;
    Terminate;
  end;

  procedure Tjtop.Genopts;
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(ConfigFile, fmCreate);
    try
      GeneratecfgFile(S);
    finally
      S.Free;
    end;
  end;

  procedure Tjtop.ProcessOpts;
  var
    S: string;
  begin
    { Set defaults }
    Infilename  := '';
    OutFileName := '';
    ConfigFile  := '';
    TheIndent   := 2;
    TheBufSize  := 255;
    TheLineSize := DefLineSize;
    BeVerbose   := False;
    S           := CheckOptions('icglbhv', '');
    if (S <> '') then
    begin
      writeLn(stderr, S);
      Usage(1);
    end;
    if HasOption('h') then
      usage(0);
    TheIndent   := StrToIntDef(GetOptionValue('i', ''), 2);
    TheBufSize  := StrToIntDef(GetOptionValue('b', ''), 255);
    TheLineSize := StrToIntDef(GetOptionValue('l', ''), DefLineSize);
    if HasOption('g') then
    begin
      ConfigFile := GetOptionValue('g', '');
      GenOpts;
      // halt(0);
      ExitCode := 0; // this change instead of halt(0);
      Terminate;    // this change instead of halt(0);
    end;
    ConfigFile := GetOptionValue('c', '');
    BeVerbose  := HasOption('v');
    if (ParamCount > 1) then
    begin
      InFileName  := ParamStr(ParamCount - 1);
      OutFilename := ParamStr(ParamCount);
    end;
  end; { Of ProcessOpts }

  procedure Tjtop.DoRun;
  var
    F, InS, OutS, cfgS: TSTream;
    PPrinter: TPrettyPrinter;
  begin
    ProcessOpts;
    if BeVerbose then
    begin
      writeLn(Title + ' ' + Version);
      writeLn(Copyright);
      writeLn;
    end;

    if (ParamCount > 1) then
    begin
      if (Length(InfileName) = 0) or (Length(OutFileName) = 0) then
      begin
        writeLn(stderr, SErrNoInputOutput);
        Usage(1);
      end;
      Ins := TMemoryStream.Create;
      try
        F := TFileStream.Create(InFileName, fmOpenRead);
        try
          Ins.CopyFrom(F, 0);
          Ins.Position := 0;
        finally
          F.Free;
        end;
        OutS := TwriteBufStream.Create(TFileStream.Create(OutFileName, fmCreate));
        try
          if ConfigFile <> '' then
            CfgS     := TFileStream.Create(ConfigFile, fmOpenRead)
          else
            CfgS     := nil;
          try
            PPrinter := TPrettyPrinter.Create;
            try
              PPrinter.Indent := TheIndent;
              PPrinter.LineSize := TheLineSize;
              PPrinter.Source := Ins;
              PPrinter.Dest   := OutS;
              PPrinter.Config := CfgS;
              if BeVerbose then
                PPrinter.OnVerbose := @DoVerbose;
              PPrinter.PrettyPrint;
            finally
              FreeAndNil(PPrinter);
            end;
          finally
            FreeAndNil(CfgS);
          end;
        finally
          FreeAndNil(OutS);
        end;
      finally
        FreeAndNil(Ins);
      end;
      Terminate;
    end
    else
      Usage(1);

  end;

begin
  with Tjtop.Create(nil) do
    try
      Title           := ATitle;
      StopOnException := True;
      Initialize;
      Run;
    finally
      Free;
    end;
end.

