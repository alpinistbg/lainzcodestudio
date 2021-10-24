program lainzcs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, uhighdpi, lcs_debugform, lcs_dialog_input, ulocals, uwatches;

{$R *.res}

begin
  Application.Title:='Lainz Code Studio 0.2';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.CreateForm(Tfrm_lcs_dialog_input, frm_lcs_dialog_input);
  Application.CreateForm(TfrmLocals, frmLocals);
  Application.CreateForm(TfrmWatches, frmWatches);
  Application.Run;
end.

