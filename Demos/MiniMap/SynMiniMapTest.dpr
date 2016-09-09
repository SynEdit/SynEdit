program SynMiniMapTest;

{$SetPEFlags $0001}
{$WEAKLINKRTTI ON}

uses
  Forms,
  SynMiniMap in 'SynMiniMap.pas' {FormSynEditMinimap};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSynEditMinimap, FormSynEditMinimap);
  Application.Run;
end.
