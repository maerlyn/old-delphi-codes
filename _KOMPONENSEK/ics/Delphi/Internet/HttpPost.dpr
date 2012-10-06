program HttpPost;

uses
  Forms,
  HttpPost1 in 'HttpPost1.pas' {HttpPostForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpPostForm, HttpPostForm);
  Application.Run;
end.
