unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls
  , OpenXR
  , OpenXRProgram
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProgram:TOpenXRProgram;
  public
    procedure CreateProgram;
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if assigned(xr) then begin
   // at this point xr is empty
   // we should call LoadOpenXRLibrary, this is done in TOpenXRProgram
   application.ProcessMessages;
   CreateProgram;
  end;
end;

procedure TForm1.CreateProgram;
begin
  FProgram := TOpenXRProgram.create('OpenXR test',Memo1.lines);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  if assigned(FProgram) then begin
    FProgram.Free;
  end;
end;

end.
