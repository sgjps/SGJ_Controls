{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sgj_ctrls;

{$warn 5023 off : no warning about unused units}
interface

uses
  SGJLinkLabel, sgjpanel, SGJToogleButton, SGJButton, WinApi.DirectDraw, 
  WinApi.GDIPAPI, WinApi.GDIPOBJ, WinApi.GDIPUTIL, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SGJLinkLabel', @SGJLinkLabel.Register);
  RegisterUnit('sgjpanel', @sgjpanel.Register);
  RegisterUnit('SGJToogleButton', @SGJToogleButton.Register);
  RegisterUnit('SGJButton', @SGJButton.Register);
end;

initialization
  RegisterPackage('sgj_ctrls', @Register);
end.
