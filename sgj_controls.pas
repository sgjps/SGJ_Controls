{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SGJ_Controls;

{$warn 5023 off : no warning about unused units}
interface

uses
  SGJ.Button, SGJ.LinkLabel, SGJ.Panel, SGJ.ToogleButton, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SGJ.Button', @SGJ.Button.Register);
  RegisterUnit('SGJ.LinkLabel', @SGJ.LinkLabel.Register);
  RegisterUnit('SGJ.Panel', @SGJ.Panel.Register);
  RegisterUnit('SGJ.ToogleButton', @SGJ.ToogleButton.Register);
end;

initialization
  RegisterPackage('SGJ_Controls', @Register);
end.
