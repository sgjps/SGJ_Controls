{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SGJ_Controls;

{$warn 5023 off : no warning about unused units}
interface

uses
  SGJ.LinkLabel, SGJ.Button, SGJ.SimpleButton, SGJ.CheckBox, SGJ.Panel, 
  SGJ.ShellTreeView, SGJ.ListView, SGJListViewPropEdit, SGJ.Win32PopupMenu, 
  SGJ.Win32Extension, SGJ.TitleBarCtrls, SGJ.CalendarView, SGJ.ExpandPanel, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SGJ.LinkLabel', @SGJ.LinkLabel.Register);
  RegisterUnit('SGJ.Button', @SGJ.Button.Register);
  RegisterUnit('SGJ.SimpleButton', @SGJ.SimpleButton.Register);
  RegisterUnit('SGJ.CheckBox', @SGJ.CheckBox.Register);
  RegisterUnit('SGJ.Panel', @SGJ.Panel.Register);
  RegisterUnit('SGJ.ShellTreeView', @SGJ.ShellTreeView.Register);
  RegisterUnit('SGJ.ListView', @SGJ.ListView.Register);
  RegisterUnit('SGJ.Win32PopupMenu', @SGJ.Win32PopupMenu.Register);
  RegisterUnit('SGJ.Win32Extension', @SGJ.Win32Extension.Register);
  RegisterUnit('SGJ.TitleBarCtrls', @SGJ.TitleBarCtrls.Register);
  RegisterUnit('SGJ.CalendarView', @SGJ.CalendarView.Register);
  RegisterUnit('SGJ.ExpandPanel', @SGJ.ExpandPanel.Register);
end;

initialization
  RegisterPackage('SGJ_Controls', @Register);
end.
