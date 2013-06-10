object Form1: TForm1
  Left = 206
  Top = 196
  Width = 696
  Height = 491
  ActiveControl = SynEdit1
  Caption = 'URL Demo'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 688
    Height = 457
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clBlack
    Gutter.Font.Height = 8
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Pitch = fpVariable
    Gutter.Font.Style = []
    Highlighter = SynURISyn1
    Lines.WideStrings = 
      'This project demonstrates how to use SynURIOpener to make links ' +
      'clickable.'#13#10#13#10'Under Windows (also CLX when running Windows) this' +
      ' is very easy.'#13#10'For a Linux demo open URLDemoLinux.dpr.'#13#10#13#10'Simpl' +
      'y assign two properties of SynURIOpener:'#13#10'- Editor-property must' +
      ' be set to your SynEdit/SynMemo.'#13#10'- URIHighlighter-property shou' +
      'ld be assigned an instance of a SynURISyn.'#13#10#13#10'Don'#39't forget to se' +
      't also SynEdit'#39's Highlighter-property to a SynURISyn.'#13#10'That'#39's al' +
      'l!'#13#10#13#10'NOTE:'#13#10'By default you have to press CTRL to make the links' +
      ' clickable.'#13#10'If you don'#39't like that, set CtrlActivatesLinks-prop' +
      'erty to false.'#13#10#13#10#13#10'Some test links:'#13#10'----------------'#13#10'(not all' +
      ' real ones, just to check if correct app starts)'#13#10#13#10'http://www.s' +
      'omewhere.org'#13#10'ftp://superhost.org/downloads/gems.zip'#13#10'www.w3c.or' +
      'g'#13#10'mailto:big@lebowski.edu'#13#10'douglas@adams.lod'#13#10'news:comp.lang.pa' +
      'scal.borland'
  end
  object SynURIOpener1: TSynURIOpener
    Editor = SynEdit1
    URIHighlighter = SynURISyn1
    Left = 632
    Top = 40
  end
  object SynURISyn1: TSynURISyn
    Left = 632
    Top = 8
  end
end
