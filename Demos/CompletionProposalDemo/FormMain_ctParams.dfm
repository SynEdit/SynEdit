object Form1: TForm1
  Left = 256
  Top = 122
  Width = 754
  Height = 541
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SynTest: TSynEdit
    Left = 416
    Top = 8
    Width = 321
    Height = 497
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Lines.WideStrings = 
      'This is a Demo to show you how the '#13#10'Code Completion component w' +
      'orks '#13#10'when the default kind is ctParams.'#13#10#13#10'Everything really d' +
      'epends on the'#13#10'code you put in the execute event.'#13#10'This determin' +
      'es what the parameters'#13#10'are and what index should be'#13#10'highlighte' +
      'd.'#13#10#13#10'See the source file execute event'#13#10'for more information.'#13#10
    RemovedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 16496
      end>
  end
  object SynEdit1: TSynEdit
    Left = 6
    Top = 96
    Width = 401
    Height = 401
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 1
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Lines.WideStrings = 
      'Use Shift+Ctrl+Space to activate Parameter'#13#10'completion, or type ' +
      'the function name'#13#10'and the '#39'('#39' (open paren) to start it with'#13#10'th' +
      'e timer.'#13#10#13#10'The valid functions for this example are'#13#10#13#10'TestFunc' +
      'tion'#13#10'Min'#13#10'Max'#13#10#13#10'Below is an example using paren Counting:'#13#10'Max' +
      '(a + b(1 + 2), (3 + 4) * c)'#13#10#13#10'Here is an example of embeded fun' +
      'ctions'#13#10#13#10'TestFunction(Min(a, b), SomeVar, Another)'#13#10
    RemovedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 16496
      end>
  end
  object Button3: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Font'
    TabOrder = 2
    OnClick = Button3Click
  end
  object scpParams: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer]
    ClBackground = clInfoBk
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    OnExecute = scpParamsExecute
    ShortCut = 24608
    Editor = SynEdit1
    TimerInterval = 1200
    Left = 8
    EndOfTokenChrW = '()[]. '
    TriggerCharsW = '('
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 44
  end
end
