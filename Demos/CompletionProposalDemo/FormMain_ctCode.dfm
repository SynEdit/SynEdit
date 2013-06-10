object Form1: TForm1
  Left = 212
  Top = 100
  Width = 776
  Height = 586
  Caption = 'SynEdit Code Completion Demo for ctCode'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 753
    Height = 537
    ActivePage = CodeCompletion
    TabOrder = 0
    object CodeCompletion: TTabSheet
      Caption = 'CodeCompletion'
      object Label3: TLabel
        Left = 8
        Top = 19
        Width = 64
        Height = 13
        Caption = 'BiggestWord:'
      end
      object Label4: TLabel
        Left = 224
        Top = 19
        Width = 23
        Height = 13
        Caption = 'Title:'
      end
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
        TabOrder = 9
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Terminal'
        Gutter.Font.Style = []
        Gutter.Visible = False
        Lines.WideStrings = 
          'This is a Demo to show you how the'#13#10'Code Completion component wo' +
          'rks'#13#10'when the default kind is ctCode.'#13#10#13#10'BiggestWord: If you are' +
          ' using'#13#10'  PrettyText then this is the biggest'#13#10'  word that will ' +
          'show up before the'#13#10'  *bold* words'#13#10#13#10'CaseSensitive: makes the t' +
          'ext you'#13#10'  type and the matching in the'#13#10'  dropdown list case se' +
          'nsitive'#13#10#13#10'AnsiStrings  : Use Ansi string'#13#10'  comparisons instead' +
          ' of default'#13#10'  string comparisons'#13#10#13#10'UsePrettyText: Allows you t' +
          'o format'#13#10'  the text displayed in the dropdown.'#13#10'  Please refer ' +
          'to the tsyncompletion-'#13#10'  proposal.html file for a description'#13#10 +
          '  of the available commands.'#13#10#13#10'UseInsertList: Lets you display ' +
          'one'#13#10'  thing in the dropdown and insert'#13#10'  another thing when th' +
          'ey choose an'#13#10'  item.  Like in Delphi, the'#13#10'  dropdown might dis' +
          'play'#13#10'  "procedure foo(AVariable: Integer)"'#13#10'  and only insert f' +
          'oo when you'#13#10'  select it.  The InsertList must'#13#10'  have as many i' +
          'tems as the ItemList'#13#10'  or you will get a list index out'#13#10'  of b' +
          'ounds error when you select'#13#10'  an item outside of the range.'#13#10#13#10 +
          'LimitToMatchedText: Limits the'#13#10'  dropdown to the items matching' +
          ' the'#13#10'  text you have typed, similar to the'#13#10'  way the D6 does i' +
          't.'#13#10
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
      object edBiggestWord: TEdit
        Left = 80
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'constructor'
        OnChange = edBiggestWordChange
      end
      object cbCase: TCheckBox
        Tag = 1
        Left = 8
        Top = 40
        Width = 129
        Height = 17
        Caption = 'Case Sensitive'
        TabOrder = 2
        OnClick = CheckBoxClick
      end
      object cbPrettyText: TCheckBox
        Tag = 3
        Left = 8
        Top = 64
        Width = 129
        Height = 17
        Caption = 'Use Pretty Text'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CheckBoxClick
      end
      object cbUseInsertList: TCheckBox
        Tag = 4
        Left = 8
        Top = 88
        Width = 129
        Height = 17
        Caption = 'Use Insert List'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = CheckBoxClick
      end
      object cbLimitToMatchedText: TCheckBox
        Tag = 5
        Left = 8
        Top = 112
        Width = 129
        Height = 17
        Caption = 'Limit To Matched Text'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckBoxClick
      end
      object SynEdit1: TSynEdit
        Left = 5
        Top = 160
        Width = 401
        Height = 344
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 8
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Terminal'
        Gutter.Font.Style = []
        Lines.WideStrings = 
          'Use Ctrl+Space to activate Code Completion'#13#10'with a shortcut, or ' +
          'use the '#39'.'#39' key'#13#10'to activate it with a timer'#13#10#13#10#13#10
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
      object edTitle: TEdit
        Left = 256
        Top = 16
        Width = 145
        Height = 21
        TabOrder = 1
        Text = 'Completion Proposal Demo'
        OnChange = edTitleChange
      end
      object Button3: TButton
        Left = 240
        Top = 112
        Width = 75
        Height = 25
        Caption = 'Font'
        TabOrder = 6
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 328
        Top = 112
        Width = 75
        Height = 25
        Caption = 'Title Font'
        TabOrder = 7
        OnClick = Button4Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Insert and Item Lists'
      ImageIndex = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Insert List'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label2: TLabel
        Left = 8
        Top = 240
        Width = 45
        Height = 13
        Caption = 'ItemList'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object mmoInsert: TMemo
        Left = 8
        Top = 24
        Width = 729
        Height = 169
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'mmoInsert')
        ParentFont = False
        TabOrder = 0
      end
      object mmoItem: TMemo
        Left = 8
        Top = 256
        Width = 729
        Height = 209
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'mmoItem')
        ParentFont = False
        TabOrder = 1
        WantTabs = True
      end
      object Button1: TButton
        Left = 8
        Top = 200
        Width = 137
        Height = 25
        Caption = 'Update Insert List'
        TabOrder = 2
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 8
        Top = 472
        Width = 137
        Height = 25
        Caption = 'Update Item List'
        TabOrder = 3
        OnClick = Button2Click
      end
    end
  end
  object scpDemo: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Title = 'Completion Proposal Demo'
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
    Columns = <
      item
        BiggestWord = 'constructor'
        BiggestWordW = 'constructor'
      end>
    ShortCut = 16416
    Editor = SynEdit1
    Left = 216
    EndOfTokenChrW = '()[]. '
    TriggerCharsW = '.'
    TitleW = 'Completion Proposal Demo'
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 348
    Top = 80
  end
end
