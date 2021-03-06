�
 TFORM1 0�  TPF0TForm1Form1Left� ToplWidth)Height#Caption0ExTable12 - Saving and restoring column settings
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OnCreate
FormCreatePixelsPerInch`
TextHeight 	TOvcTable	OvcTable1LeftTopWidth� Height� 
LockedRowsTopRow	ActiveRowRowLimit

LockedColsLeftCol	ActiveColAccess	otxNormalAdjustotaCenterLeftBorderStylebsSingleColorUnusedclWindowColors.ActiveFocusedclHighlightColors.ActiveFocusedTextclHighlightTextColors.ActiveUnfocusedclHighlightColors.ActiveUnfocusedTextclHighlightTextColors.Locked	clBtnFaceColors.LockedTextclWindowTextColors.Editing	clBtnFaceColors.EditingTextclWindowTextColors.SelectedclHighlightColors.SelectedTextclHighlightText
ControllerOvcController1!GridPenSet.NormalGrid.NormalColorclBtnShadow!GridPenSet.NormalGrid.SecondColorclBtnHighlightGridPenSet.NormalGrid.StylepsDotGridPenSet.NormalGrid.EffectgeBoth!GridPenSet.LockedGrid.NormalColorclBtnShadow!GridPenSet.LockedGrid.SecondColorclBtnHighlightGridPenSet.LockedGrid.StylepsSolidGridPenSet.LockedGrid.Effectge3D&GridPenSet.CellWhenFocused.NormalColorclBlack&GridPenSet.CellWhenFocused.SecondColorclBtnHighlight GridPenSet.CellWhenFocused.StylepsSolid!GridPenSet.CellWhenFocused.EffectgeBoth(GridPenSet.CellWhenUnfocused.NormalColorclBlack(GridPenSet.CellWhenUnfocused.SecondColorclBtnHighlight"GridPenSet.CellWhenUnfocused.StylepsDash#GridPenSet.CellWhenUnfocused.EffectgeBothOptionsotoNoRowResizingotoNoColResizingotoNoSelectionotoAllowColMoves ParentColor
ScrollBarsssBothTabOrder TabStop	OnColumnsChangedOvcTable1ColumnsChangedOnExitOvcTable1ExitOnGetCellDataOvcTable1GetCellDataCellData RowData ColData, S2   TButtonButton1Left@Top� WidthYHeight!Caption&SaveTabOrderOnClickButton1Click  TMemoMemo1Left� TopWidthHeight� TabStopLines.Strings5The primary purpose of this example is to show how to9store column settings, i.e., the default cell, width, and;original design position, in an INI file so that the values5can be read back at startup and the table restored tothe same settings. 4This is a *very basic* example in that it depends on3a fixed number of columns, does not store the data,5nor allow for values to be missing from the INI file,6i.e., the default cell is not assigned in these cases.+These and other details are left up to you. ReadOnly	
ScrollBars
ssVerticalTabOrderWordWrap  TOvcControllerOvcController1EntryCommands.TableListDefault	 WordStar Grid  EntryOptionsefoAutoSelectefoBeepOnErrorefoInsertPushes EpochlLeft� TopB  TOvcTCCheckBoxCB1AcceptActivationClick	Access
otxDefaultAdjust
otaDefaultAllowGrayedCellGlyphs.IsDefault	CellGlyphs.GlyphCountCellGlyphs.ActiveGlyphCountMarginShowHintTable	OvcTable1
TableColor	OnClickCB1ClickLeft;Top
  TOvcTCStringSF1Access
otxDefaultAdjust
otaDefaultAutoAdvanceCharAutoAdvanceLeftRightMargin	MaxLength
ShowHintTable	OvcTable1
TableColor		TableFont	TextHiColorclBtnHighlight	TextStyletsFlatUseWordWrapUseASCIIZStrings	LeftjTop
  TOvcTCPictureFieldPF1DataTypepftTimePictureMaskhh:mm	MaxLengthAccess
otxDefaultAdjust
otaDefaultCaretIns.BitmapHotSpotX CaretIns.BitmapHotSpotY CaretIns.Shape
csVertLineCaretIns.AligncaLeftCaretIns.BlinkTime CaretIns.CaretHeight
CaretIns.CaretWidthCaretIns.IsGrayCaretOvr.BitmapHotSpotX CaretOvr.BitmapHotSpotY CaretOvr.ShapecsBlockCaretOvr.AligncaLeftCaretOvr.BlinkTime CaretOvr.CaretHeight
CaretOvr.CaretWidthCaretOvr.IsGrayControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextEpoch MarginOptionsefoCaretToEnd PadChar PasswordChar*ShowHintTable	OvcTable1
TableColor		TableFont	TextHiColorclBtnHighlight
TextMargin	TextStyletsFlatLeft� Top
	RangeHigh

   Q       RangeLow

                