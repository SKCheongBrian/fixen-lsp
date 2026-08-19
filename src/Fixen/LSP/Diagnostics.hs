module Fixen.LSP.Diagnostics 
( sendDocumentDiagnostics
, toLspDiagnostic
, toLspRange
) where

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

import Fixen.Diagnostics qualified as Fixen
import Fixen.LSP.Types (Config)

toLspPosition :: Int -> Int -> Position
toLspPosition line column =
  Position
    (fromIntegral (max 0 (line - 1)))
    (fromIntegral (max 0 (column - 1)))

toLspRange :: Maybe Fixen.FixenSourceSpan -> Range
toLspRange maybeSpan =
  case maybeSpan of
    Nothing ->
      Range
        (Position 0 0)
        (Position 0 0)

    Just sourceSpan ->
      Range
        ( toLspPosition
            (Fixen.fixenStartLine sourceSpan)
            (Fixen.fixenStartColumn sourceSpan)
        )
        ( toLspPosition
            (Fixen.fixenEndLine sourceSpan)
            (Fixen.fixenEndColumn sourceSpan)
        )

toLspSeverity
  :: Fixen.FixenDiagnosticSeverity
  -> DiagnosticSeverity
toLspSeverity Fixen.FixenDiagnosticError =
  DiagnosticSeverity_Error
toLspSeverity Fixen.FixenDiagnosticWarning =
  DiagnosticSeverity_Warning

toLspDiagnostic :: Fixen.FixenDiagnostic -> Diagnostic
toLspDiagnostic diagnostic =
  Diagnostic
    { _range =
        toLspRange (Fixen.fixenDiagnosticSpan diagnostic)
    , _severity =
        Just
          ( toLspSeverity
              (Fixen.fixenDiagnosticSeverity diagnostic)
          )
    , _code = Nothing
    , _codeDescription = Nothing
    , _source = Just "fixen"
    , _message = Fixen.fixenDiagnosticMessage diagnostic
    , _tags = Nothing
    , _relatedInformation = Nothing
    , _data_ = Nothing
    }

sendDocumentDiagnostics
  :: Uri
  -> [Diagnostic]
  -> LspM Config ()
sendDocumentDiagnostics uri diagnostics =
  sendNotification
    SMethod_TextDocumentPublishDiagnostics
    (PublishDiagnosticsParams uri Nothing diagnostics)
